{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.Main
  ( CliError(..)
  , HasCliError(..)
  , AsCliError(..)
  , cli
  , cliIO
  ) where

import Control.Applicative.Lift (Errors, failure, runErrors)
import Control.Lens (Getting, Prism', (^.), (^..), _1, _2, _Wrapped, anyOf, aside, each, filtered, folded, makeClassy, makeClassyPrisms, has, only, to)
import Control.Monad (unless)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Function (on)
import Data.List (find, groupBy, sort)
import Data.Text.Encoding (encodeUtf8)
import Data.Semigroup (Endo, (<>))
import Data.Yaml (decodeEither)
import Network.AWS (Credentials(Discover), Env, newEnv)
import System.Exit (exitFailure)

import Prelude hiding (readFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Template
import Network.CloudSeeder.Types

--------------------------------------------------------------------------------
-- IO wiring

data CliError
  = CliMissingEnvVars [T.Text]
  | CliFileSystemError FileSystemError
  | CliStackNotConfigured T.Text
  | CliMissingDependencyStacks [T.Text]
  | CliTemplateDecodeFail String
  | CliMissingRequiredParameters (S.Set T.Text)
  | CliDuplicateParameterValues (M.Map T.Text [T.Text])
  | CliDuplicateTagValues (M.Map T.Text [T.Text])
  | CliExtraParameterFlags (S.Set T.Text)
  deriving (Eq, Show)

makeClassy ''CliError
makeClassyPrisms ''CliError

renderCliError :: CliError -> T.Text
renderCliError (CliMissingEnvVars vars)
  =  "the following required environment variables were not set:\n"
  <> T.unlines (map ("  " <>) vars)
renderCliError (CliFileSystemError (FileNotFound path))
  = "file not found: ‘" <> path <> "’\n"
renderCliError (CliStackNotConfigured stackName)
  = "stack name not present in configuration: ‘" <> stackName <> "’\n"
renderCliError (CliMissingDependencyStacks stackNames)
  =  "the following dependency stacks do not exist in AWS:\n"
  <> T.unlines (map ("  " <>) stackNames)
renderCliError (CliTemplateDecodeFail decodeFailure)
  = "template YAML decoding failed: " <> T.pack decodeFailure
renderCliError (CliMissingRequiredParameters params)
  = "the following required parameters were not supplied:\n"
  <> T.unlines (map (" " <>) (S.toAscList params))
renderCliError (CliDuplicateParameterValues params)
  = "the following parameters were supplied more than one value:\n"
  <> renderKeysToManyVals params
renderCliError (CliDuplicateTagValues ts)
  = "the following tags were supplied more than one value:\n"
  <> renderKeysToManyVals ts
renderCliError (CliExtraParameterFlags ts)
  = "parameter flags defined in config that were not present in template:\n"
  <> T.unlines (map (" " <>) (S.toAscList ts))

renderKeysToManyVals :: M.Map T.Text [T.Text] -> T.Text
renderKeysToManyVals xs = T.unlines $ map renderKeyToManyVals (M.toAscList xs)
  where renderKeyToManyVals (k, vs) = k <> ": " <> T.intercalate ", " vs

newtype AppM a = AppM (ReaderT Env (ExceptT CliError (LoggingT IO)) a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO
           , MonadCatch, MonadThrow, MonadReader Env, MonadError CliError
           , MonadLogger, MonadEnvironment )

instance MonadBaseControl IO AppM where
  type StM AppM a = StM (ReaderT Env (ExceptT CliError (LoggingT IO))) a
  liftBaseWith f = AppM (liftBaseWith (\g -> f (\(AppM x) -> g x)))
  restoreM = AppM . restoreM

instance MonadFileSystem CliError AppM where
  readFile = readFile'

instance MonadCLI AppM where
  getArgs = getArgs'
  getOptions = getOptions'

instance MonadCloud AppM where
  computeChangeset = computeChangeset'
  getStackOutputs = getStackOutputs'
  runChangeSet = runChangeSet'

runAppM :: AppM a -> IO a
runAppM (AppM x) = do
  env <- newEnv Discover
  result <- runStderrLoggingT . runExceptT $ runReaderT x env
  either (\err -> T.putStr (renderCliError err) >> exitFailure) return result

--------------------------------------------------------------------------------
-- Logic

instance AsFileSystemError CliError where
  _FileSystemError = _CliFileSystemError

cli :: (MonadCLI m, MonadCloud m, MonadFileSystem CliError m, MonadEnvironment m) => m DeploymentConfiguration -> m ()
cli mConfig = do
  config <- mConfig
  (ProvisionStack nameToProvision env) <- getArgs

  let dependencies = takeWhile (/= nameToProvision) (config ^.. stacks.each.name)
      appName = config ^. name

  stackToProvision <- getStackToProvision config nameToProvision

  templateBody <- readFile $ nameToProvision <> ".yaml"
  template <- decodeTemplate templateBody

  let paramSources = (config ^. parameterSources) <> (stackToProvision ^. parameterSources)
      paramSpecs = template ^. parameterSpecs._Wrapped
  allParams <- getParameters paramSources paramSpecs dependencies env appName
  allTags <- getTags config stackToProvision env appName

  let fullStackName = mkFullStackName env appName nameToProvision
  csId <- computeChangeset fullStackName templateBody allParams allTags
  runChangeSet csId

getStackToProvision :: (AsCliError e, MonadError e m) => DeploymentConfiguration -> T.Text -> m StackConfiguration
getStackToProvision config nameToProvision = do
  let maybeStackToProvision = config ^. stacks.to (find (has (name.only nameToProvision)))
  maybe (throwing _CliStackNotConfigured nameToProvision) return maybeStackToProvision

decodeTemplate :: (AsCliError e, MonadError e m) => T.Text -> m Template
decodeTemplate templateBody = do
  let decodeOrFailure = decodeEither (encodeUtf8 templateBody) :: Either String Template
  either (throwing _CliTemplateDecodeFail) return decodeOrFailure

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName

getTags :: (MonadError e m, AsCliError e) => DeploymentConfiguration -> StackConfiguration -> T.Text -> T.Text -> m (M.Map T.Text T.Text)
getTags config stackToProvision env appName =
   assertUnique _CliDuplicateTagValues (baseTags <> globalTags <> localTags)
  where
    baseTags :: S.Set (T.Text, T.Text)
    baseTags = [("cj:environment", env), ("cj:application", appName)]
    globalTags = config ^. tagSet
    localTags = stackToProvision ^. tagSet

-- | Fetches parameter values for all param sources, handling potential errors
-- and misconfigurations.
getParameters
  :: forall e m. (AsCliError e, MonadError e m, MonadCLI m, MonadEnvironment m, MonadCloud m)
  => S.Set (T.Text, ParameterSource) -- ^ parameter sources to fetch values for
  -> S.Set ParameterSpec -- ^ parameter specs from the template currently being deployed
  -> [T.Text] -- ^ names of stack dependencies
  -> T.Text -- ^ name of environment being deployed to
  -> T.Text -- ^ name of application being deployed
  -> m (M.Map T.Text T.Text)
getParameters paramSources paramSpecs dependencies env appName = do
    let constants = paramSources ^..* folded.aside _Constant
    fetchedParams <- S.unions <$> sequence [envVars, flags, outputs]
    let allParams = S.insert ("Env", env) (constants <> fetchedParams)
    validateParameters allParams
  where
    envVars :: m (S.Set (T.Text, T.Text))
    envVars = do
      let requiredEnvVars = paramSources ^.. folded.filtered (has (_2._Env))._1
      maybeEnvValues <- mapM (\envVarKey -> (envVarKey,) <$> getEnv envVarKey) requiredEnvVars
      let envVarsOrFailure = runErrors $ traverse (extractResult (,)) maybeEnvValues
      either (throwing _CliMissingEnvVars . sort) (return . S.fromList) envVarsOrFailure

    flags :: m (S.Set (T.Text, T.Text))
    flags = do
      let paramFlags = paramSources ^..* folded.filtered (has (_2._Flag))._1
          flaggedParamSpecs = paramSpecs ^..* folded.filtered (anyOf parameterKey (`elem` paramFlags))

          paramSpecNames = paramSpecs ^..* folded.parameterKey
          paramFlagsNotInTemplate = paramFlags ^..* folded.filtered (`notElem` paramSpecNames)

      unless (S.null paramFlagsNotInTemplate) $
        throwing _CliExtraParameterFlags paramFlagsNotInTemplate
      S.fromList . M.toList <$> getOptions flaggedParamSpecs

    outputs :: m (S.Set (T.Text, T.Text))
    outputs = do
      maybeOutputs <- mapM (\stackName -> (stackName,) <$> getStackOutputs (mkFullStackName env appName stackName)) dependencies
      let outputsOrFailure = runErrors $ traverse (extractResult (flip const)) maybeOutputs
      either (throwing _CliMissingDependencyStacks) (return . S.fromList . concatMap M.toList) outputsOrFailure

    validateParameters :: S.Set (T.Text, T.Text) -> m (M.Map T.Text T.Text)
    validateParameters params = do
      let requiredParamNames = paramSpecs ^..* folded._Required
          allowedParamNames = paramSpecs ^..* folded.parameterKey

      uniqueParams <- assertUnique _CliDuplicateParameterValues params
      let missingParamNames = requiredParamNames S.\\ M.keysSet uniqueParams
      unless (S.null missingParamNames) $
        throwing _CliMissingRequiredParameters missingParamNames

      return $ uniqueParams `M.intersection` M.fromSet (const ()) allowedParamNames

cliIO :: AppM DeploymentConfiguration -> IO ()
cliIO mConfig = runAppM $ cli mConfig

-- | Given a set of tuples that represent a mapping between keys and values,
-- assert the keys are all unique, and produce a map as a result. If any keys
-- are duplicated, the provided prism will be used to signal an error.
assertUnique
  :: forall k v e m. (Ord k, MonadError e m)
  => Prism' e (M.Map k [v]) -> S.Set (k, v) -> m (M.Map k v)
assertUnique _Err paramSet = case duplicateParams of
    [] -> return $ M.fromList paramList
    _ -> throwing _Err duplicateParams
  where
    paramList = S.toAscList paramSet
    paramsGrouped = tuplesToMap paramList
    duplicateParams = M.filter ((> 1) . length) paramsGrouped

    tuplesToMap :: [(k, v)] -> M.Map k [v]
    tuplesToMap xs = M.fromList $ map concatGroup grouped
      where
        grouped = groupBy ((==) `on` fst) xs
        concatGroup ys = (fst (head ys), map snd ys)

-- | Applies a function to the members of a tuple to produce a result, unless
-- the tuple contains 'Nothing', in which case this logs an error in the
-- 'Errors' applicative using the left side of the tuple as a label.
--
-- >>> runErrors $ extractResult (,) ("foo", Just True)
-- Right ("foo", True)
-- >>> runErrors $ extractResult (,) ("foo", Nothing)
-- Left ["foo"]
extractResult :: (a -> b -> c) -> (a, Maybe b) -> Errors [a] c
extractResult f (k, m) = do
  v <- maybe (failure [k]) pure m
  pure (f k v)

-- | Like '^..', but collects the result into a 'S.Set' instead of a list.
infixl 8 ^..*
(^..*) :: Ord a => s -> Getting (Endo [a]) s a -> S.Set a
x ^..* l = S.fromList (x ^.. l)
