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
import Control.Lens (Prism', _1, _2, _Wrapped, (^.), (^..), each, filtered, folded, makeClassy, makeClassyPrisms, has, only, to)
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
import Data.Semigroup ((<>))
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
  | CliArgumentsError ArgumentsError
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
renderCliError (CliArgumentsError (WrongArity args)) 
  = "wrong number of arguments provided: " <> (T.intercalate ", " args) 
  <> "\n Please supply in the form 'COMMAND STACK ENV'"

renderKeysToManyVals :: M.Map T.Text [T.Text] -> T.Text
renderKeysToManyVals xs = T.unlines $ map renderKeyToManyVals (M.toAscList xs)
  where renderKeyToManyVals (k, vs) = k <> ": " <> (T.intercalate ", " vs)

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

instance MonadCLI CliError AppM where 
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

instance AsArgumentsError CliError where 
  _ArgumentsError = _CliArgumentsError

cli :: (MonadCLI CliError m, MonadCloud m, MonadFileSystem CliError m, MonadEnvironment m) => m DeploymentConfiguration -> m ()
cli mConfig = do
  config <- mConfig
  (DeployStack nameToDeploy env) <- getArgs

  let dependencies = takeWhile (/= nameToDeploy) (config ^.. stacks.each.name)
      appName = config ^. name

  stackToDeploy <- getStackToDeploy config nameToDeploy

  templateBody <- readFile $ nameToDeploy <> ".yaml"
  template <- decodeTemplate templateBody

  allParams <- getParameters config stackToDeploy dependencies env appName
  validParams <- validateParameters template allParams

  let stackTags = getTags config stackToDeploy env appName
  validTags <- validateTags stackTags

  let fullStackName = mkFullStackName env appName nameToDeploy
  csId <- computeChangeset fullStackName templateBody validParams validTags
  runChangeSet csId

getStackToDeploy :: (AsCliError e, MonadError e m) => DeploymentConfiguration -> T.Text -> m StackConfiguration
getStackToDeploy config nameToDeploy = do 
  let maybeStackToDeploy = config ^. stacks.to (find (has (name.only nameToDeploy)))
  maybe (throwing _CliStackNotConfigured nameToDeploy) return maybeStackToDeploy

getEnvVars :: (MonadError CliError m, MonadEnvironment m) => DeploymentConfiguration -> StackConfiguration -> m (S.Set (T.Text, T.Text))
getEnvVars config stackToDeploy = do 
  let requiredGlobalEnvVars = config ^. environmentVariables
      requiredStackEnvVars = stackToDeploy ^. environmentVariables
      requiredEnvVars = requiredGlobalEnvVars ++ requiredStackEnvVars

  maybeEnvValues <- mapM (\envVarKey -> (envVarKey,) <$> getEnv envVarKey) requiredEnvVars
  let envVarsOrFailure = runErrors $ traverse (extractResult (,)) maybeEnvValues
  either (throwError . CliMissingEnvVars . sort) (return . S.fromList) envVarsOrFailure

decodeTemplate :: (AsCliError e, MonadError e m) => T.Text -> m Template
decodeTemplate templateBody = do
  let decodeOrFailure = decodeEither (encodeUtf8 templateBody) :: Either String Template
  either (throwing _CliTemplateDecodeFail) return decodeOrFailure

getOutputs :: (AsCliError e, MonadError e m, MonadCloud m, Traversable t) 
           => t T.Text -> T.Text -> T.Text -> m (S.Set (T.Text, T.Text))
getOutputs dependencies env appName = do 
  maybeOutputs <- mapM (\stackName -> (stackName,) <$> getStackOutputs (mkFullStackName env appName stackName)) dependencies
  let outputsOrFailure = runErrors $ traverse (extractResult (flip const)) maybeOutputs
  either (throwing _CliMissingDependencyStacks) (return . S.fromList . concatMap M.toList) outputsOrFailure

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName

getTags :: DeploymentConfiguration -> StackConfiguration -> T.Text -> T.Text -> S.Set (T.Text, T.Text)
getTags config stackToDeploy env appName = baseTags <> globalTags <> localTags
  where 
    baseTags :: S.Set (T.Text, T.Text)
    baseTags = [("cj:environment", env), ("cj:application", appName)]
    globalTags = config ^. tagSet
    localTags = stackToDeploy ^. tagSet

getPassedParameters :: (MonadCLI e m) => DeploymentConfiguration -> StackConfiguration -> m (S.Set (T.Text, T.Text))
getPassedParameters config stackToDeploy = do
  let globalParamSources = config ^. parameterSources
      localParamSources = stackToDeploy ^. parameterSources
      allParamSources = globalParamSources <> localParamSources
      paramFlags = S.fromList $ allParamSources ^.. folded.filtered (has (_2._Flag))._1
  S.fromList . M.toList <$> getOptions paramFlags

collectParameters :: DeploymentConfiguration -> StackConfiguration -> S.Set (T.Text, T.Text) -> T.Text -> S.Set (T.Text, T.Text) -> S.Set (T.Text, T.Text) -> S.Set (T.Text, T.Text)
collectParameters config stackToDeploy envVars env passedParams outputs =
  let globalParams = config ^. parameters
      localParams = stackToDeploy ^. parameters
  in globalParams <> localParams <> outputs <> envVars <> [("Env", env)] <> passedParams

getParameters :: (MonadCLI CliError m, MonadEnvironment m, MonadCloud m) 
              => DeploymentConfiguration -> StackConfiguration -> [T.Text] -> T.Text -> T.Text -> m (S.Set (T.Text, T.Text))
getParameters config stackToDeploy dependencies env appName = do 
  envVars <- getEnvVars config stackToDeploy
  outputs <- getOutputs dependencies env appName
  passedParams <- getPassedParameters config stackToDeploy
  return $ collectParameters config stackToDeploy envVars env passedParams outputs

validateTags :: (MonadError e m, AsCliError e) => S.Set (T.Text, T.Text) -> m (M.Map T.Text T.Text)
validateTags ts = assertUnique _CliDuplicateTagValues ts

validateParameters :: (AsCliError e, MonadError e m) => Template -> S.Set (T.Text, T.Text) -> m (M.Map T.Text T.Text)
validateParameters template params = do
  let paramSpecs = template ^. parameterSpecs
      requiredParamNames = S.fromList (paramSpecs ^.. _Wrapped.folded._Required)
      allowedParamNames = S.fromList (paramSpecs ^.. _Wrapped.folded.parameterKey)

  uniqueParams <- assertUnique _CliDuplicateParameterValues params
  let missingParamNames = requiredParamNames S.\\ M.keysSet uniqueParams
  unless (S.null missingParamNames) $
    throwing _CliMissingRequiredParameters missingParamNames

  return $ uniqueParams `M.intersection` M.fromSet (const ()) allowedParamNames

cliIO :: AppM DeploymentConfiguration -> IO ()
cliIO mConfig = runAppM $ cli mConfig

tuplesToMap :: Ord a => [(a, b)] -> M.Map a [b]
tuplesToMap xs = M.fromList $ map concatGroup grouped
  where
    grouped = groupBy ((==) `on` fst) xs
    concatGroup ys = (fst (head ys), map snd ys)

assertUnique :: MonadError e m => Prism' e (M.Map T.Text [T.Text]) -> S.Set (T.Text, T.Text) -> m (M.Map T.Text T.Text)
assertUnique _Err paramSet = case duplicateParams of
    [] -> return $ M.fromList paramList
    _ -> throwing _Err duplicateParams
  where
    paramList = S.toAscList paramSet
    paramsGrouped = tuplesToMap paramList
    duplicateParams = M.filter ((> 1) . length) paramsGrouped

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
