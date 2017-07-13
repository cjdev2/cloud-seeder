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
import Control.Lens (Prism', (^.), (^..), each, filtered, folded, makeClassy, makeClassyPrisms, has, only, to)
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

instance MonadArguments CliError AppM where 
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

cli :: (MonadArguments CliError m, MonadCloud m, MonadFileSystem CliError m, MonadEnvironment m) => m DeploymentConfiguration -> m ()
cli mConfig = do
  config <- mConfig
  (DeployStack nameToDeploy env) <- getArgs

  let allNames = config ^.. stacks.each.name
      dependencies = takeWhile (/= nameToDeploy) allNames
      appName = config ^. name
      maybeStackToDeploy = config ^. stacks.to (find (has (name.only nameToDeploy)))

  stackToDeploy <- maybe (throwing _CliStackNotConfigured nameToDeploy) return maybeStackToDeploy
  let requiredGlobalEnvVars = config ^. environmentVariables
      requiredStackEnvVars = stackToDeploy ^. environmentVariables
      requiredEnvVars = requiredGlobalEnvVars ++ requiredStackEnvVars

  maybeEnvValues <- mapM (\envVarKey -> (envVarKey,) <$> getEnv envVarKey) requiredEnvVars
  let envVarsOrFailure = runErrors $ traverse (extractResult (,)) maybeEnvValues
  envVars <- either (throwError . CliMissingEnvVars . sort) (return . S.fromList) envVarsOrFailure

  let baseTags = [("cj:environment", env), ("cj:application", appName)]
      globalTags = config ^. tagSet
      localTags = stackToDeploy ^. tagSet
      stackTags = baseTags <> globalTags <> localTags
      mkStackName s = StackName $ env <> "-" <> appName <> "-" <> s

  templateBody <- readFile $ nameToDeploy <> ".yaml"
  let decodeOrFailure = decodeEither (encodeUtf8 templateBody) :: Either String Template
  template <- either (throwing _CliTemplateDecodeFail) return decodeOrFailure

  let paramSpecs = template ^. parameterSpecs

  maybeOutputs <- mapM (\stackName -> (stackName,) <$> getStackOutputs (mkStackName stackName)) dependencies
  let outputsOrFailure = runErrors $ traverse (extractResult (flip const)) maybeOutputs
  outputs <- either (throwing _CliMissingDependencyStacks) (return . S.fromList . concatMap M.toList) outputsOrFailure

  let globalParams = config ^. parameters
      localParams = stackToDeploy ^. parameters
      globalParamSources :: S.Set (T.Text, ParameterSource)
      globalParamSources = config ^. parameterSources
      localParamSources :: S.Set (T.Text, ParameterSource)
      localParamSources = stackToDeploy ^. parameterSources
      paramFlags :: S.Set (T.Text, ParameterSource)
      paramFlags = S.fromList $ (globalParamSources <> localParamSources)^..folded.filtered isFlag 
      --TODO: use more sweet lensiness, a la something like replacing "isFlag" w/ "(_2.is _Flag)"
      isFlag (_, Flag) = True
      isFlag (_, _) = False

  passedParams <- S.fromList . M.toList <$> getOptions paramFlags

  let allParams = globalParams <> localParams <> outputs <> envVars <> [("Env", env)] <> passedParams
      requiredParamNames = S.fromList (paramSpecs ^.. folded._Required)
      allowedParamNames = S.fromList (paramSpecs ^.. folded.parameterKey)

  uniqueParams <- assertUnique _CliDuplicateParameterValues allParams
  uniqueTags <- assertUnique _CliDuplicateTagValues stackTags

  let missingParamNames = requiredParamNames S.\\ M.keysSet uniqueParams
      allowedParams = uniqueParams `M.intersection` M.fromSet (const ()) allowedParamNames
  unless (S.null missingParamNames) $
    throwing _CliMissingRequiredParameters missingParamNames

  csId <- computeChangeset (mkStackName nameToDeploy) templateBody allowedParams uniqueTags
  runChangeSet csId

cliIO :: AppM DeploymentConfiguration -> IO ()
cliIO mConfig = runAppM $ cli mConfig

tuplesToMap :: Ord a => [(a, b)] -> M.Map a [b]
tuplesToMap xs = M.fromList $ map concatGroup grouped
  where
    grouped = groupBy ((==) `on` fst) xs
    concatGroup ys = (fst (head ys), map snd ys)

assertUnique :: (AsCliError e, MonadError e m) => Prism' e (M.Map T.Text [T.Text]) -> S.Set (T.Text, T.Text) -> m (M.Map T.Text T.Text)
assertUnique _Err paramSet = case duplicateParams of
    [] -> return $ M.fromList paramList
    _ -> throwing _Err duplicateParams
  where
    paramList = S.toList paramSet
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
