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
import Control.Lens ((^.), (^..), each, has, only, to)
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Aeson.Lens
import Data.HashMap.Lazy (keys)
import Data.List (find, sort)
import Data.Text.Encoding (encodeUtf8)
import Data.Semigroup ((<>))
import Data.Yaml (Value(..), decodeEither)
import Network.AWS (Credentials(Discover), Env, newEnv)
import System.Exit (exitFailure)

import Prelude hiding (readFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Interfaces

--------------------------------------------------------------------------------
-- IO wiring

data CliError
  = CliMissingEnvVars [T.Text]
  | CliFileSystemError FileSystemError
  | CliStackNotConfigured T.Text
  | CliMissingDependencyStacks [T.Text]
  | CliTemplateDecodeFail String
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

newtype AppM a = AppM (ReaderT Env (ExceptT CliError (LoggingT IO)) a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO
           , MonadCatch, MonadThrow, MonadReader Env, MonadError CliError
           , MonadLogger, MonadArguments, MonadEnvironment )

instance MonadBaseControl IO AppM where
  type StM AppM a = StM (ReaderT Env (ExceptT CliError (LoggingT IO))) a
  liftBaseWith f = AppM (liftBaseWith (\g -> f (\(AppM x) -> g x)))
  restoreM = AppM . restoreM

instance MonadFileSystem CliError AppM where
  readFile = readFile'

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

cli :: (MonadArguments m, MonadCloud m, MonadFileSystem CliError m, MonadEnvironment m) => m DeploymentConfiguration -> m ()
cli mConfig = do
  (DeployStack nameToDeploy env) <- getArgs
  config <- mConfig

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
  envVars <- either (throwError . CliMissingEnvVars . sort) return envVarsOrFailure

  let baseTags = [("cj:environment", env), ("cj:application", appName)]
      globalTags = config ^. tagSet
      localTags = stackToDeploy ^. tagSet
      stackTags = sort (baseTags ++ globalTags ++ localTags)
      mkStackName s = StackName $ env <> "-" <> appName <> "-" <> s

  templateBody <- readFile $ nameToDeploy <> ".yaml"

  let decodeOrFailure = decodeEither (encodeUtf8 templateBody) :: Either String Value
  template <- either (throwing _CliTemplateDecodeFail) return decodeOrFailure
  let requiredParams = template ^. key "Parameters" . _Object.to keys

  maybeOutputs <- mapM (\stackName -> (stackName,) <$> getStackOutputs (mkStackName stackName)) dependencies
  let outputsOrFailure = runErrors $ traverse (extractResult (flip const)) maybeOutputs
  outputs <- either (throwing _CliMissingDependencyStacks) (return . concat) outputsOrFailure

  let globalParams = (config ^. parameters) ++ [("Env", env)]
      localParams = stackToDeploy ^. parameters
      allParams = globalParams ++ localParams
      isRequired = (`elem` requiredParams)
      stackParams = filter (isRequired . fst) (allParams ++ outputs ++ envVars)

  csId <- computeChangeset (mkStackName nameToDeploy) templateBody stackParams stackTags
  runChangeSet csId

cliIO :: AppM DeploymentConfiguration -> IO ()
cliIO mConfig = runAppM $ cli mConfig

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
