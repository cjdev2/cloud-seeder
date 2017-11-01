{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.Main
  ( AppM
  , cli
  , cliIO
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Network.AWS (Credentials(Discover), Env, newEnv)
import System.Exit (exitFailure)

import Prelude hiding (readFile)

import qualified Data.Text.IO as T

import qualified Network.CloudSeeder.CommandLine as CL
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Provision
import Network.CloudSeeder.Shared
import Network.CloudSeeder.Wait

--------------------------------------------------------------------------------
-- IO wiring

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

instance MonadCli AppM where
  getArgs = getArgs'

instance MonadCloud CliError AppM where
  computeChangeset = computeChangeset'
  describeStack = describeStack'
  runChangeSet = runChangeSet'
  encrypt = encrypt'
  upload = upload'
  generateSecret = generateSecret'
  wait = wait'

runAppM :: AppM a -> IO a
runAppM (AppM x) = do
  env <- newEnv Discover
  result <- runStderrLoggingT (runExceptT (runReaderT x env))
  either (\err -> T.putStr (renderCliError err) >> exitFailure) return result

--------------------------------------------------------------------------------
-- Logic

cli
  :: (AsCliError e, MonadCli m, MonadCloud e m, MonadFileSystem e m, MonadEnvironment m, MonadLogger m)
  => m (DeploymentConfiguration m) -> m ()
cli mConfig = do
  input <- getArgs
  cmd <- parseArgs input
  case cmd of
    CL.Wait nameToWaitFor env -> waitCommand mConfig nameToWaitFor env
    CL.ProvisionStack nameToProvision env -> provisionCommand mConfig nameToProvision env input

cliIO :: AppM (DeploymentConfiguration AppM) -> IO ()
cliIO mConfig = runAppM $ cli mConfig
