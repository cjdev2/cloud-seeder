module Network.CloudSeeder.Teardown
  ( teardownCommand
  ) where

import Control.Lens ((^.), has, only, to)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.List (find)
import Data.Semigroup ((<>))

import Prelude hiding (readFile)

import qualified Data.Text as T

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Shared
import Network.CloudSeeder.Types (StackName(..))

teardownCommand :: (AsCliError e, MonadCloud e m, MonadLogger m)
  => m (DeploymentConfiguration m) -> T.Text -> T.Text -> m ()
teardownCommand mConfig nameToTeardown env = do
  config <- mConfig
  void $ getStackToTeardown config nameToTeardown

  let appName = config ^. name
      fullStackName = mkFullStackName env appName nameToTeardown
      (StackName fullStackNameText) = fullStackName

  logInfoN ("deleting stack " <> fullStackNameText)
  deleteStack fullStackName

getStackToTeardown
  :: (AsCliError e, MonadError e m)
  => DeploymentConfiguration m -> T.Text -> m (StackConfiguration m)
getStackToTeardown config nameToTeardown = do
  let maybeStackToTeardown = config ^. stacks.to (find (has (name.only nameToTeardown)))
  maybe (throwing _CliStackNotConfigured nameToTeardown) return maybeStackToTeardown
