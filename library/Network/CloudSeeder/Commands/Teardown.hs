module Network.CloudSeeder.Commands.Teardown
  ( teardownCommand
  ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Semigroup ((<>))

import Prelude hiding (readFile)

import qualified Data.Text as T

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Commands.Shared
import Network.CloudSeeder.Types (StackName(..))

teardownCommand :: (AsCliError e, MonadCloud e m, MonadLogger m)
  => m (DeploymentConfiguration m) -> T.Text -> T.Text -> m ()
teardownCommand mConfig nameToTeardown env = do
  config <- mConfig
  void $ getStackFromConfig config nameToTeardown

  let appName = config ^. name
      fullStackName = mkFullStackName env appName nameToTeardown
      (StackName fullStackNameText) = fullStackName

  waitOnStack fullStackName
  logInfoN ("tearing down stack " <> fullStackNameText <> "...")
  deleteStack fullStackName
  waitOnStack fullStackName
  logInfoN ("teardown complete for stack " <> fullStackNameText)
