module Network.CloudSeeder.Wait
  ( waitCommand
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger)

import qualified Data.Text as T

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Shared

waitCommand :: (AsCliError e, MonadCloud e m, MonadLogger m)
  => m (DeploymentConfiguration m) -> T.Text -> T.Text -> m ()
waitCommand mConfig nameToWaitFor env = do
  config <- mConfig
  let appName = config ^. name
      stackName = mkFullStackName env appName nameToWaitFor
  stackInfo <- waitOnStack stackName
  logStack stackInfo
