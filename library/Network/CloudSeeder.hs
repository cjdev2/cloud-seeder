module Network.CloudSeeder
  ( deployment
  , module Network.CloudSeeder.CommandLine
  , module Network.CloudSeeder.DSL
  , module Network.CloudSeeder.Interfaces
  , module Control.Monad
  ) where

import qualified Network.CloudSeeder.DSL as DSL

import Data.Text (Text)
import Control.Monad
import Control.Monad.State (StateT)

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.DSL hiding (deployment)
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Main

deployment :: Text -> StateT DeploymentConfiguration AppM a -> IO ()
deployment x y = cliIO $ DSL.deployment x y
