module Network.CloudSeeder
  ( deployment
  , module Network.CloudSeeder.DSL
  , module Network.CloudSeeder.Monads.AWS
  , module Network.CloudSeeder.Monads.CLI
  , module Network.CloudSeeder.Monads.Environment
  , module Network.CloudSeeder.Monads.FileSystem
  , module Network.CloudSeeder.Commands.Shared
  , module Control.Monad
  ) where

import qualified Network.CloudSeeder.DSL as DSL

import Data.Text (Text)
import Control.Monad
import Control.Monad.State (StateT)

import Network.CloudSeeder.Commands.Shared
import Network.CloudSeeder.DSL hiding (deployment)
import Network.CloudSeeder.Main
import Network.CloudSeeder.Monads.AWS
import Network.CloudSeeder.Monads.CLI
import Network.CloudSeeder.Monads.Environment
import Network.CloudSeeder.Monads.FileSystem

deployment :: Text -> StateT (DeploymentConfiguration AppM) AppM a -> IO ()
deployment x y = cliIO $ DSL.deployment x y
