{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.CloudSeeder.Test.Orphans () where

import Control.Monad.Mock (MockT)
import Network.CloudSeeder.Monads.CLI

instance MonadCli m => MonadCli (MockT f m)
