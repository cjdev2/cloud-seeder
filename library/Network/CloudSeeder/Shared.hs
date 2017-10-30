{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.Shared
  ( logStack
  , getStack
  , mkFullStackName
  ) where

import Control.Lens ((^.))
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Logger (MonadLogger, logInfo)
import Data.Semigroup ((<>))

import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Types

import qualified Data.Text as T

logStack :: MonadLogger m => Stack -> m ()
logStack stackInfo = $(logInfo) (render stackInfo)
  where
    render :: Stack -> T.Text
    render s = T.unlines
      [ "Stack Info:"
      , "  name: " <> s^.name
      , "  status: " <> T.pack (show (s^.stackStatus))
      , "  outputs: " <> T.pack (show (s^.outputs))
      ]

getStack :: (AsCliError e, MonadCloud e m) => StackName -> m Stack
getStack stackName = do
  maybeStack <- describeStack stackName
  let (StackName s) = stackName
  maybe (throwing _CliStackDoesNotExist s) pure maybeStack

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName
