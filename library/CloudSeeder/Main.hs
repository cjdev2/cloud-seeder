{-# LANGUAGE OverloadedStrings #-}

module CloudSeeder.Main
  ( Command(..)
  , DescribeOptions(..)
  , DeployOptions(..)
  , mainIO
  , main
  , MonadArguments(..)
  , MonadDeploy(..)
  , MonadFileSystem(..)
  , Options(..)
  , StackName(..)
  ) where

import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT, logInfoN)
import Prelude hiding (readFile)

import qualified Data.Text as T

import CloudSeeder.Interfaces
import CloudSeeder.CommandLine

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadLogger, MonadDeploy, MonadArguments, MonadFileSystem )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

mainIO :: IO ()
mainIO = runAppM main

--------------------------------------------------------------------------------
-- Logic

runCmd :: (MonadFileSystem m, MonadDeploy m, MonadLogger m, MonadArguments m) => Command -> m (Either T.Text T.Text)
runCmd opts = case opts of
  DescribeStack x -> describeStack (StackName $ desStackName x)
  DeployStack x -> do
      templateBody <- readFile (depTemplatePath x)
      deployStack (StackName $ depStackName x) templateBody (depParameters x)

main :: (MonadFileSystem m, MonadDeploy m, MonadLogger m, MonadArguments m) => m ()
main = do
  (Options opts) <- getArgs
  result <- runCmd opts
  either logInfoN logInfoN result
