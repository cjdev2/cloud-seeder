{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.DSL
  ( DeploymentConfiguration(..)
  , StackConfiguration(..)
  , environmentVariables
  , name
  , stacks
  , deployment
  , environment
  , stack_
  , stack
  )
  where

import Control.Lens ((%=))
import Control.Monad.State (StateT, execStateT, lift)
import Control.Lens.TH (makeFields)
import qualified Data.Text as T

data DeploymentConfiguration = DeploymentConfiguration
  { _deploymentConfigurationName :: T.Text
  , _deploymentConfigurationEnvironmentVariables :: [T.Text]
  , _deploymentConfigurationStacks :: [StackConfiguration]
  } deriving (Eq, Show)

data StackConfiguration = StackConfiguration
  { _stackConfigurationName :: T.Text
  , _stackConfigurationEnvironmentVariables :: [T.Text]
  } deriving (Eq, Show)

makeFields ''DeploymentConfiguration
makeFields ''StackConfiguration

deployment :: Monad m => T.Text -> StateT DeploymentConfiguration m a -> m DeploymentConfiguration
deployment name' x =
  let config = DeploymentConfiguration name' [] []
  in execStateT x config

environment :: (Monad m, HasEnvironmentVariables a [T.Text]) => [T.Text] -> StateT a m ()
environment vars = environmentVariables %= (++ vars)

stack_ :: Monad m => T.Text -> StateT DeploymentConfiguration m ()
stack_ name' = stack name' $ return ()

stack :: Monad m => T.Text -> StateT StackConfiguration m a -> StateT DeploymentConfiguration m ()
stack name' x = do
  let stackConfig = StackConfiguration name' []
  stackConfig' <- lift $ execStateT x stackConfig
  stacks %= (++ [stackConfig'])
