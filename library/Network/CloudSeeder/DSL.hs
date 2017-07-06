{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.DSL
  ( DeploymentConfiguration(..)
  , StackConfiguration(..)
  , environmentVariables
  , getEnvArg
  , name
  , stacks
  , deployment
  , environment
  , optional
  , optionals
  , param
  , parameters
  , stack_
  , stack
  , tags
  , tagSet
  , whenEnv
  ) where

import Control.Lens ((%=))
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT, lift)
import Control.Lens.TH (makeFields)
import qualified Data.Text as T

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Interfaces

data DeploymentConfiguration = DeploymentConfiguration
  { _deploymentConfigurationName :: T.Text
  , _deploymentConfigurationEnvironmentVariables :: [T.Text]
  , _deploymentConfigurationTagSet :: [(T.Text, T.Text)]
  , _deploymentConfigurationStacks :: [StackConfiguration]
  , _deploymentConfigurationParameters :: [(T.Text, T.Text)]
  , _deploymentConfigurationOptionals :: [T.Text]
  } deriving (Eq, Show)

data StackConfiguration = StackConfiguration
  { _stackConfigurationName :: T.Text
  , _stackConfigurationEnvironmentVariables :: [T.Text]
  , _stackConfigurationTagSet :: [(T.Text, T.Text)]
  , _stackConfigurationParameters :: [(T.Text, T.Text)]
  , _stackConfigurationOptionals :: [T.Text]
  } deriving (Eq, Show)

makeFields ''DeploymentConfiguration
makeFields ''StackConfiguration

deployment :: Monad m => T.Text -> StateT DeploymentConfiguration m a -> m DeploymentConfiguration
deployment name' x =
  let config = DeploymentConfiguration name' [] [] [] [] []
  in execStateT x config

environment :: (Monad m, HasEnvironmentVariables a [T.Text]) => [T.Text] -> StateT a m ()
environment vars = environmentVariables %= (++ vars)

tags :: (Monad m, HasTagSet a [(T.Text, T.Text)] ) => [(T.Text, T.Text)] -> StateT a m ()
tags ts = tagSet %= (++ ts)

optional :: (Monad m, HasOptionals a [T.Text]) => T.Text -> StateT a m ()
optional opt = optionals %= (++ [opt])

param :: (Monad m, HasParameters a [(T.Text, T.Text)] ) => T.Text -> T.Text -> StateT a m ()
param key val = parameters %= (++ [(key, val)])

stack_ :: Monad m => T.Text -> StateT DeploymentConfiguration m ()
stack_ name' = stack name' $ return ()

stack :: Monad m => T.Text -> StateT StackConfiguration m a -> StateT DeploymentConfiguration m ()
stack name' x = do
  let stackConfig = StackConfiguration name' [] [] [] []
  stackConfig' <- lift $ execStateT x stackConfig
  stacks %= (++ [stackConfig'])

getEnvArg :: MonadArguments m => m T.Text
getEnvArg = do
  (DeployStack _ env _) <- getArgs
  return env

whenEnv :: MonadArguments m => T.Text -> m () -> m ()
whenEnv env x = do
  envToDeploy <- getEnvArg
  when (envToDeploy == env) x
