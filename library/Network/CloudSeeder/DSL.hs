{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Network.CloudSeeder.DSL
  ( DeploymentConfiguration(..)
  , StackConfiguration(..)
  , HasEnvironmentVariables(..)
  , HasName(..)
  , HasParameters(..)
  , HasParameterSources(..)
  , HasStacks(..)
  , HasTagSet(..)
  , deployment
  , environment
  , flag
  , tags
  , param 
  , stack_
  , stack 
  ) where

import Control.Lens ((%=))
import Control.Monad.State (StateT, execStateT, lift)
import Control.Lens.TH (makeFields)
import Data.Semigroup ((<>))

import qualified Data.Set as S
import qualified Data.Text as T

import Network.CloudSeeder.Types

data DeploymentConfiguration = DeploymentConfiguration
  { _deploymentConfigurationName :: T.Text
  , _deploymentConfigurationEnvironmentVariables :: [T.Text]
  , _deploymentConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _deploymentConfigurationStacks :: [StackConfiguration]
  , _deploymentConfigurationParameters :: S.Set (T.Text, T.Text)
  , _deploymentConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  } deriving (Eq, Show)

data StackConfiguration = StackConfiguration
  { _stackConfigurationName :: T.Text
  , _stackConfigurationEnvironmentVariables :: [T.Text]
  , _stackConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _stackConfigurationParameters :: S.Set (T.Text, T.Text)
  , _stackConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  } deriving (Eq, Show)

makeFields ''DeploymentConfiguration
makeFields ''StackConfiguration

paramSource :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)) ) => T.Text -> ParameterSource -> StateT a m ()
paramSource pName source = parameterSources %= (S.insert (pName, source))

deployment :: Monad m => T.Text -> StateT DeploymentConfiguration m a -> m DeploymentConfiguration
deployment name' x =
  let config = DeploymentConfiguration name' [] [] [] [] []
  in execStateT x config

environment :: (Monad m, HasEnvironmentVariables a [T.Text]) => [T.Text] -> StateT a m ()
environment vars = environmentVariables %= (++ vars)

flag :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource))) => T.Text -> StateT a m ()
flag pName = paramSource pName Flag

tags :: (Monad m, HasTagSet a (S.Set (T.Text, T.Text))) => [(T.Text, T.Text)] -> StateT a m ()
tags ts = tagSet %= (<> (S.fromList ts))

param :: (Monad m, HasParameters a (S.Set (T.Text, T.Text)), HasParameterSources a (S.Set (T.Text, ParameterSource))) 
      => T.Text -> T.Text -> StateT a m ()
param key val = do 
  parameters %= (<> S.fromList [(key, val)])
  paramSource key Constant

stack_ :: Monad m => T.Text -> StateT DeploymentConfiguration m ()
stack_ name' = stack name' $ return ()

stack :: Monad m => T.Text -> StateT StackConfiguration m a -> StateT DeploymentConfiguration m ()
stack name' x = do
  let stackConfig = StackConfiguration name' [] [] [] []
  stackConfig' <- lift $ execStateT x stackConfig
  stacks %= (++ [stackConfig'])
