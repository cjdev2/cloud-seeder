{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Network.CloudSeeder.DSL
  ( DeploymentConfiguration(..)
  , StackConfiguration(..)
  , HasName(..)
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
  , _deploymentConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _deploymentConfigurationStacks :: [StackConfiguration]
  , _deploymentConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  } deriving (Eq, Show)

data StackConfiguration = StackConfiguration
  { _stackConfigurationName :: T.Text
  , _stackConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _stackConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  } deriving (Eq, Show)

makeFields ''DeploymentConfiguration
makeFields ''StackConfiguration

deployment :: Monad m => T.Text -> StateT DeploymentConfiguration m a -> m DeploymentConfiguration
deployment name' x =
  let config = DeploymentConfiguration name' [] [] []
  in execStateT x config

stack_ :: Monad m => T.Text -> StateT DeploymentConfiguration m ()
stack_ name' = stack name' $ return ()

stack :: Monad m => T.Text -> StateT StackConfiguration m a -> StateT DeploymentConfiguration m ()
stack name' x = do
  let stackConfig = StackConfiguration name' [] []
  stackConfig' <- lift $ execStateT x stackConfig
  stacks %= (++ [stackConfig'])

environment :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource))) => [T.Text] -> StateT a m ()
environment = mapM_ (`paramSource` Env)

paramSource :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)) ) => T.Text -> ParameterSource -> StateT a m ()
paramSource pName source = parameterSources %= S.insert (pName, source)

flag :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource))) => T.Text -> StateT a m ()
flag pName = paramSource pName Flag

param :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)))
      => T.Text -> T.Text -> StateT a m ()
param key val = paramSource key (Constant val)

tags :: (Monad m, HasTagSet a (S.Set (T.Text, T.Text))) => [(T.Text, T.Text)] -> StateT a m ()
tags ts = tagSet %= (<> S.fromList ts)
