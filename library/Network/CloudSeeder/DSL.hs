{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.DSL
  ( DeploymentConfiguration(..)
  , StackConfiguration(..)
  , HasName(..)
  , HasParameterSources(..)
  , HasStacks(..)
  , HasTagSet(..)
  , HasGlobalStack(..)
  , HasHooksCreate(..)
  , CanSetConstant(..)
  , CreateT(..)
  , HookContext(..)
  , password
  , deploymentConfiguration
  , stackConfiguration
  , deployment
  , environment
  , flag
  , global
  , tags
  , onCreate
  , stack_
  , stack
  ) where

import Control.Lens ((%=), (.=), (^.))
import Control.Monad.Except (MonadError(..), ExceptT)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Reader (MonadReader(..), ReaderT, ask)
import Control.Monad.State (StateT, execStateT, modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Lens.TH (makeFields)
import Data.Semigroup ((<>))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Network.CloudSeeder.Error
import Network.CloudSeeder.Types
import Network.CloudSeeder.Interfaces

data HookContext m = HookContext
  { _hookContextOutputs :: S.Set (T.Text, T.Text)
  , _hookContextDeploymentConfiguration :: DeploymentConfiguration m
  , _hookContextStackConfiguration :: StackConfiguration m
  }

newtype CreateT m a = CreateT (StateT (M.Map T.Text T.Text) (ExceptT CliError (ReaderT (HookContext m) m)) a)
  deriving (Functor, Applicative, Monad, MonadError CliError)

instance MonadTrans CreateT where
  lift = CreateT . lift . lift . lift

data DeploymentConfiguration m = DeploymentConfiguration
  { _deploymentConfigurationName :: T.Text
  , _deploymentConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _deploymentConfigurationStacks :: [StackConfiguration m]
  , _deploymentConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  }

data StackConfiguration m = StackConfiguration
  { _stackConfigurationName :: T.Text
  , _stackConfigurationTagSet :: S.Set (T.Text, T.Text)
  , _stackConfigurationParameterSources :: S.Set (T.Text, ParameterSource)
  , _stackConfigurationGlobalStack :: Bool
  , _stackConfigurationHooksCreate :: [CreateT m ()]
  }

makeFields ''DeploymentConfiguration
makeFields ''StackConfiguration
makeFields ''HookContext

class Monad m => CanSetConstant m where
  param :: T.Text -> T.Text -> m ()

instance Monad m => CanSetConstant (CreateT m) where
  param key val = CreateT $ modify (M.insert key val)

instance (Monad m, HasParameterSources s (S.Set (T.Text, ParameterSource))) => CanSetConstant (StateT s m) where
  param key val = paramSource key (Constant val)

deployment :: Monad m => T.Text -> StateT (DeploymentConfiguration m) m a -> m (DeploymentConfiguration m)
deployment name' x =
  let config = DeploymentConfiguration name' [] [] []
  in execStateT x config

stack_ :: Monad m => T.Text -> StateT (DeploymentConfiguration m) m ()
stack_ name' = stack name' $ return ()

stack :: Monad m => T.Text -> StateT (StackConfiguration m) m a -> StateT (DeploymentConfiguration m) m ()
stack name' x = do
  let stackConfig = StackConfiguration name' [] [] False []
  stackConfig' <- lift $ execStateT x stackConfig
  stacks %= (++ [stackConfig'])

global :: (Monad m, HasGlobalStack a Bool) => StateT a m ()
global = globalStack .= True

environment :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)))
            => [T.Text] -> StateT a m ()
environment = mapM_ (`paramSource` Env)

paramSource :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)))
            => T.Text -> ParameterSource -> StateT a m ()
paramSource pName source = parameterSources %= S.insert (pName, source)

flag :: (Monad m, HasParameterSources a (S.Set (T.Text, ParameterSource)))
     => T.Text -> StateT a m ()
flag pName = paramSource pName Flag

tags :: (Monad m, HasTagSet a (S.Set (T.Text, T.Text)))
     => [(T.Text, T.Text)] -> StateT a m ()
tags ts = tagSet %= (<> S.fromList ts)

onCreate :: (Monad m, HasHooksCreate s [CreateT m ()])
         => CreateT m () -> StateT s m ()
onCreate action = hooksCreate %= (++ [action])

-- | Generates and stores a password
password :: (MonadCloud m)
  => T.Text -- ^ path in S3 bucket to store the password
  -> Int -- ^ desired password length
  -> T.Text -- ^ output that exports the encryption key
  -> T.Text -- ^ output that exports the bucket where secrets are stored
  -> CreateT m ()
password path passwordLength encryptionKeyOutput secretsStoreOutput = do
  context <- CreateT ask
  let outputs' = context ^. outputs
      outputsMap = M.fromList . S.toAscList $ outputs'
  encryptionKeyId <- maybe (throwing _CliMissingRequiredOutput encryptionKeyOutput)
    return (M.lookup encryptionKeyOutput outputsMap)
  secretsStore <- maybe (throwing _CliMissingRequiredOutput secretsStoreOutput)
    return (M.lookup secretsStoreOutput outputsMap)
  CreateT $ generateEncryptUploadSecret passwordLength encryptionKeyId secretsStore path
