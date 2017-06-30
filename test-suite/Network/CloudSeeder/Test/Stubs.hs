{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.Test.Stubs where

import qualified Data.Text as T

import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger(..))
import Data.ByteString (ByteString)
import System.Log.FastLogger (fromLogStr, toLogStr)

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Interfaces

--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT Options m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadLogger, MonadFileSystem e, MonadCloud, MonadEnvironment )

-- | Runs a computation with access to a set of command-line arguments.
runArgumentsT :: Options -> ArgumentsT m a -> m a
runArgumentsT opts (ArgumentsT x) = runReaderT x opts

instance Monad m => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT ask

--------------------------------------------------------------------------------
-- Logger

newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadArguments, MonadFileSystem e, MonadCloud, MonadEnvironment )

-- | Runs a computation that may emit log messages, returning the result of the
-- computation combined with the set of messages logged, in order.
runLoggerT :: LoggerT m a -> m (a, [ByteString])
runLoggerT (LoggerT x) = runWriterT x

instance Monad m => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- File System

newtype FileSystemT m a = FileSystemT (ReaderT [(T.Text, T.Text)] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadArguments, MonadLogger, MonadCloud, MonadEnvironment )

-- | Runs a computation that may interact with the file system, given a mapping
-- from file paths to file contents.
stubFileSystemT :: [(T.Text, T.Text)] -> FileSystemT m a -> m a
stubFileSystemT fs (FileSystemT x) = runReaderT x fs

instance (AsFileSystemError e, MonadError e m) => MonadFileSystem e (FileSystemT m) where
  readFile path = FileSystemT $ ask >>= \files ->
    maybe (throwing _FileNotFound path)
          return (lookup path files)

--------------------------------------------------------------------------------
-- Environment

newtype EnvironmentT m a = EnvironmentT (ReaderT [(T.Text, T.Text)] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadArguments, MonadLogger, MonadFileSystem e, MonadCloud )

stubEnvironmentT :: [(T.Text, T.Text)] -> EnvironmentT m a -> m a
stubEnvironmentT fs (EnvironmentT x) = runReaderT x fs

instance Monad m => MonadEnvironment (EnvironmentT m) where
  getEnv x = lookup x <$> EnvironmentT ask
