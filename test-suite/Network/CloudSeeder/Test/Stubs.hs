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
import Data.Maybe (fromJust)
import Options.Applicative (ParserInfo(..), execParserPure, defaultPrefs, getParseResult)
import System.Log.FastLogger (fromLogStr, toLogStr)

import qualified Data.Map as M

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Interfaces

--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT [String] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadLogger, MonadFileSystem e, MonadCloud, MonadEnvironment )

-- | Runs a computation with access to a set of command-line arguments.
stubArgumentsT :: [String] -> ArgumentsT m a -> m a
stubArgumentsT fake (ArgumentsT x) = runReaderT x fake

instance (AsArgumentsError e, MonadError e m) => MonadCLI e (ArgumentsT m) where
  getArgs = ArgumentsT $ do 
    input <- ask
    return $ consume parseArguments $ take 3 input

  getOptions pSpecs = ArgumentsT $ do 
    input <- ask
    let x = execParserPure defaultPrefs (parseOptions pSpecs) input
    return $ fromJust $ getParseResult x

consume :: ParserInfo c -> [String] -> c
consume p = fromJust . getParseResult . execParserPure defaultPrefs p

--------------------------------------------------------------------------------
-- Logger

newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadCLI e, MonadFileSystem e, MonadCloud, MonadEnvironment )

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
           , MonadCLI e, MonadLogger, MonadCloud, MonadEnvironment )

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

newtype EnvironmentT m a = EnvironmentT (ReaderT (M.Map T.Text T.Text) m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadCLI e, MonadLogger, MonadFileSystem e, MonadCloud )

stubEnvironmentT :: M.Map T.Text T.Text -> EnvironmentT m a -> m a
stubEnvironmentT fs (EnvironmentT x) = runReaderT x fs

instance Monad m => MonadEnvironment (EnvironmentT m) where
  getEnv x = M.lookup x <$> EnvironmentT ask
