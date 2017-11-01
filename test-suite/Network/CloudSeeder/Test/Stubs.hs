{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.Test.Stubs where

import Control.Monad (unless)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Mock (MockT, WithResult(..), runMockT)
import Control.Monad.Mock.TH (makeAction, ts)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (StateT)
import Control.Monad.Writer (MonadWriter(..), WriterT(..), mapWriterT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger(..))
import Data.Align (alignWith)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Semigroup ((<>))
import Data.These (These(..))
import Options.Applicative (ParserInfo(..), ParserResult(..), execParserPure, defaultPrefs, renderFailure)
import System.Log.FastLogger (fromLogStr, toLogStr)

import qualified Data.Text as T
import qualified Data.Map as M

import Network.CloudSeeder.CommandLine hiding (Wait, wait)
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces

--------------------------------------------------------------------------------
-- STUBS
--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT [String] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadWriter w, MonadLogger, MonadFileSystem e, MonadCloud e
           , MonadEnvironment )

-- | Runs a computation with access to a set of command-line arguments.
stubCommandLineT :: [String] -> ArgumentsT m a -> m a
stubCommandLineT fake (ArgumentsT x) = runReaderT x fake

instance Monad m => MonadCLI (ArgumentsT m) where
  getArgs = ArgumentsT $ do
    input <- ask
    return $ take 3 input

  getOptions pSpecs = ArgumentsT $ do
    input <- ask
    return $ consume (parseOptions pSpecs) input

consume :: ParserInfo a -> [String] -> a
consume p x = case parserErrorOrResult of
    Right result -> result
    Left err -> error err
  where parserErrorOrResult = parserResult $ execParserPure defaultPrefs p x

parserResult :: ParserResult a -> Either String a
parserResult (Success a) = return a
parserResult (Failure failure) = Left msg
  where (msg, _) = renderFailure failure "stub"
parserResult _ = error "parserResult: completion encountered"

--------------------------------------------------------------------------------
-- Logger

newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadCLI, MonadFileSystem e, MonadCloud e, MonadEnvironment )

-- | Runs a computation that may emit log messages, returning the result of the
-- computation.
stubLoggerT :: Monad m => [ByteString] -> LoggerT m a -> m a
stubLoggerT expectedLogs (LoggerT x) = do
  (result, logs) <- runWriterT x
  unless (expectedLogs == logs) $
    error $ "stubLoggerT -- expected logs did not match actual logs: \n"
      <> unlines (unpack <$> alignWith render expectedLogs logs)
  pure result
  where
    render :: These ByteString ByteString -> ByteString
    render = \case
      These expected actual -> "  expected: '" <> expected <> "' | actual: '" <> actual <> "'"
      This expected -> "  expected: '" <> expected <> "' | actual: NULL "
      That actual -> "  expected: NULL | actual: '" <> actual <> "'"

ignoreLoggerT :: Monad m => LoggerT m a -> m ()
ignoreLoggerT (LoggerT x) = do
  _ <- runWriterT x
  pure ()

instance MonadWriter w m => MonadWriter w (LoggerT m) where
  tell = lift . tell
  listen (LoggerT x) = LoggerT $ flip mapWriterT x $ \y -> do
    ((r, bs), w) <- listen y
    return ((r, w), bs)
  pass (LoggerT x) = LoggerT $ flip mapWriterT x $ \y -> pass $ do
    ((r, f), bs) <- y
    return ((r, bs), f)

instance Monad m => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- File System

newtype FileSystemT m a = FileSystemT (ReaderT [(T.Text, T.Text)] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadWriter w , MonadCLI, MonadLogger, MonadCloud e
           , MonadEnvironment )

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
           , MonadWriter w, MonadCLI, MonadLogger, MonadFileSystem e
           , MonadCloud e )

stubEnvironmentT :: M.Map T.Text T.Text -> EnvironmentT m a -> m a
stubEnvironmentT fs (EnvironmentT x) = runReaderT x fs

instance Monad m => MonadEnvironment (EnvironmentT m) where
  getEnv x = M.lookup x <$> EnvironmentT ask

--------------------------------------------------------------------------------
-- MOCKS
--------------------------------------------------------------------------------
-- Missiles

class Monad m => MonadMissiles m where
  launchMissiles :: m ()

  default launchMissiles :: (MonadTrans t, MonadMissiles m', m ~ t m') => m ()
  launchMissiles = lift launchMissiles

instance MonadMissiles m => MonadMissiles (ExceptT e m)
instance MonadMissiles m => MonadMissiles (ReaderT r m)
instance MonadMissiles m => MonadMissiles (StateT s m)
instance MonadMissiles m => MonadMissiles (FileSystemT m)
instance MonadMissiles m => MonadMissiles (EnvironmentT m)
instance MonadMissiles m => MonadMissiles (ArgumentsT m)
instance MonadMissiles m => MonadMissiles (CreateT m)
instance MonadMissiles m => MonadMissiles (LoggerT m)

makeAction "CloudAction" [ts| MonadCloud CliError, MonadMissiles |]
mockActionT :: Monad m => [WithResult CloudAction] -> MockT CloudAction m a -> m a
mockActionT = runMockT
