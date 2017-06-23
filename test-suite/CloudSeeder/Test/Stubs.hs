{-# LANGUAGE UndecidableInstances #-}

module CloudSeeder.Test.Stubs where

import qualified Data.Text as T

import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.State (StateT(..), get, put)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger(..))
import Data.ByteString (ByteString)
import Data.Type.Equality ((:~:)(..))
import System.Log.FastLogger (fromLogStr, toLogStr)

import CloudSeeder.CommandLine
import CloudSeeder.Interfaces

--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT Command m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadLogger, MonadFileSystem e, MonadCloud, MonadEnvironment )

-- | Runs a computation with access to a set of command-line arguments.
runArgumentsT :: Command -> ArgumentsT m a -> m a
runArgumentsT args (ArgumentsT x) = runReaderT x args

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
-- Cloud

data CloudAction r where
  ComputeChangeset :: StackName -> T.Text -> [(T.Text, T.Text)] -> CloudAction T.Text
  DescribeStack :: StackName -> CloudAction (Maybe [(T.Text, T.Text)])
  RunChangeSet :: T.Text -> CloudAction ()
deriving instance Eq (CloudAction r)
deriving instance Show (CloudAction r)

eqAction :: CloudAction a -> CloudAction b -> Maybe (a :~: b)
eqAction (ComputeChangeset a b c) (ComputeChangeset a' b' c')
  = if a == a' && b == b' && c == c' then Just Refl else Nothing
eqAction (DescribeStack a) (DescribeStack a')
  = if a == a' then Just Refl else Nothing
eqAction (RunChangeSet a) (RunChangeSet a')
  = if a == a' then Just Refl else Nothing
eqAction _ _ = Nothing

data WithResult f where
  (:->) :: f r -> r -> WithResult f

newtype CloudT m a = CloudT (StateT [WithResult CloudAction] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadArguments, MonadFileSystem e, MonadLogger, MonadEnvironment )

stubCloudT :: Monad m => [WithResult CloudAction] -> CloudT m a -> m a
stubCloudT actions (CloudT x) = runStateT x actions >>= \case
  (r, []) -> return r
  (_, remainingActions) ->
    fail $ "stubCloudT: expected the following unexecuted actions to be run:\n"
         ++ unlines (map (\(action :-> _) -> "  " ++ show action) remainingActions)

stubCloudAction :: Monad m => String -> CloudAction r -> CloudT m r
stubCloudAction fnName action = CloudT $ get >>= \case
  [] -> fail $ "stubCloudT: expected end of program, called " ++ fnName ++ "\n  given action:\n"
            ++ "  " ++ show action ++ "\n"
  (action' :-> r) : actions
    | Just Refl <- action `eqAction` action' -> put actions >> return r
    | otherwise -> fail $ "stubCloudT: argument mismatch in " ++ fnName ++ "\n"
                       ++ "  given: " ++ show action ++ "\n"
                       ++ "  expected: " ++ show action' ++ "\n"

instance Monad m => MonadCloud (CloudT m) where
  computeChangeset a b c = stubCloudAction "computeChangeset" (ComputeChangeset a b c)
  getStackOutputs a = stubCloudAction "getStackOutputs" (DescribeStack a)
  runChangeSet a = stubCloudAction "runChangeSet" (RunChangeSet a)

--------------------------------------------------------------------------------
-- Environment

newtype EnvironmentT m a = EnvironmentT (ReaderT [(T.Text, T.Text)] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadError e
           , MonadArguments, MonadLogger, MonadFileSystem e, MonadCloud )

stubEnvironmentT :: [(T.Text, T.Text)] -> EnvironmentT m a -> m a
stubEnvironmentT fs (EnvironmentT x) = runReaderT x fs

instance Monad m => MonadEnvironment (EnvironmentT m) where
  getEnv x = lookup x <$> EnvironmentT ask
