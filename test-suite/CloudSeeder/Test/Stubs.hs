module CloudSeeder.Test.Stubs where

import Control.DeepSeq (NFData(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.State (StateT(..), get, put)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger(..))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Log.FastLogger (fromLogStr, toLogStr)

import CloudSeeder.Core

--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT Options m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadLogger, MonadDeploy, MonadFileSystem )

-- | Runs a computation with access to a set of command-line arguments.
runArgumentsT :: Options -> ArgumentsT m a -> m a
runArgumentsT args (ArgumentsT x) = runReaderT x args

instance Monad m => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT ask

--------------------------------------------------------------------------------
-- Logger

newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadArguments, MonadDeploy, MonadFileSystem )

-- | Runs a computation that may emit log messages, returning the result of the
-- computation combined with the set of messages logged, in order.
runLoggerT :: LoggerT m a -> m (a, [ByteString])
runLoggerT (LoggerT x) = runWriterT x

instance Monad m => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- Deploy

data Stack = Stack
  { sStackName :: StackName
  , sTemplateBody :: T.Text
  , sParameters :: [(T.Text, T.Text)]
  } deriving (Eq, Show, Generic)
instance NFData Stack

data NoStack = NoStack
  deriving (Eq, Show, Generic)
instance NFData NoStack

newtype DeployState = DeployState [(StackName, Either NoStack Stack)]
  deriving (Eq, Show, Generic)
instance NFData DeployState

newtype DeployT m a = DeployT (StateT DeployState m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadLogger, MonadArguments, MonadFileSystem )

-- | Runs a computation with a map representing deployed applications and their states
runDeployT :: Monad m => [(StackName, Either NoStack Stack)] -> DeployT m a -> m (a, DeployState)
runDeployT stackMap (DeployT x) = runStateT x (DeployState stackMap)

instance Monad m => MonadDeploy (DeployT m) where
  describeStack stackName = DeployT $ do
    (DeployState stackMap) <- get
    return $ case lookup stackName stackMap of
      (Just _) -> Right $ "stack " <> name <> " deployed"
        where (StackName name) = stackName
      Nothing -> do
        let (StackName name) = stackName
        Left $ "no stack exists with name: " <> name

  deployStack stackName templateBody parameters = DeployT $ do
    (DeployState stackMap) <- get
    put $ DeployState (stackMap <> [(stackName, Right $ Stack stackName templateBody parameters)])
    return $ Right "stackId 123"

--------------------------------------------------------------------------------
-- File System

newtype FileSystemT m a = FileSystemT (ReaderT [(T.Text, T.Text)] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadArguments, MonadLogger, MonadDeploy )

-- | Runs a computation that may interact with the file system, given a mapping
-- from file paths to file contents.
runFileSystemT :: [(T.Text, T.Text)] -> FileSystemT m a -> m a
runFileSystemT fs (FileSystemT x) = runReaderT x fs

instance Monad m => MonadFileSystem (FileSystemT m) where
  readFile path = FileSystemT $ ask >>= \files ->
    maybe (fail $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
          return (lookup path files)
