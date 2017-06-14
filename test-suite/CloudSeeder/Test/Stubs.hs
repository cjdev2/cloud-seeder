module CloudSeeder.Test.Stubs where

import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.State (StateT(..), evalStateT, get)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Logger (MonadLogger(..))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Log.FastLogger (fromLogStr, toLogStr)

import CloudSeeder.Core

--------------------------------------------------------------------------------
-- Arguments

newtype ArgumentsT m a = ArgumentsT (ReaderT Opts m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadLogger, MonadDeploy )

-- | Runs a computation with access to a set of command-line arguments.
runArgumentsT :: Opts -> ArgumentsT m a -> m a
runArgumentsT args (ArgumentsT x) = runReaderT x args

instance Monad m => MonadArguments (ArgumentsT m) where
  getArgs = ArgumentsT ask

--------------------------------------------------------------------------------
-- Logger

newtype LoggerT m a = LoggerT (WriterT [ByteString] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadArguments, MonadDeploy )

-- | Runs a computation that may emit log messages, returning the result of the
-- computation combined with the set of messages logged, in order.
runLoggerT :: LoggerT m a -> m (a, [ByteString])
runLoggerT (LoggerT x) = runWriterT x

instance Monad m => MonadLogger (LoggerT m) where
  monadLoggerLog _ _ _ str = LoggerT $ tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- Deploy

data DeployState = DeployState [(StackName, T.Text)]
  deriving (Eq, Show)

newtype DeployT m a = DeployT (StateT DeployState m a)
  deriving ( Functor, Applicative, Monad, MonadTrans
           , MonadLogger, MonadArguments )

-- | Runs a computation with a map representing deployed applications and their states
runDeployT :: Monad m => [(StackName, T.Text)] -> DeployT m a -> m a
runDeployT stackMap (DeployT x) = evalStateT x (DeployState stackMap)

instance Monad m => MonadDeploy (DeployT m) where
  describeStack stackName = DeployT $ do
    (DeployState stackMap) <- get
    return $ case lookup stackName stackMap of
      (Just stack) -> Right stack
      Nothing -> do
        let (StackName name) = stackName
        Left $ "no stack exists with name: " <> name

  deployStack = undefined
