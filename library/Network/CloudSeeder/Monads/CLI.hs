module Network.CloudSeeder.Monads.CLI
  ( MonadCli(..)
  , getArgs'
  ) where

import Prelude hiding (readFile)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Except (ExceptT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (WriterT)

import qualified System.Environment as IO

-- | A class of monads that can access command-line arguments.
class Monad m => MonadCli m where
  getArgs :: m [String]
  default getArgs :: (MonadTrans t, MonadCli m', m ~ t m') => m [String]
  getArgs = lift getArgs

getArgs' :: MonadBase IO m => m [String]
getArgs' = liftBase IO.getArgs

instance MonadCli m => MonadCli (ExceptT e m)
instance MonadCli m => MonadCli (LoggingT m)
instance MonadCli m => MonadCli (ReaderT r m)
instance MonadCli m => MonadCli (StateT s m)
instance (Monoid s, MonadCli m) => MonadCli (WriterT s m)
