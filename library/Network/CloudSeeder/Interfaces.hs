{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.Interfaces
  ( MonadCLI(..)
  , getArgs'
  , getOptions'
  , whenEnv
  , getEnvArg

  , MonadCloud(..)
  , computeChangeset'
  , getStackInfo'
  , getStackOutputs'
  , runChangeSet'

  , MonadEnvironment(..)
  , StackName(..)

  , MonadFileSystem(..)
  , FileSystemError(..)
  , readFile'
  , HasFileSystemError(..)
  , AsFileSystemError(..)
  ) where

import Prelude hiding (readFile)

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Lens (Traversal', (.~), (^.), (^?), (?~), _Just, only, to)
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Control.Monad (void, unless, when)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.AWS (runResourceT, runAWST, send)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Writer (WriterT)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Network.AWS (AsError(..), ErrorMessage(..), HasEnv(..), serviceMessage)
import Options.Applicative (execParser)

import qualified Control.Exception.Lens as IO
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.AWS.CloudFormation as AWS
import qualified System.Environment as IO

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Types

newtype StackName = StackName T.Text
  deriving (Eq, Show, Generic, IsString)
instance NFData StackName

--------------------------------------------------------------------------------
-- | A class of monads that can access command-line arguments.

class Monad m => MonadCLI m where
  -- | Returns positional arguments provided to the program while ignoring flags -- separate from getOptions to avoid cyclical dependencies.
  getArgs :: m Command
  default getArgs :: (MonadTrans t, MonadCLI m', m ~ t m') => m Command
  getArgs = lift getArgs

  -- | Returns flags provided to the program while ignoring positional arguments -- separate from getArgs to avoid cyclical dependencies.
  getOptions :: S.Set ParameterSpec -> m (M.Map T.Text ParameterValue)
  default getOptions :: (MonadTrans t, MonadCLI m', m ~ t m') => S.Set ParameterSpec -> m (M.Map T.Text ParameterValue)
  getOptions = lift . getOptions

getArgs' :: MonadBase IO m => m Command
getArgs' = liftBase $ execParser parseArguments

getOptions' :: MonadBase IO m => S.Set ParameterSpec -> m (M.Map T.Text ParameterValue)
getOptions' = liftBase . execParser . parseOptions

instance MonadCLI m => MonadCLI (ExceptT e m)
instance MonadCLI m => MonadCLI (LoggingT m)
instance MonadCLI m => MonadCLI (ReaderT r m)
instance MonadCLI m => MonadCLI (StateT s m)
instance (Monoid s, MonadCLI m) => MonadCLI (WriterT s m)

-- DSL helpers

getEnvArg :: MonadCLI m => m T.Text
getEnvArg = do
  (ProvisionStack _ env) <- getArgs
  return env

whenEnv :: MonadCLI m => T.Text -> m () -> m ()
whenEnv env x = do
  envToProvision <- getEnvArg
  when (envToProvision == env) x

--------------------------------------------------------------------------------
newtype FileSystemError
  = FileNotFound T.Text
  deriving (Eq, Show)

makeClassy ''FileSystemError
makeClassyPrisms ''FileSystemError

-- | A class of monads that can interact with the filesystem.
class (AsFileSystemError e, MonadError e m) => MonadFileSystem e m | m -> e where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  readFile :: T.Text -> m T.Text

  default readFile :: (MonadTrans t, MonadFileSystem e m', m ~ t m') => T.Text -> m T.Text
  readFile = lift . readFile

readFile' :: (AsFileSystemError e, MonadError e m, MonadBase IO m) => T.Text -> m T.Text
readFile' p = do
    let _IOException_NoSuchThing = IO._IOException . to isNoSuchThingIOError
    x <- liftBase $ IO.catching_ _IOException_NoSuchThing (Just <$> T.readFile (T.unpack p)) (return Nothing)
    maybe (throwing _FileNotFound p) return x
  where
    isNoSuchThingIOError IOError { ioe_type = NoSuchThing } = True
    isNoSuchThingIOError _                                  = False

instance MonadFileSystem e m => MonadFileSystem e (ExceptT e m)
instance MonadFileSystem e m => MonadFileSystem e (LoggingT m)
instance MonadFileSystem e m => MonadFileSystem e (ReaderT r m)
instance MonadFileSystem e m => MonadFileSystem e (StateT s m)
instance (MonadFileSystem e m, Monoid w) => MonadFileSystem e (WriterT w m)

--------------------------------------------------------------------------------
-- | A class of monads that can interact with cloud deployments.
class Monad m => MonadCloud m where
  computeChangeset :: StackName -> ProvisionType -> T.Text -> M.Map T.Text ParameterValue -> M.Map T.Text T.Text -> m T.Text
  getStackInfo :: StackName -> m (Maybe Stack)
  getStackOutputs :: StackName -> m (Maybe (M.Map T.Text T.Text))
  runChangeSet :: T.Text -> m ()

  default computeChangeset :: (MonadTrans t, MonadCloud m', m ~ t m') => StackName -> ProvisionType -> T.Text -> M.Map T.Text ParameterValue -> M.Map T.Text T.Text -> m T.Text
  computeChangeset a b c d e = lift $ computeChangeset a b c d e

  default getStackInfo :: (MonadTrans t, MonadCloud m', m ~ t m') => StackName -> m (Maybe Stack)
  getStackInfo = lift . getStackInfo

  default getStackOutputs :: (MonadTrans t, MonadCloud m', m ~ t m') => StackName -> m (Maybe (M.Map T.Text T.Text))
  getStackOutputs = lift . getStackOutputs

  default runChangeSet :: (MonadTrans t, MonadCloud m', m ~ t m') => T.Text -> m ()
  runChangeSet = lift . runChangeSet

type MonadCloudIO r m = (HasEnv r, MonadReader r m, MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadThrow m)

_StackDoesNotExistError :: AsError a => StackName -> Traversal' a ()
_StackDoesNotExistError (StackName stackName) = _ServiceError.serviceMessage._Just.only (ErrorMessage msg)
  where msg = "Stack with id " <> stackName <> " does not exist"

computeChangeset' :: MonadCloudIO r m => StackName -> ProvisionType -> T.Text -> M.Map T.Text ParameterValue -> M.Map T.Text T.Text -> m T.Text
computeChangeset' (StackName stackName) provisionType templateBody params tags = do
    uuid <- liftBase nextRandom
    let changeSetName = "cs-" <> toText uuid -- change set name must begin with a letter

    env <- ask
    runResourceT . runAWST env $ do
      let request = AWS.createChangeSet stackName changeSetName
            & AWS.ccsParameters .~ map awsParam (M.toList params)
            & AWS.ccsTemplateBody ?~ templateBody
            & AWS.ccsCapabilities .~ [AWS.CapabilityIAM]
            & AWS.ccsTags .~ map awsTag (M.toList tags)
            & AWS.ccsChangeSetType ?~ provisionTypeToChangeSetType provisionType
      response <- send request
      maybe (fail "computeChangeset: the impossible happened--createChangeSet did not return a change set id")
            return (response ^. AWS.ccsrsId)
  where
    awsParam (key, Value val) = AWS.parameter
      & AWS.pParameterKey ?~ key
      & AWS.pParameterValue ?~ val
    awsParam (key, UsePreviousValue) = AWS.parameter
      & AWS.pParameterKey ?~ key
      & AWS.pUsePreviousValue ?~ True
    awsTag (key, val) = AWS.tag
      & AWS.tagKey ?~ key
      & AWS.tagValue ?~ val
    provisionTypeToChangeSetType CreateStack = AWS.Create
    provisionTypeToChangeSetType (UpdateStack _) = AWS.Update

getStackInfo' :: MonadCloudIO r m => StackName -> m (Maybe Stack)
getStackInfo' (StackName stackName) = do
  env <- ask
  let request = AWS.describeStacks & AWS.dStackName ?~ stackName
  runResourceT . runAWST env $ do
    response <- IO.trying_ (_StackDoesNotExistError (StackName stackName)) $ send request
    case response ^? _Just.AWS.dsrsStacks of
      Nothing -> return Nothing
      Just [s] -> do
        let awsParams = s ^. AWS.sParameters
        params <- S.fromList <$> mapM awsParameterKey awsParams
        let stack = Stack params
        return $ Just stack
      Just _ -> fail "getStackOutputs: the impossible happened--describeStacks returned more than one stack"
  where
    awsParameterKey :: Monad m => AWS.Parameter -> m T.Text
    awsParameterKey x = case x ^. AWS.pParameterKey of
      (Just k) -> return k
      Nothing -> fail "getStackProvisionType: the impossible happened--stack parameter key was missing"

getStackOutputs' :: MonadCloudIO r m => StackName -> m (Maybe (M.Map T.Text T.Text))
getStackOutputs' (StackName stackName) = do
    env <- ask
    let request = AWS.describeStacks & AWS.dStackName ?~ stackName
    runResourceT . runAWST env $ do
      response <- IO.trying_ (_StackDoesNotExistError (StackName stackName)) $ send request
      case response ^? _Just.AWS.dsrsStacks of
        Nothing -> return Nothing
        Just [stack] -> Just . M.fromList <$> mapM outputToTuple (stack ^. AWS.sOutputs)
        Just _ -> fail "getStackOutputs: the impossible happened--describeStacks returned more than one stack"
  where
    outputToTuple :: Monad m => AWS.Output -> m (T.Text, T.Text)
    outputToTuple x = case (x ^. AWS.oOutputKey, x ^. AWS.oOutputValue) of
      (Just k, Just v) -> return (k, v)
      (Nothing, _) -> fail "getStackOutputs: the impossible happened--stack output key was missing"
      (_, Nothing) -> fail "getStackOutputs: the impossible happened--stack output value was missing"

runChangeSet' :: MonadCloudIO r m => T.Text -> m ()
runChangeSet' csId = do
    env <- ask
    waitUntilChangeSetReady env
    runResourceT . runAWST env $
      void $ send (AWS.executeChangeSet csId)
  where
    waitUntilChangeSetReady env = do
      liftBase $ threadDelay 1000000
      cs <- runResourceT . runAWST env $
        send (AWS.describeChangeSet csId)
      execStatus <- case cs ^. AWS.drsExecutionStatus of
        Just x -> return x
        Nothing -> fail "runChangeSet: the impossible happened--change set lacks execution status"
      unless (execStatus == AWS.Available) $ void $ waitUntilChangeSetReady env

instance MonadCloud m => MonadCloud (ExceptT e m)
instance MonadCloud m => MonadCloud (LoggingT m)
instance MonadCloud m => MonadCloud (ReaderT r m)
instance MonadCloud m => MonadCloud (StateT s m)
instance (MonadCloud m, Monoid w) => MonadCloud (WriterT w m)

--------------------------------------------------------------------------------
-- | A class of monads that can access environment variables
class Monad m => MonadEnvironment m where
  getEnv :: T.Text -> m (Maybe T.Text)

  default getEnv :: (MonadTrans t, MonadEnvironment m', m ~ t m') => T.Text -> m (Maybe T.Text)
  getEnv = lift . getEnv

instance MonadEnvironment IO where
  getEnv = fmap (fmap T.pack) . IO.lookupEnv . T.unpack

instance MonadEnvironment m => MonadEnvironment (ExceptT e m)
instance MonadEnvironment m => MonadEnvironment (LoggingT m)
instance MonadEnvironment m => MonadEnvironment (ReaderT r m)
instance MonadEnvironment m => MonadEnvironment (StateT s m)
instance (MonadEnvironment m, Monoid w) => MonadEnvironment (WriterT w m)
