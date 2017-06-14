{-# LANGUAGE OverloadedStrings #-}

module CloudSeeder.Core
  ( mainIO
  , main
  , MonadArguments(..)
  , MonadDeploy(..)
  , Opts(..)
  , StackName(..)
  ) where

import Control.Lens ((<&>), set, (^.))
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT, logInfoN)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.AWS (newEnv, runResourceT, runAWST, within, Region(..), LogLevel(..), newLogger, envLogger, send)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Network.AWS (Credentials(..))
import Network.AWS.CloudFormation (dStackName, dsrsStacks, describeStacks)
import Options.Applicative (Mod, OptionFields, Parser, strOption, long, metavar, help, execParser, info, (<**>), helper, fullDesc, progDesc, header)
import qualified Data.Text as T
import System.IO (stdout)

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadLogger, MonadDeploy, MonadArguments )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

mainIO :: IO ()
mainIO = runAppM main

--------------------------------------------------------------------------------
-- Command Line Parsing

data Opts = Opts
  { _stackName :: T.Text
  }

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

cli :: Parser Opts
cli = Opts
      <$> textOption
          ( long "stack-name"
         <> metavar "STACK_NAME"
         <> help "name of the extant stack to be described" )

--------------------------------------------------------------------------------
-- Logic

main :: (MonadDeploy m, MonadLogger m, MonadArguments m) => m ()
-- main :: IO ()
main = do
  (Opts stackName) <- getArgs
  result <- describeStack (StackName stackName)
  either logInfoN logInfoN result

newtype StackName = StackName T.Text deriving (Eq, Show)
newtype Parameters = Parameters (M.Map T.Text T.Text) deriving (Eq, Show)
newtype TemplatePath = TemplatePath FilePath deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Interfaces

-- | A class of monads that encapsulate deployment semantics
class Monad m => MonadDeploy m where
  -- | deploys a stack and returns an AWS CloudFormation StackId or an Error
  deployStack :: StackName -> TemplatePath -> Parameters -> m (Either T.Text T.Text)

  default deployStack :: (MonadTrans t, MonadDeploy m', m ~ t m')
                 => StackName -> TemplatePath -> Parameters
                 -> m (Either T.Text T.Text)
  deployStack sn tp p = lift $ deployStack sn tp p

  describeStack:: StackName -> m (Either T.Text T.Text)

  default describeStack:: (MonadTrans t, MonadDeploy m', m ~ t m')
                   => StackName -> m (Either T.Text T.Text)
  describeStack= lift . describeStack

instance MonadDeploy IO where
  deployStack stackName templatePath params = do
    return $ Right "test"

  describeStack(StackName stackName) = do
    logger <- newLogger Info stdout
    env <- newEnv Discover <&> set envLogger logger
    let request = set dStackName (Just stackName) describeStacks
    fmap return $ runResourceT . runAWST env . within NorthVirginia $ do
      response <- send request
      let stacks = response ^. dsrsStacks
      return $ T.pack $ show stacks

instance MonadDeploy m => MonadDeploy (LoggingT m)
instance MonadDeploy m => MonadDeploy (ReaderT r m)
instance MonadDeploy m => MonadDeploy (StateT s m)
instance (Monoid s, MonadDeploy m) => MonadDeploy (WriterT s m)

-- | A class of monads that can access command-line arguments.
class Monad m => MonadArguments m where
  -- | Returns the command-line arguments provided to the program.
  getArgs :: m Opts

  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m Opts
  getArgs = lift getArgs

instance MonadArguments m => MonadArguments (LoggingT m)
instance MonadArguments m => MonadArguments (ReaderT r m)
instance MonadArguments m => MonadArguments (StateT s m)
instance (Monoid s, MonadArguments m) => MonadArguments (WriterT s m)

instance MonadArguments IO where
  getArgs = execParser opts
    where
      opts = info (cli <**> helper)
        (  fullDesc
        <> progDesc "describe stack named STACK_NAME"
        <> header "AWS CloudFormation Describe Stack"
        )
