{-# LANGUAGE OverloadedStrings #-}

module CloudSeeder.Core
  ( Command(..)
  , DescribeOptions(..)
  , DeployOptions(..)
  , mainIO
  , main
  , MonadArguments(..)
  , MonadDeploy(..)
  , MonadFileSystem(..)
  , Options(..)
  , StackName(..)
  ) where

import Control.Lens (set, (<&>), (.~), (^.), (?~))
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT, logInfoN)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.AWS (newEnv, runResourceT, runAWST, within, Region(..), LogLevel(..), newLogger, envLogger, send)
import Control.Monad.Writer (WriterT)
import Control.DeepSeq (NFData)
import Data.Function ((&))
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Network.AWS (Credentials(..))
import Network.AWS.CloudFormation (dStackName, dsrsStacks, describeStacks, createStack, csrsStackId, csCapabilities, csTemplateBody, csParameters)
import Network.AWS.CloudFormation.Types (Capability(..), parameter, pParameterKey, pParameterValue)
import Options.Applicative (Mod, OptionFields, Parser, strOption, long, metavar, help, execParser, info, (<**>), helper, fullDesc, progDesc, header, subparser, command, option, auto, short, many)
import System.IO (stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (readFile)

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadLogger, MonadDeploy, MonadArguments, MonadFileSystem )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

mainIO :: IO ()
mainIO = runAppM main

--------------------------------------------------------------------------------
-- Command Line Parsing

data Options = Options
  { optCommand :: Command
  } deriving (Show, Eq)

data Command
  = DescribeStack DescribeOptions
  | DeployStack DeployOptions
  deriving (Show, Eq)

data DescribeOptions = DescribeOptions
  { desStackName :: T.Text
  } deriving (Show, Eq)

data DeployOptions = DeployOptions
  { depStackName :: T.Text
  , depTemplatePath :: T.Text
  , depParameters :: [(T.Text, T.Text)]
  } deriving (Show, Eq)

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

parameterOption :: Parser (T.Text, T.Text)
parameterOption = option auto
  ( long "parameter"
 <> short 'p'
 <> help "parameters to pass into a deploying stack"
  )

describeOptions :: Parser Options
describeOptions = Options . DescribeStack . DescribeOptions
  <$> textOption
      ( long "stack-name"
     <> metavar "STACK_NAME"
     <> help "name of the extant stack to be described" )

deployOptions :: Parser DeployOptions
deployOptions = DeployOptions
  <$> textOption
      ( long "stack-name"
     <> metavar "STACK_NAME"
     <> help "name of the extant stack to be described" )
  <*> textOption
      ( long "template-path"
     <> metavar "TEMPLATE_PATH"
     <> help "file path to template" )
  <*> many parameterOption

cli :: Parser Options
cli = subparser
   ( command "describe" (info describeOptions (progDesc "get info about a running stack"))
  <> command "deploy" (info (Options . DeployStack <$> deployOptions) (progDesc "deploy a stack"))
   )

--------------------------------------------------------------------------------
-- Logic

runCmd :: (MonadFileSystem m, MonadDeploy m, MonadLogger m, MonadArguments m) => Command -> m (Either T.Text T.Text)
runCmd opts = case opts of
  DescribeStack x -> describeStack (StackName $ desStackName x)
  DeployStack x -> do
      templateBody <- readFile (depTemplatePath x)
      deployStack (StackName $ depStackName x) templateBody (depParameters x)

main :: (MonadFileSystem m, MonadDeploy m, MonadLogger m, MonadArguments m) => m ()
-- main :: IO ()
main = do
  (Options opts) <- getArgs
  -- logInfoN $ "opts: " <> T.pack (show opts)
  result <- runCmd opts
  either logInfoN logInfoN result

--------------------------------------------------------------------------------
-- Interfaces

newtype StackName = StackName T.Text deriving (Eq, Show, Generic)

instance NFData StackName

-- | A class of monads that encapsulate deployment semantics
class Monad m => MonadDeploy m where
  -- | deploys a stack and returns an AWS CloudFormation StackId or an Error
  deployStack :: StackName -> T.Text -> [(T.Text, T.Text)] -> m (Either T.Text T.Text)

  default deployStack :: (MonadTrans t, MonadDeploy m', m ~ t m')
                 => StackName -> T.Text -> [(T.Text, T.Text)]
                 -> m (Either T.Text T.Text)
  deployStack sn tp p = lift $ deployStack sn tp p

  describeStack:: StackName -> m (Either T.Text T.Text)

  default describeStack:: (MonadTrans t, MonadDeploy m', m ~ t m')
                   => StackName -> m (Either T.Text T.Text)
  describeStack= lift . describeStack

instance MonadDeploy IO where
  -- use change sets to...
  -- if stack exists, update it with template,
  -- otherwise create new stack
  deployStack (StackName stackName) templateBody parameters = do
    logger <- newLogger Info stdout
    env <- newEnv Discover <&> envLogger .~ logger
    let awsParams (key, val) = parameter
          & pParameterKey ?~ key
          & pParameterValue ?~ val
    let request = createStack stackName
          & csCapabilities .~ [CapabilityIAM]
          & csTemplateBody ?~ templateBody
          & csParameters .~ (awsParams <$> parameters)
    fmap return $ runResourceT . runAWST env . within NorthVirginia $ do
      response <- send request
      let stacks = response ^. csrsStackId
      return $ T.pack $ show stacks

  describeStack (StackName stackName) = do
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
  getArgs :: m Options

  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m Options
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
        <> progDesc "do stuff with the cloudformations"
        <> header "im a header"
        )

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  readFile :: T.Text -> m T.Text

  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m') => T.Text -> m T.Text
  readFile = lift . readFile

instance MonadFileSystem m => MonadFileSystem (LoggingT m)
instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT s m)
instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

instance MonadFileSystem IO where
  readFile = T.readFile . T.unpack
