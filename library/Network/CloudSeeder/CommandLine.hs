module Network.CloudSeeder.CommandLine
    ( Command(..)
    , Optional(..)
    , defaultDeployStack
    , parseCommandWithInfo
    , withInfo
    ) where

import Data.Monoid ((<>))
import Options.Applicative (ArgumentFields, OptionFields, Parser, ParserInfo, Mod, subparser, strArgument, strOption, command, info, progDesc, many, metavar, helper, long, short)
import qualified Data.Text as T

data Command = DeployStack 
 { commandStack :: T.Text
 , commandEnv :: T.Text
 , commandOptionals :: [Optional] 
 } deriving (Eq, Show)

data Optional = Optional T.Text T.Text
  deriving (Eq, Show)

defaultDeployStack :: Command
defaultDeployStack = DeployStack 
  { commandStack = ""
  , commandEnv = ""
  , commandOptionals = [] }

-- helpers --

textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- parsers --

parseCommandWithInfo :: ParserInfo Command
parseCommandWithInfo = parseCommand `withInfo` "Interact with the CloudFormation API"

parseCommand :: Parser Command
parseCommand = subparser $ command "deploy" (parseDeploy `withInfo` "Deploy a stack to ENV")

parseDeploy :: Parser Command
parseDeploy = DeployStack <$> parseStack <*> parseEnv <*> many parseOptional

parseStack :: Parser T.Text
parseStack = textArgument (metavar "STACK")

parseEnv :: Parser T.Text
parseEnv = textArgument (metavar "ENV")

parseOptional :: Parser Optional
parseOptional = Optional 
  <$> textOption (long "optional-key" <> short 'k' <> metavar "OPTIONAL_KEY") 
  <*> textOption (long "optional-val" <> short 'v' <> metavar "OPTIONAL_VALUE")
