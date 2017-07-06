module Network.CloudSeeder.CommandLine
    ( Command(..)
    , Optional(..)
    , defaultDeployStack
    , parseCommand
    , withInfo
    ) where

import Data.Monoid ((<>))
import Options.Applicative (ArgumentFields, OptionFields, Parser, ParserInfo, Mod, subparser, strArgument, strOption, command, info, progDesc, many, metavar, helper, long, short)
import qualified Data.Text as T

data Command = DeployStack 
 { commandStack :: T.Text
 , commandEnv :: T.Text
 , commandOptionals :: [Optional] 
 }

data Optional = Optional T.Text T.Text

defaultDeployStack :: Command
defaultDeployStack = DeployStack 
  { commandStack = ""
  , commandEnv = ""
  , commandOptionals = [] }

textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptional :: Parser Optional
parseOptional = Optional <$> textOption (long "optional-key" <> short 'k') <*> textOption (long "optional-val" <> short 'v')

parseDeploy :: Parser Command
parseDeploy = DeployStack <$> parseStack <*> parseEnv <*> many parseOptional

parseCommand :: Parser Command
parseCommand = subparser $ command "deploy" (parseDeploy `withInfo` "Deploy a stack to ENV")

parseStack :: Parser T.Text
parseStack = textArgument (metavar "STACK")

parseEnv :: Parser T.Text
parseEnv = textArgument (metavar "ENV")
