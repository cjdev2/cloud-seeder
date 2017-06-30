module Network.CloudSeeder.CommandLine
    ( Options(..)
    , Command(..)
    , withInfo
    , parseOptions
    ) where

import Options.Applicative (ArgumentFields, Parser, ParserInfo, Mod, subparser, strArgument, command, info, progDesc, metavar, helper)
import qualified Data.Text as T

data Command
  = DeployStack

data Options = Options Command T.Text T.Text

textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseDeploy :: Parser Command
parseDeploy = pure DeployStack

parseCommand :: Parser Command
parseCommand = subparser $ command "deploy" (parseDeploy `withInfo` "Deploy a stack to ENV")

parseStack :: Parser T.Text
parseStack = textArgument (metavar "STACK")

parseEnv :: Parser T.Text
parseEnv = textArgument (metavar "ENV")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand <*> parseStack <*> parseEnv