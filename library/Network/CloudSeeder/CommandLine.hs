module Network.CloudSeeder.CommandLine
    ( Command(..)
    , parseCommand
    , withInfo
    ) where

import Options.Applicative (ArgumentFields, Parser, ParserInfo, Mod, subparser, strArgument, command, info, progDesc, metavar, helper)
import qualified Data.Text as T

data Command = DeployStack T.Text T.Text

textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseDeploy :: Parser Command
parseDeploy = DeployStack <$> parseStack <*> parseEnv

parseCommand :: Parser Command
parseCommand = subparser $ command "deploy" (parseDeploy `withInfo` "Deploy a stack to ENV")

parseStack :: Parser T.Text
parseStack = textArgument (metavar "STACK")

parseEnv :: Parser T.Text
parseEnv = textArgument (metavar "ENV")
