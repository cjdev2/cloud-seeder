module Network.CloudSeeder.CommandLine
    ( Command(..)
    , ParameterSpec(..)
    , parseArguments
    , parseOptions
    , withInfo
    ) where

import Data.Monoid ((<>))
import Options.Applicative 

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Network.CloudSeeder.Types

--                         stack  env
data Command = DeployStack T.Text T.Text
  deriving (Eq, Show)

-- helpers --

textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (helper <*> parser) $ progDesc desc

-- parsers --

parseOptions :: S.Set (T.Text, ParameterSource) -> ParserInfo (M.Map T.Text T.Text)
parseOptions ps = info (helper <*> parseCommand *> parseParameters ps)
  ( fullDesc 
 <> progDesc "Interact with the CloudFormation API"
 <> header "Cloud-Seeder -- a tool for interacting with the AWS CloudFormation API"
  )

parseParameters :: S.Set (T.Text, ParameterSource) -> Parser (M.Map T.Text T.Text)
parseParameters ps = M.fromList <$> traverse (parseParameter . fst) (S.toList ps)

parseParameter :: T.Text -> Parser (T.Text, T.Text)
parseParameter key = do 
  let k = T.unpack key
  val <- textOption (long k <> metavar k <> noArgError (ErrorMsg $ "Required flag not provided: " <> k))
  pure (key, val)

parseArguments :: ParserInfo Command
parseArguments = info (helper <*> parseCommand) 
  ( fullDesc 
 <> progDesc "Interact with the CloudFormation API"
 <> header "Cloud-Seeder -- a tool for interacting with the AWS CloudFormation API"
  )

parseCommand :: Parser Command
parseCommand = subparser $ command "deploy" (parseDeploy `withInfo` "Deploy a stack to ENV")

parseDeploy :: Parser Command
parseDeploy = DeployStack <$> parseStack <*> parseEnv <* many parseAnyOption

parseStack :: Parser T.Text
parseStack = textArgument (metavar "STACK")

parseEnv :: Parser T.Text
parseEnv = textArgument (metavar "ENV")

parseAnyOption :: Parser String
parseAnyOption = strArgument hidden
