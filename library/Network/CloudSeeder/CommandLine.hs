module Network.CloudSeeder.CommandLine
    ( Command(..)
    , ParameterSpec(..)
    , parseArguments
    , parseOptions
    , withInfo
    ) where

import Control.Lens ((^.))
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

-- parseOptions --
parseOptions :: S.Set ParameterSpec -> ParserInfo (M.Map T.Text T.Text)
parseOptions ps = info (helper <*> parseCommand *> parseParameters ps)
  ( fullDesc
 <> progDesc "Interact with the CloudFormation API"
 <> header "Cloud-Seeder -- a tool for interacting with the AWS CloudFormation API"
  )

parseParameters :: S.Set ParameterSpec -> Parser (M.Map T.Text T.Text)
parseParameters ps = M.fromList <$> traverse parseParameter (S.toList ps)

parseParameter :: ParameterSpec -> Parser (T.Text, T.Text)
parseParameter pSpec = do
  let key = pSpec ^. parameterKey
      k   = T.unpack key
  val <- case pSpec of
    Required _ -> textOption (long k <> metavar k <> noArgError (ErrorMsg $ "Required flag not provided: " <> k))
    Optional _ defVal -> textOption (long k <> metavar k <> value (T.unpack defVal))
  pure (key, val)

-- parseArguments --
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
