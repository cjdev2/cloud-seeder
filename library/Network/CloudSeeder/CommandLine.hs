module Network.CloudSeeder.CommandLine
    ( Command(..)
    , commandParser
    ) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, Mod, OptionFields, subparser, command, info, progDesc, strOption, long, metavar, help, short)
import qualified Data.Text as T

data Command = DeployStack T.Text
  deriving (Eq, Show)

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

commandParser :: Parser Command
commandParser = subparser $ command "deploy" (info (DeployStack <$> stackName) (progDesc "deploy a stack"))
  where
    stackName :: Parser T.Text
    stackName = textOption
       ( long "stack"
      <> short 's'
      <> metavar "STACK"
      <> help "the name of the stack in the configuration")
