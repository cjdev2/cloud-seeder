module CloudSeeder.CommandLine
    ( Options(..)
    , DescribeOptions(..)
    , Command(..)
    , DeployOptions(..)
    , textOption
    , parameterOption
    , describeOptions
    , deployOptions
    , cli
    ) where

import Data.Semigroup ((<>))
import Options.Applicative (Mod, OptionFields, Parser, strOption, long, metavar, help, info, progDesc, subparser, command, option, auto, short, many)
import qualified Data.Text as T

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
