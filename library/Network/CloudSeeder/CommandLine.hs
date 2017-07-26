{-|
This module implements command-line parsing for cloud-seeder. The core
command-line interface, from the user’s point of view, is simple: there are a
set of subcommands (such as “provision”) that take various positional arguments
and flags. Internally, however, the parsing is a little more complex, and this
is because we want to use the user’s deployment configuration to /generate/ a
set of options that may be supplied.

For example, if the user specifies the following configuration:

@
'Network.CloudSeeder.DSL.deployment' "foo" $ do
  'Network.CloudSeeder.DSL.flag' "SomeParam"
@

…then we want to generate a @--SomeParam=[PARAM]@ option. Not only that, we want
to make it optional or required based on whether or not there is a default value
or not.

To accomplish this, we need to collect all the flags from the DSL, /then/ run
the command-line parser. If we do that, though, we have a new problem! The DSL
might need access to the environment, which is a positional argument. This is a
circular dependency: we need to parse the environment from the command line in
order to execute the DSL, but we need to execute the DSL in order to know which
flags to parse from the command line.

To solve this problem, we parse arguments in two phases: first, we parse
positional arguments and accept /any/ options (and ignore them). Once we’ve used
the information in the positional arguments to evaluate the DSL, we parse the
arguments a second time, enhanced with more information.
-}
module Network.CloudSeeder.CommandLine
    ( Command(..)
    , ParameterSpec(..)
    , parseArguments
    , parseOptions
    ) where

import Control.Lens ((^.))
import Data.Monoid ((<>))
import Options.Applicative

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Network.CloudSeeder.Types

data Command
  -- | @'ProvisionStack' "stack" "env"@
  = ProvisionStack T.Text T.Text
  deriving (Eq, Show)

-- | A parser that corresponds to the first “parsing phase” for the @provision@
-- subcommand, as described in the module documentation for
-- 'Network.CloudSeeder.CommandLine'.
parseArguments :: ParserInfo Command
parseArguments = program PhaseArguments

-- | A parser that corresponds to the second “parsing phase” for the @provision@
-- subcommand, as described in the module documentation for
-- 'Network.CloudSeeder.CommandLine'.
parseOptions :: S.Set ParameterSpec -> ParserInfo (M.Map T.Text T.Text)
parseOptions = program . PhaseOptions

program :: ParsingPhase r -> ParserInfo r
program phase = info (helper <*> provision phase)
  (fullDesc <> progDesc "Manage stacks in CloudFormation")

-- | Represents the current “parsing phase”, as described in the module
-- documentation for 'Network.CloudSeeder.CommandLine'. Used to parameterize
-- the 'provision' parser.
data ParsingPhase r where
  PhaseArguments :: ParsingPhase Command
  PhaseOptions :: S.Set ParameterSpec -> ParsingPhase (M.Map T.Text T.Text)

-- | Parser for the 'provision' subcommand, which parses a 'ProvisionStack'
-- value if it succeeds.
--
-- This parsers has two “phases” of parsing, as noted in the module
-- documentation for 'Network.CloudSeeder.CommandLine'. This is reflected in the
-- first argument, which also controls the result of the parser.
provision :: ParsingPhase r -> Parser r
provision phase = subparser . command "provision" $ info parser infoMod
  where
    -- When parsing arguments, we want to ignore options. Using 'forwardOptions'
    -- treats them as positional arguments rather than outright ignoring them,
    -- but that’s good enough for our purposes.
    infoMod = progDesc "Provision a stack in an environment" <> case phase of
      PhaseArguments -> forwardOptions
      PhaseOptions _ -> mempty

    parser = case phase of
        PhaseArguments -> commandParser <* ignoreArguments
        PhaseOptions specs -> helper <*> (commandParser *> optionsParser specs)
      where
        ignoreArguments = many $ strArgument @String hidden

    commandParser :: Parser Command
    commandParser = ProvisionStack <$> helper' stack <*> helper' env
      where
        stack = textArgument (metavar "STACK")
        env = textArgument (metavar "ENV")
        -- This is like 'helper', but slightly modified to avoid over-eagerly
        -- failing. This carefully handles “deferring” showing the help text to
        -- 'PhaseOptions' if both STACK and ENV are supplied.
        helper' p = (abortOption ShowHelpText opts <*> empty) <|> p
          where opts = long "help" <> short 'h' <> internal

    optionsParser :: S.Set ParameterSpec -> Parser (M.Map T.Text T.Text)
    optionsParser specs = M.fromList <$> traverse parameter (S.toList specs)
      where
        parameter spec = do
          let key = spec ^. parameterKey
              keyStr = T.unpack key
          val <- textOption (long keyStr <> metavar keyStr <> defaultMod spec)
          pure (key, val)

        defaultMod (Required _) = mempty
        defaultMod (Optional _ defVal) = value (T.unpack defVal)

-- helpers --
textArgument :: Mod ArgumentFields String -> Parser T.Text
textArgument = fmap T.pack . strArgument

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption
