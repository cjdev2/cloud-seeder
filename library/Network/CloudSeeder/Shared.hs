{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.Shared
  ( parseArgs
  , getEnvArg
  , whenEnv
  , logStack
  , getStack
  , mkFullStackName
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger, logInfo)
import Data.Semigroup ((<>))
import Options.Applicative (ParserPrefs(..), ParserResult(..), execParserPure, renderFailure)

import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Types

import qualified Network.CloudSeeder.CommandLine as CL
import qualified Data.Text as T

parseArgs :: (AsCliError e, MonadError e m) => [String] -> m CL.Command
parseArgs args = do
  let prefs = ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = False
        , prefColumns = 80
        }
  case execParserPure prefs CL.parseArguments args of
    Success a -> pure a
    Failure f -> throwing _CliParseFailure (T.pack . fst $ renderFailure f "cloud-seeder")
    CompletionInvoked _ -> error "internal error--bash completion invoked; bash completions not supported"

getEnvArg :: (AsCliError e, MonadError e m, MonadCLI m) => m T.Text
getEnvArg = do
  args <- getArgs
  (CL.ProvisionStack _ env) <- parseArgs args
  pure env

whenEnv :: (AsCliError e, MonadError e m, MonadCLI m) => T.Text -> m () -> m ()
whenEnv env x = do
  envToProvision <- getEnvArg
  when (envToProvision == env) x

logStack :: MonadLogger m => Stack -> m ()
logStack stackInfo = $(logInfo) (render stackInfo)
  where
    render :: Stack -> T.Text
    render s = T.unlines
      [ "Stack Info:"
      , "  name: " <> s^.name
      , "  status: " <> T.pack (show (s^.stackStatus))
      , "  outputs: " <> T.pack (show (s^.outputs))
      ]

getStack :: (AsCliError e, MonadCloud e m) => StackName -> m Stack
getStack stackName = do
  maybeStack <- describeStack stackName
  let (StackName s) = stackName
  maybe (throwing _CliStackDoesNotExist s) pure maybeStack

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName
