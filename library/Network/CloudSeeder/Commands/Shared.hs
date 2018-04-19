module Network.CloudSeeder.Commands.Shared
  ( parseArgs
  , parseOpts
  , getEnvArg
  , whenEnv
  , toYamlText
  , getStack
  , mkFullStackName
  , waitOnStack
  , getStackFromConfig
  ) where

import Control.Lens ((^.), has, only, to)
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Data.Yaml (ToJSON)
import Data.List (find)
import Data.Semigroup ((<>))
import Data.Text.Conversions (UTF8(..), decodeConvertText)
import Data.Yaml.Pretty (defConfig, encodePretty)
import Options.Applicative (ParserPrefs(..), ParserResult(..), execParserPure, renderFailure)

import Network.CloudSeeder.Monads.AWS
import Network.CloudSeeder.Monads.CLI
import Network.CloudSeeder.Error
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Types

import qualified Network.CloudSeeder.CommandLine as CL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Network.AWS.CloudFormation as CF

parseOpts :: (AsCliError e, MonadError e m) => S.Set ParameterSpec -> [String] -> m CL.Options
parseOpts pSpecs opts = do
  let prefs = ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = False
        , prefColumns = 80
        }
  case execParserPure prefs (CL.parseOptions pSpecs) opts of
    Success a -> pure a
    Failure f -> throwing _CliParseFailure (T.pack . fst $ renderFailure f "cloud-seeder")
    CompletionInvoked _ -> error "internal error--bash completion invoked; bash completions not supported"

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

getEnvArg :: (AsCliError e, MonadError e m, MonadCli m) => m T.Text
getEnvArg = do
  args <- getArgs
  command <- parseArgs args
  let env = case command of
        CL.ProvisionStack _ x -> x
        CL.Wait _ x -> x
        CL.TeardownStack _ x -> x
  pure env

whenEnv :: (AsCliError e, MonadError e m, MonadCli m) => T.Text -> m () -> m ()
whenEnv env x = do
  envToProvision <- getEnvArg
  when (envToProvision == env) x

toYamlText
  :: (AsCliError e, MonadError e m, ToJSON a)
  => a -> T.Text -> m T.Text
toYamlText x xName = do
  let bs = encodePretty defConfig x
  maybe
    (throwing _CliCloudError (CloudErrorInternal $ xName <> " failed to decode to valid ASCII"))
    -- TODO should this be a CloudError?
    pure
    (decodeConvertText (UTF8 (bs :: BS.ByteString)) :: Maybe T.Text)

getStack :: (AsCliError e, MonadCloud e m) => StackName -> m Stack
getStack stackName = do
  maybeStack <- describeStack stackName
  let (StackName s) = stackName
  maybe (throwing _CliStackDoesNotExist s) pure maybeStack

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName

waitOnStack :: (AsCliError e, MonadCloud e m) => StackName -> m ()
waitOnStack stackName = do
  thisStack <- getStack stackName
  doWait thisStack
  where
    doWait :: (AsCliError e, MonadCloud e m) => Stack -> m ()
    doWait thisStack = do
      let thisStackStatus = thisStack ^. stackStatus
      case thisStackStatus of
        CF.SSCreateComplete -> wait StackCreateComplete stackName
        CF.SSCreateFailed -> wait StackCreateComplete stackName
        CF.SSCreateInProgress -> wait StackCreateComplete stackName
        CF.SSDeleteComplete -> wait StackDeleteComplete stackName
        CF.SSDeleteFailed -> wait StackDeleteComplete stackName
        CF.SSDeleteInProgress -> wait StackDeleteComplete stackName
        CF.SSRollbackComplete -> wait StackUpdateComplete stackName
        CF.SSRollbackFailed -> wait StackUpdateComplete stackName
        CF.SSRollbackInProgress -> wait StackUpdateComplete stackName
        CF.SSUpdateComplete -> wait StackUpdateComplete stackName
        CF.SSUpdateCompleteCleanupInProgress -> wait StackUpdateComplete stackName
        CF.SSUpdateInProgress -> wait StackUpdateComplete stackName
        CF.SSUpdateRollbackComplete -> wait StackUpdateComplete stackName
        CF.SSUpdateRollbackCompleteCleanupInProgress -> wait StackUpdateComplete stackName
        CF.SSUpdateRollbackFailed -> wait StackUpdateComplete stackName
        CF.SSUpdateRollbackInProgress -> wait StackUpdateComplete stackName
        CF.SSReviewInProgress -> throwing _CliStackNeedsChangeSetReview s
          where (StackName s) = stackName

getStackFromConfig
  :: (AsCliError e, MonadError e m)
  => DeploymentConfiguration m -> T.Text -> m (StackConfiguration m)
getStackFromConfig config targetStackName = do
  let maybeStackToProvision = config ^. stacks.to (find (has (name.only targetStackName)))
  maybe (throwing _CliStackNotConfigured targetStackName) return maybeStackToProvision
