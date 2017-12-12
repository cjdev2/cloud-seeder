module Network.CloudSeeder.Shared
  ( parseArgs
  , parseOpts
  , getEnvArg
  , whenEnv
  , logStack
  , logChangeSet
  , getStack
  , mkFullStackName
  , waitOnStack
  ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Semigroup ((<>))
import Options.Applicative (ParserPrefs(..), ParserResult(..), execParserPure, renderFailure)

import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Types

import qualified Network.CloudSeeder.CommandLine as CL
import qualified Data.Text as T
import qualified Data.Map as M
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
  (CL.ProvisionStack _ env) <- parseArgs args
  pure env

whenEnv :: (AsCliError e, MonadError e m, MonadCli m) => T.Text -> m () -> m ()
whenEnv env x = do
  envToProvision <- getEnvArg
  when (envToProvision == env) x

logStack :: MonadLogger m => Stack -> m ()
logStack = logInfoN . render
  where
    render :: Stack -> T.Text
    render s = T.unlines
      [ "Stack Info:"
      , "  name: " <> s^.name
      , "  status: " <> renderStatus (s^.stackStatus)
      , "  outputs: " <> "\n" <> renderOutputs (s^.outputs)
      ]
    renderStatus :: CF.StackStatus -> T.Text
    renderStatus s = case s of
        CF.SSCreateComplete -> "StackCreateComplete"
        CF.SSCreateFailed -> "StackCreateComplete"
        CF.SSCreateInProgress -> "StackCreateComplete"
        CF.SSDeleteComplete -> "StackDeleteComplete"
        CF.SSDeleteFailed -> "StackDeleteComplete"
        CF.SSDeleteInProgress -> "StackDeleteComplete"
        CF.SSRollbackComplete -> "StackUpdateComplete"
        CF.SSRollbackFailed -> "StackUpdateComplete"
        CF.SSRollbackInProgress -> "StackUpdateComplete"
        CF.SSUpdateComplete -> "StackUpdateComplete"
        CF.SSUpdateCompleteCleanupInProgress -> "StackUpdateComplete"
        CF.SSUpdateInProgress -> "StackUpdateComplete"
        CF.SSUpdateRollbackComplete -> "StackUpdateComplete"
        CF.SSUpdateRollbackCompleteCleanupInProgress -> "StackUpdateComplete"
        CF.SSUpdateRollbackFailed -> "StackUpdateComplete"
        CF.SSUpdateRollbackInProgress -> "StackUpdateComplete"
        CF.SSReviewInProgress -> "ReviewInProgress"
    renderOutputs :: M.Map T.Text T.Text -> T.Text
    renderOutputs os = T.unlines (renderOutput <$> M.toList os)
    renderOutput :: (T.Text, T.Text) -> T.Text
    renderOutput (k,v) = "    " <> k <> ": " <> v

getStack :: (AsCliError e, MonadCloud e m) => StackName -> m Stack
getStack stackName = do
  maybeStack <- describeStack stackName
  let (StackName s) = stackName
  maybe (throwing _CliStackDoesNotExist s) pure maybeStack

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName

waitOnStack :: (AsCliError e, MonadCloud e m) => StackName -> m Stack
waitOnStack stackName = do
  thisStack <- getStack stackName
  doWait thisStack
  maybeStackInfo <- describeStack stackName
  maybe
    (throwing _CliCloudError (CloudErrorInternal "stack did not exist after wait"))
    pure
    maybeStackInfo
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

logChangeSet :: MonadLogger m => ChangeSet -> m ()
logChangeSet = logInfoN . render
  where
    render :: ChangeSet -> T.Text
    render cs = T.unlines $ T.stripEnd <$>
      [ "Change Set Info:"
      , "  ID: " <> cs ^. csId
      , "  Status: " <> T.pack (show $ cs ^. executionStatus)
      , "  Status Reason: N/A"
      , "  Parameters: " <> T.unlines (map (T.stripEnd . renderParam) (cs ^. parameters))
      , "  Changes: " <> T.unlines (map (T.stripEnd . renderChange) (cs ^. changes))
      ]

    renderParam :: Parameter -> T.Text
    renderParam (Parameter (key, val)) = case val of
      Value v -> f v
      UsePreviousValue -> f "UsePreviousValue"
      where
        f v = "\n    " <> key <> ": " <> v

    renderChange :: Change -> T.Text
    renderChange (Add c) = T.unlines $ T.stripEnd <$>
      [ "\n    Add: "
      , "      Logical ID: " <> c ^. logicalId
      , "      Physical ID: " <> T.pack (show $ c ^. physicalId)
      , "      Resource Type: " <> c ^. resourceType ]
    renderChange (Remove c) = T.unlines $ T.stripEnd <$>
      [ "\n    Remove: "
      , "      Logical ID: " <> c ^. logicalId
      , "      Physical ID: " <> T.pack (show $ c ^. physicalId)
      , "      Resource Type: " <> c ^. resourceType ]
    renderChange (Modify c) = T.unlines $ T.stripEnd <$>
      [ "\n    Modify: "
      , "      Logical ID: " <> c ^. logicalId
      , "      Physical ID: " <> T.pack (show $ c ^. physicalId)
      , "      Resource Type: " <> c ^. resourceType
      , "      Scope: " <> T.unlines (map renderScope (c ^. scope ))
      , "      Details: " <> T.unlines (map renderDetails (c ^. details))
      , "      Replacement: " <> renderReplacement (c ^. replacement) ]
      where
        renderScope :: CF.ResourceAttribute -> T.Text
        renderScope = T.stripEnd . T.pack . show
        renderDetails :: CF.ResourceChangeDetail -> T.Text
        renderDetails d = T.unlines $ T.stripEnd <$>
          [ "\n        Causing Entity: " <> T.pack (show $ d ^. CF.rcdCausingEntity)
          , "        Change Source: " <> T.pack (show $ d ^. CF.rcdChangeSource)
          , "        Evaluation: " <> T.pack (show $ d ^. CF.rcdEvaluation)
          , "        Target: " <> maybe "Nothing" renderTarget (d ^. CF.rcdTarget)
          ]
        renderReplacement :: CF.Replacement -> T.Text
        renderReplacement = T.stripEnd . T.pack . show
        renderTarget :: CF.ResourceTargetDefinition -> T.Text
        renderTarget t = T.unlines $ (T.stripEnd . T.pack) <$>
          [ "\n          Attribute: " <> show (t ^. CF.rtdAttribute)
          , "          Requires Recreation: " <> show (t ^. CF.rtdRequiresRecreation)
          , "          Name: " <> show (t ^. CF.rtdName)
          ]
