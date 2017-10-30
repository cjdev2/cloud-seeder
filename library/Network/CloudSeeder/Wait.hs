module Network.CloudSeeder.Wait
  ( waitCommand
  , waitOnStack
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Error.Lens (throwing)
import Network.AWS.CloudFormation (StackStatus(..))

import qualified Data.Text as T

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Shared
import Network.CloudSeeder.Types

waitCommand :: (AsCliError e, MonadCloud e m, MonadLogger m)
  => m (DeploymentConfiguration m) -> T.Text -> T.Text -> m ()
waitCommand mConfig nameToWaitFor env = do
  config <- mConfig
  let appName = config ^. name
      stackName = mkFullStackName env appName nameToWaitFor
  stackInfo <- waitOnStack stackName
  logStack stackInfo

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
        SSCreateComplete -> wait StackCreateComplete stackName
        SSCreateFailed -> wait StackCreateComplete stackName
        SSCreateInProgress -> wait StackCreateComplete stackName
        SSDeleteComplete -> wait StackDeleteComplete stackName
        SSDeleteFailed -> wait StackDeleteComplete stackName
        SSDeleteInProgress -> wait StackDeleteComplete stackName
        SSRollbackComplete -> wait StackUpdateComplete stackName
        SSRollbackFailed -> wait StackUpdateComplete stackName
        SSRollbackInProgress -> wait StackUpdateComplete stackName
        SSUpdateComplete -> wait StackUpdateComplete stackName
        SSUpdateCompleteCleanupInProgress -> wait StackUpdateComplete stackName
        SSUpdateInProgress -> wait StackUpdateComplete stackName
        SSUpdateRollbackComplete -> wait StackUpdateComplete stackName
        SSUpdateRollbackCompleteCleanupInProgress -> wait StackUpdateComplete stackName
        SSUpdateRollbackFailed -> wait StackUpdateComplete stackName
        SSUpdateRollbackInProgress -> wait StackUpdateComplete stackName
        SSReviewInProgress -> throwing _CliStackNeedsChangeSetReview s
          where (StackName s) = stackName
