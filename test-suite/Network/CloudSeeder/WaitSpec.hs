module Network.CloudSeeder.WaitSpec (spec) where

import Control.Lens ((&), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Functor.Identity (runIdentity)
import Network.AWS.CloudFormation (StackStatus(..))
import Test.Hspec

import Network.CloudSeeder.Commands.Wait
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Monads.AWS
import Network.CloudSeeder.Types
import Network.CloudSeeder.Test.Stubs

import qualified Data.Text as T

spec :: Spec
spec =
  describe "Wait" $ do
    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)
        testFooBase = "test-foo-base"

        expectedStack :: T.Text -> StackStatus -> Stack
        expectedStack stackName = Stack
          Nothing
          (Just "csId")
          stackName
          []
          ["Env"]
          (Just "sId")

        config = DeploymentConfiguration "foo" [] [StackConfiguration "base" [] [] False [] Nothing] []
        mConfig = pure config

    describe "waitCommand" $ do
      it "waits on a stack" $ do
        runSuccess $ waitCommand mConfig "base" "test"
          & ignoreLoggerT
          & mockActionT
            [ DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSCreateInProgress)
            , Wait StackCreateComplete (StackName testFooBase) :-> ()
            , DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSCreateComplete) ]
          & stubExceptT

        runSuccess $ waitCommand mConfig "base" "test"
          & ignoreLoggerT
          & mockActionT
            [ DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSCreateInProgress)
            , Wait StackCreateComplete (StackName testFooBase) :-> ()
            , DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSCreateFailed) ]
          & stubExceptT

        runSuccess $ waitCommand mConfig "base" "test"
          & ignoreLoggerT
          & mockActionT
            [ DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSDeleteInProgress)
            , Wait StackDeleteComplete (StackName testFooBase) :-> ()
            , DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSDeleteComplete) ]
          & stubExceptT

      it "throws an error if stack is waiting for change set review" $
        runFailure _CliStackNeedsChangeSetReview testFooBase $ waitCommand mConfig "base" "test"
          & ignoreLoggerT
          & mockActionT
            [ DescribeStack (StackName testFooBase) :-> Just (expectedStack testFooBase SSReviewInProgress) ]
          & stubExceptT

      it "throws an error if stack doesn't exist" $
        runFailure _CliStackDoesNotExist testFooBase $ waitCommand mConfig "base" "test"
          & ignoreLoggerT
          & mockActionT
            [ DescribeStack (StackName testFooBase) :-> Nothing ]
          & stubExceptT
