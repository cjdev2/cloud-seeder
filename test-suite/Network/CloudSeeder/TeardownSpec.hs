module Network.CloudSeeder.TeardownSpec (spec) where

import Control.Lens ((&), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Teardown
import Network.CloudSeeder.Test.Orphans ()
import Network.CloudSeeder.Test.Stubs
import Network.CloudSeeder.Types

import qualified Data.Text as T
import qualified Network.AWS.CloudFormation as CF

spec :: Spec
spec =
  describe "Teardown" $ do
    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)

        simpleConfig = DeploymentConfiguration "foo" []
          [ StackConfiguration "base" [] [] False [] Nothing
          , StackConfiguration "server" [] [] False [] Nothing
          , StackConfiguration "frontend" [] [] False [] Nothing
          ] []

        expectedStack :: T.Text -> CF.StackStatus -> Stack
        expectedStack stackName = Stack
          Nothing
          (Just "csId")
          stackName
          []
          ["Env"]
          (Just "sId")

    describe "teardownCommand" $ do
      describe "failures" $ do
        it "fails if stack is not configured" $
          runFailure _CliStackNotConfigured "snipe"
            $ teardownCommand (pure simpleConfig) "snipe" "test"
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

        it "fails if stack is not provisioned" $
          runFailure _CliStackDoesNotExist "test-foo-base"
            $ teardownCommand (pure simpleConfig) "base" "test"
            & ignoreLoggerT
            & mockActionT [ DescribeStack "test-foo-base" :-> Nothing ]
            & stubExceptT

      describe "happy path" $
        it "tears down a configured, stable stack" $ example $
          runSuccess $ teardownCommand (pure simpleConfig) "base" "test"
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" CF.SSCreateComplete)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DeleteStack "test-foo-base" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" CF.SSDeleteComplete)
              , Wait StackDeleteComplete (StackName "test-foo-base") :-> ()
              ]
            & stubExceptT
