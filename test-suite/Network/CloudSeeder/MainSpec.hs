module Network.CloudSeeder.MainSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import Network.AWS.CloudFormation (StackStatus(..), ExecutionStatus(..))
import Test.Hspec

import Network.CloudSeeder.Commands.Shared
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Main
import Network.CloudSeeder.Monads.AWS
import Network.CloudSeeder.Types
import Network.CloudSeeder.Test.Stubs

import qualified Data.Text as T

spec :: Spec
spec =
  describe "cli" $ do
    let rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootExpectedParams = [("Env", Value "test")]
        baseTestArgs = ["provision", "base", "test"]

        expectedStack :: T.Text -> StackStatus -> Stack
        expectedStack stackName = Stack Nothing (Just "csId") stackName [] ["Env"] (Just "sId")

        expectedParameters = [ Parameter ("Env", Value "test") ]
        expectedChanges = [ Add $ ChangeAdd "" (Just "") "" ]
        expectedChangeSet = ChangeSet Nothing "csid" expectedParameters Available expectedChanges

        stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()

    context "monadic dsl logic" $ do
      context "can make decisions based on the env passed in" $ do
        let template = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
                    <> "  foo:\n"
                    <> "    Type: String\n"
                    <> "  baz:\n"
                    <> "    Type: String\n"
                    <> "    Default: prod"
        let config = deployment "foo" $ do
              param "foo" "bar"
              whenEnv "prod" $ param "baz" "qux"
              stack_ "base"
            expectedTags = [("cj:application","foo"),("cj:environment","prod")]

        it "can add a param based on prod" $
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "prod"]
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "prod-foo-base" :-> Nothing
              , ComputeChangeset
                  "prod-foo-base"
                  CreateStack
                  template
                  [("Env", Value "prod"), ("baz", Value "qux"), ("foo", Value "bar")]
                  expectedTags
                  :-> "csid"
              , DescribeChangeSet "csid" :-> expectedChangeSet
              , RunChangeSet "csid" :-> ()
              , DescribeStack "prod-foo-base" :-> Just (expectedStack "prod-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "prod-foo-base") :-> ()
              , DescribeStack "prod-foo-base" :-> Just (expectedStack "prod-foo-base" SSCreateComplete)
              ]
            & stubExceptT

        it "does not provide prod-only params when not in prod" $
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubCommandLineT baseTestArgs
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  template
                  ([("foo", Value "bar")] <> rootExpectedParams)
                  rootExpectedTags
                  :-> "csid"
              , DescribeChangeSet "csid" :-> expectedChangeSet
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

      context "hooks" $
        context "onCreate" $ do
          it "runs onCreate actions after the stack is created" $ do
            let config' = deployment "foo" $
                  stack "base" $
                    onCreate
                      launchMissiles

            runSuccess $ cli config'
              & stubFileSystemT
                [ ("base.yaml", rootTemplate) ]
              & stubEnvironmentT []
              & stubCommandLineT ["provision", "base", "test"]
              & ignoreLoggerT
              & mockActionT
                [ DescribeStack "test-foo-base" :-> Nothing
                , LaunchMissiles :-> ()
                , ComputeChangeset
                    "test-foo-base"
                    CreateStack
                    rootTemplate
                    rootExpectedParams
                    rootExpectedTags
                    :-> "csid"
                , DescribeChangeSet "csid" :-> expectedChangeSet
                , RunChangeSet "csid" :-> ()
                , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
                , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
                , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
              & stubExceptT

          it "onCreate actions can set parameters" $ do
            let config' = deployment "foo" $
                  stack "base" $
                    onCreate $
                      param "bucketName" "the-best-bucket"

                template' = "Parameters:\n"
                        <> "  Env:\n"
                        <> "    Type: String\n"
                        <> "  bucketName:\n"
                        <> "    Type: String\n"

            runSuccess $ cli config'
                  & stubFileSystemT
                    [ ("base.yaml", template') ]
                  & stubEnvironmentT []
                  & stubCommandLineT ["provision", "base", "test"]
                  & ignoreLoggerT
                  & mockActionT
                    [ DescribeStack "test-foo-base" :-> Nothing
                    , ComputeChangeset
                        "test-foo-base"
                        CreateStack
                        template'
                        (rootExpectedParams <> [("bucketName", Value "the-best-bucket")])
                        rootExpectedTags
                        :-> "csid"
                    , DescribeChangeSet "csid" :-> expectedChangeSet
                    , RunChangeSet "csid" :-> ()
                    , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
                    , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
                    , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
                  & stubExceptT

          it "onCreate actions can override parameters" $ do
            let config' = deployment "foo" $
                  stack "base" $ do
                    param "bucketName" "the-worst-bucket"
                    onCreate $
                      param "bucketName" "the-best-bucket"
            let template' = "Parameters:\n"
                        <> "  Env:\n"
                        <> "    Type: String\n"
                        <> "  bucketName:\n"
                        <> "    Type: String\n"
            runSuccess $ cli config'
                  & stubFileSystemT
                    [ ("base.yaml", template') ]
                  & stubEnvironmentT []
                  & stubCommandLineT ["provision", "base", "test"]
                  & ignoreLoggerT
                  & mockActionT
                    [ DescribeStack "test-foo-base" :-> Nothing
                    , ComputeChangeset
                        "test-foo-base"
                        CreateStack
                        template'
                        (rootExpectedParams <> [("bucketName", Value "the-best-bucket")])
                        rootExpectedTags
                        :-> "csid"
                    , DescribeChangeSet "csid" :-> expectedChangeSet
                    , RunChangeSet "csid" :-> ()
                    , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
                    , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
                    , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
                  & stubExceptT

          it "onCreate hooks are not executed on stack update" $ do
            let config' = deployment "foo" $
                  stack "base" $
                    onCreate launchMissiles
            runSuccess $ cli config'
              & stubFileSystemT
                [ ("base.yaml", rootTemplate) ]
              & stubEnvironmentT []
              & stubCommandLineT ["provision", "base", "test"]
              & ignoreLoggerT
              & mockActionT
                [ DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
                , ComputeChangeset
                    "test-foo-base"
                    (UpdateStack ["Env"])
                    rootTemplate
                    rootExpectedParams
                    rootExpectedTags
                    :-> "csid"
                , DescribeChangeSet "csid" :-> expectedChangeSet
                , RunChangeSet "csid" :-> ()
                , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
                , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
                , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
              & stubExceptT
