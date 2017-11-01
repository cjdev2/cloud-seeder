module Network.CloudSeeder.MainSpec (spec) where

import Control.Lens ((.~), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import GHC.Exts (IsList(..))
import Network.AWS.CloudFormation (StackStatus(..))
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Main
import Network.CloudSeeder.Shared
import Network.CloudSeeder.Types
import Network.CloudSeeder.Test.Stubs

import qualified Data.Text as T
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B

type TagList = forall a. (IsList a, Item a ~ (T.Text, T.Text)) => a

spec :: Spec
spec =
  describe "cli" $ do
    let rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootParams = [("Env", "test")]
        rootExpectedParams = [("Env", Value "test")]
        serverTestArgs = ["provision", "server", "test"]
        baseTestArgs = ["provision", "base", "test"]

        expectedStack :: T.Text -> StackStatus -> Stack
        expectedStack stackName = Stack
          Nothing
          (Just "csId")
          stackName
          []
          ["Env"]
          (Just "sId")

        expectedStackInfo :: B.ByteString -> StackStatus -> B.ByteString
        expectedStackInfo stackName status = B.unlines
          [ "Stack Info:"
          , "  name: " <> stackName
          , "  status: " <> B.pack (show status)
          , "  outputs: fromList []"
          ]

    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)

    describe "provision" $ do



      context "the configuration does not have environment variables" $ do
        let config = deployment "foo" $ do
              stack_ "base"
              stack_ "server"
              stack_ "frontend"


        it "passes only the outputs from previous stacks that are listed in this template's Parameters" $ do
          let serverTemplate = rootTemplate
                            <> "  foo:\n"
                            <> "    Type: String\n"
                            <> "  bar:\n"
                            <> "    Type: String\n"
              baseOutputs = [ ("first", "output")
                            , ("foo", "baz")
                            , ("bar", "qux")
                            , ("last", "output") ]

          runSuccess $ cli config
            & stubFileSystemT
              [ ("server.yaml", serverTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT serverTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-server" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete & outputs .~ baseOutputs)
              , ComputeChangeset
                  "test-foo-server"
                  CreateStack
                  serverTemplate
                  (rootExpectedParams <> [("bar", Value "qux"), ("foo", Value "baz")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

          let frontendtemplate = rootTemplate
                              <> "  foo:\n"
                              <> "    Type: String\n"

          runSuccess $ cli config
            & stubFileSystemT
              [ ("frontend.yaml", frontendtemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "frontend", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete & outputs .~ baseOutputs)
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete)
              , ComputeChangeset
                  "test-foo-frontend"
                  CreateStack
                  frontendtemplate
                  (rootExpectedParams <> [("foo", Value "baz")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

        it "fails if a dependency stack does not exist" $ do
          runFailure _CliMissingDependencyStacks ["base"] $ cli config
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "frontend", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Nothing
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete) ]
            & stubExceptT

          runFailure _CliMissingDependencyStacks ["server"] $ cli config
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "frontend", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
              , DescribeStack "test-foo-server" :-> Nothing ]
            & stubExceptT

          runFailure _CliMissingDependencyStacks ["base", "server"] $ cli config
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "frontend", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Nothing
              , DescribeStack "test-foo-server" :-> Nothing ]
            & stubExceptT

      context "the configuration has global environment variables" $ do
        let config = deployment "foo" $ do
              environment ["Domain", "SecretsStore"]
              stack_ "base"
              stack_ "server"
              stack_ "frontend"

        it "passes the value in each global environment variable as a parameter" $ do
          let template = rootTemplate
                      <> "  Domain:\n"
                      <> "    Type: String\n"
                      <> "  SecretsStore:\n"
                      <> "    Type: String\n"
          let env = rootParams <> [ ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
              expectedParams = rootExpectedParams <> [("Domain", Value "example.com"), ("SecretsStore", Value "arn::aws:1234")]
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT env
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset "test-foo-base" CreateStack template expectedParams rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

        it "fails when a global environment variable is missing" $ do
          let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
          runFailure _CliMissingEnvVars ["Domain"] $ cli config
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT env
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
            & stubExceptT

        it "reports all missing environment variables at once in alphabetical order" $
          runFailure _CliMissingEnvVars ["Domain", "SecretsStore"] $ cli config
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
            & stubExceptT

      context "the configuration has global and local environment variables" $ do
        let config = deployment "foo" $ do
              environment ["Domain", "SecretsStore"]
              stack "base" $ environment ["Base"]
              stack "server" $ environment ["Server1", "Server2"]
              stack "frontend" $ environment ["Frontend"]
        let template = rootTemplate
                    <> "  Domain:\n"
                    <> "    Type: String\n"
                    <> "  SecretsStore:\n"
                    <> "    Type: String\n"

        it "passes the value in each local environment variable to the proper stack" $ do
          let env = [ ("Domain", "example.com"), ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
              baseEnv = env <> [ ("Base", "a") ]
              baseTemplate = template
                          <> "  Base:\n"
                          <> "    Type: String\n"
              expectedParams = rootExpectedParams <> [("Domain", Value "example.com"), ("Env", Value "test"), ("SecretsStore", Value "arn::aws:1234")]
              expectedBaseParams = expectedParams <> [("Base", Value "a")]
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", baseTemplate) ]
            & stubEnvironmentT baseEnv
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  baseTemplate
                  expectedBaseParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

          let serverEnv = env <> [ ("Server1", "b"), ("Server2", "c") ]
              serverTemplate = template
                            <> "  Server1:\n"
                            <> "    Type: String\n"
                            <> "  Server2:\n"
                            <> "    Type: String\n"
              expectedServerParams = expectedParams <> [("Server1", Value "b"), ("Server2", Value "c")]
          runSuccess $ cli config
            & stubFileSystemT
              [ ("server.yaml", serverTemplate) ]
            & stubEnvironmentT serverEnv
            & stubCommandLineT serverTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-server" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
              , ComputeChangeset
                  "test-foo-server"
                  CreateStack
                  serverTemplate
                  expectedServerParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

      context "the configuration has global tags" $ do
        let globalTags :: TagList
            globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
            config = deployment "foo" $ do
              tags globalTags
              stack_ "base"
              stack_ "server"
              stack_ "frontend"

        it "passes the value in each tag" $ do
          let expectedTags = rootExpectedTags <> globalTags
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  expectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

      context "the configuration has global and local tags" $ do
        let globalTags :: TagList
            globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
            serverTags :: TagList
            serverTags = [("x", "z")]

            expectedGlobalTags = rootExpectedTags <> globalTags
            expectedServerTags = expectedGlobalTags <> serverTags

            config = deployment "foo" $ do
              tags globalTags
              stack_ "base"
              stack "server" $ tags serverTags
              stack "frontend" $ tags [("frontendTag1", "ft1"), ("frontendTag2", "ft2")]

        it "passes the value in each tag to the proper stack" $ do
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT baseTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  expectedGlobalTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

          runSuccess $ cli config
            & stubFileSystemT
              [ ("server.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT serverTestArgs
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-server" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
              , ComputeChangeset
                  "test-foo-server"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  expectedServerTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

      context "monadic logic" $
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
              & stubLoggerT []
              & mockActionT
                [ DescribeStack "prod-foo-base" :-> Nothing
                , ComputeChangeset
                    "prod-foo-base"
                    CreateStack
                    template
                    [("Env", Value "prod"), ("baz", Value "qux"), ("foo", Value "bar")]
                    expectedTags
                    :-> "csid"
                , RunChangeSet "csid" :-> 200 ]
              & stubExceptT

          it "does not provide prod-only params when not in prod" $
            runSuccess $ cli config
              & stubFileSystemT
                [ ("base.yaml", template) ]
              & stubEnvironmentT []
              & stubCommandLineT baseTestArgs
              & stubLoggerT []
              & mockActionT
                [ DescribeStack "test-foo-base" :-> Nothing
                , ComputeChangeset
                    "test-foo-base"
                    CreateStack
                    template
                    ([("foo", Value "bar")] <> rootExpectedParams)
                    rootExpectedTags
                    :-> "csid"
                , RunChangeSet "csid" :-> 200 ]
              & stubExceptT

      context "flags" $ do
        let template = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
                    <> "  baz:\n"
                    <> "    Type: String\n"
                    <> "    Default: prod"
        let config = deployment "foo" $ do
              flag "baz"
              stack_ "base"

        it "accepts optional flags where the config calls for them" $
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "test", "--baz", "zab"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  template
                  (rootExpectedParams <> [("baz", Value "zab")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

        it "provides a default if an optional flag isn't provided" $
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  template
                  (rootExpectedParams <> [("baz", Value "prod")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

        it "raises an error if a flag is provided that does not exist in the template" $
          let config' = deployment "foo" $ do
                mapM_ flag (["foo", "bar", "baz"] :: [T.Text])
                stack_ "base"
          in runFailure _CliExtraParameterFlags ["foo", "bar"] $ cli config'
              & stubFileSystemT
                [ ("base.yaml", template) ]
              & stubEnvironmentT []
              & stubCommandLineT
                ["provision", "base", "test", "--foo", "oof", "--bar", "rab", "--baz", "zab"]
              & stubLoggerT []
              & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
              & stubExceptT

        it "when updating a stack, flags are optional" $ do
          let baseTemplate = "Parameters:\n"
                      <> "  Env:\n"
                      <> "    Type: String\n"
                      <> "  baz:\n"
                      <> "    Type: String\n"
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", baseTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :->
                  Just (expectedStack "test-foo-base" SSCreateComplete
                    & parameters .~ ["Env", "baz"])
              , ComputeChangeset
                  "test-foo-base"
                  (UpdateStack ["Env","baz"])
                  baseTemplate
                  (rootExpectedParams <> [("baz", UsePreviousValue)])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

      context "global stacks" $ do
        it "global stacks can be deployed into the global environment" $ do
          let config' = deployment "foo" $ do
                stack "repo"
                  global
                stack_ "base"
          runSuccess $ cli config'
            & stubFileSystemT
              [ ("repo.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "repo", "global"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "global-foo-repo" :-> Nothing
              , ComputeChangeset
                  "global-foo-repo"
                  CreateStack
                  rootTemplate
                  [("Env", Value "global")]
                  [("cj:application", "foo"), ("cj:environment", "global")]
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT

        it "global stack may not be deployed into namespaces other than global" $ do
          let config' = deployment "foo" $ do
                stack "repo"
                  global
                stack_ "base"
          runFailure _CliGlobalStackMustProvisionToGlobal "repo" $ cli config'
            & stubFileSystemT
              [ ("repo.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "repo", "test"]
            & stubLoggerT []
            & mockActionT []
            & stubExceptT


        it "other stacks can not be deployed into the global namespace" $ do
          let config' = deployment "foo" $ do
                stack "repo" global
                stack_ "base"
          runFailure _CliStackNotGlobal "base" $ cli config'
            & stubFileSystemT
              [("base.yaml", rootTemplate)]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "global"]
            & stubLoggerT []
            & mockActionT []
            & stubExceptT

        it "other stacks treat global stacks as dependencies" $ do
          let config' = deployment "foo" $ do
                stack "repo" global
                stack "accountSettings" global
                stack_ "base"
          runSuccess $ cli config'
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "test"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , DescribeStack "global-foo-repo" :-> Just (expectedStack "test-foo-repo" SSCreateComplete)
              , DescribeStack "global-foo-accountSettings" :-> Just (expectedStack "test-foo-accountSettings" SSCreateComplete)
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
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
                  & stubLoggerT []
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
                    , RunChangeSet "csid" :-> 200 ]
                  & stubExceptT

          it "onCreate actions can set parameters" $ do
            let config' = deployment "foo" $
                  stack "base" $
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
                  & stubLoggerT []
                  & mockActionT
                    [ DescribeStack "test-foo-base" :-> Nothing
                    , ComputeChangeset
                        "test-foo-base"
                        CreateStack
                        template'
                        (rootExpectedParams <> [("bucketName", Value "the-best-bucket")])
                        rootExpectedTags
                        :-> "csid"
                    , RunChangeSet "csid" :-> 200 ]
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
                  & stubLoggerT []
                  & mockActionT
                    [ DescribeStack "test-foo-base" :-> Nothing
                    , ComputeChangeset
                        "test-foo-base"
                        CreateStack
                        template'
                        (rootExpectedParams <> [("bucketName", Value "the-best-bucket")])
                        rootExpectedTags
                        :-> "csid"
                    , RunChangeSet "csid" :-> 200 ]
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
              & stubLoggerT []
              & mockActionT
                [ DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
                , ComputeChangeset
                    "test-foo-base"
                    (UpdateStack ["Env"])
                    rootTemplate
                    rootExpectedParams
                    rootExpectedTags
                    :-> "csid"
                , RunChangeSet "csid" :-> 200 ]
              & stubExceptT

      context "wait" $ do
        let config = deployment "foo" $
              stack_ "base"

        it "waits for the requested command to finish" $
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & stubCommandLineT ["provision", "base", "test", "--wait"]
            & stubLoggerT []
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> 200
              , Wait StackCreateComplete "test-foo-base" :-> () ]
            & stubExceptT
