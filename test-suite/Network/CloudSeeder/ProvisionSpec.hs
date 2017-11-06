module Network.CloudSeeder.ProvisionSpec (spec) where

import Control.Lens ((&), (.~), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import GHC.Exts (IsList(..))
import Network.AWS.CloudFormation (StackStatus(..))
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Provision
import Network.CloudSeeder.Types
import Network.CloudSeeder.Test.Orphans ()
import Network.CloudSeeder.Test.Stubs

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B

type TagList = forall a. (IsList a, Item a ~ (T.Text, T.Text)) => a

spec :: Spec
spec =
  describe "Provision" $ do

    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)

        rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootExpectedParams = [("Env", Value "test")]
        rootParams = [("Env", "test")]

        simpleConfig = DeploymentConfiguration "foo" []
          [ StackConfiguration "base" [] [] False [] Nothing
          , StackConfiguration "server" [] [] False [] Nothing
          , StackConfiguration "frontend" [] [] False [] Nothing
          ] []

        expectedStack :: T.Text -> StackStatus -> Stack
        expectedStack stackName = Stack
          Nothing
          (Just "csId")
          stackName
          []
          ["Env"]
          (Just "sId")

        expectedStackInfo :: B.ByteString -> B.ByteString -> B.ByteString
        expectedStackInfo stackName status = B.unlines
          [ "Stack Info:"
          , "  name: " <> stackName
          , "  status: " <> status
          , "  outputs: \n" ]

    describe "provisionCommand" $ do
      describe "failures" $ do
        it "fails if the template doesn't exist" $
          runFailure _FileNotFound "base.yaml"
            $ provisionCommand (pure simpleConfig) "base" "test" []
            & stubFileSystemT []
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

        it "fails if the template parameters can't be parsed" $ do
          let err = "YAML parse exception at line 0, column 8,\nwhile scanning a directive:\nfound unknown directive name"
          runFailure _CliTemplateDecodeFail err
            $ provisionCommand (pure simpleConfig) "base" "test" []
            & stubFileSystemT [("base.yaml", "%invalid")]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

        it "fails if user attempts to deploy a stack that doesn't exist in the config" $
          runFailure _CliStackNotConfigured "snipe"
            $ provisionCommand (pure simpleConfig) "snipe" "test" []
            & stubFileSystemT []
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

        it "fails if parameters required in the template are not supplied" $ do
          let baseTemplate =
                  rootTemplate
                <> "  foo:\n"
                <> "    Type: String\n"
                <> "  bar:\n"
                <> "    Type: String\n"
          runFailure _CliMissingRequiredParameters ["foo", "bar"]
            $ provisionCommand (pure simpleConfig) "base" "test" ["provision", "base", "test"]
            & stubFileSystemT [("base.yaml", baseTemplate)]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT [ DescribeStack "test-foo-base" :-> Nothing ]
            & stubExceptT

      describe "general" $
        it "logs a stack after successfully applying a change set" $ example $
          runSuccess $ provisionCommand (pure simpleConfig) "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [("base.yaml", rootTemplate)]
            & stubEnvironmentT []
            & stubLoggerT [expectedStackInfo "test-foo-base" "StackCreateComplete"]
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset "test-foo-base" CreateStack rootTemplate rootExpectedParams rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

      context "the configuration does not have environment variables" $ do
        it "applies a changeset to a stack" $ example $
          runSuccess $ provisionCommand (pure simpleConfig) "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [("base.yaml", rootTemplate)]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset "test-foo-base" CreateStack rootTemplate rootExpectedParams rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

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

          runSuccess $ provisionCommand (pure simpleConfig) "server" "test" ["provision", "server", "test"]
            & stubFileSystemT
              [ ("server.yaml", serverTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-server") :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete) ]
            & stubExceptT

          let frontendtemplate = rootTemplate
                              <> "  foo:\n"
                              <> "    Type: String\n"

          runSuccess $ provisionCommand (pure simpleConfig) "frontend" "test" ["provision", "frontend", "test"]
            & stubFileSystemT
              [ ("frontend.yaml", frontendtemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-frontend" :-> Just (expectedStack "test-foo-frontend" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-frontend") :-> ()
              , DescribeStack "test-foo-frontend" :-> Just (expectedStack "test-foo-frontend" SSCreateComplete) ]
            & stubExceptT

        it "fails if a dependency stack does not exist" $ do
          runFailure _CliMissingDependencyStacks ["base"]
            $ provisionCommand (pure simpleConfig) "frontend" "test" ["provision", "frontend", "test"]
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Nothing
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete) ]
            & stubExceptT

          runFailure _CliMissingDependencyStacks ["server"]
            $ provisionCommand (pure simpleConfig) "frontend" "test" ["provision", "frontend", "test"]
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete)
              , DescribeStack "test-foo-server" :-> Nothing ]
            & stubExceptT

          runFailure _CliMissingDependencyStacks ["base", "server"]
            $ provisionCommand (pure simpleConfig) "frontend" "test" ["provision", "frontend", "test"]
            & stubFileSystemT
              [ ("frontend.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-frontend" :-> Nothing
              , DescribeStack "test-foo-base" :-> Nothing
              , DescribeStack "test-foo-server" :-> Nothing ]
            & stubExceptT

      context "the configuration has global environment variables" $ do
        let ps = [("Domain", Env), ("SecretsStore", Env)] :: S.Set (T.Text, ParameterSource)
            mConfig = pure $ simpleConfig & parameterSources .~ ps
        it "passes the value in each global environment variable as a parameter" $ do
          let template = rootTemplate
                      <> "  Domain:\n"
                      <> "    Type: String\n"
                      <> "  SecretsStore:\n"
                      <> "    Type: String\n"
          let env = rootParams <> [ ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
              expectedParams = rootExpectedParams <> [("Domain", Value "example.com"), ("SecretsStore", Value "arn::aws:1234")]

          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT env
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset "test-foo-base" CreateStack template expectedParams rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

        it "fails when a global environment variable is missing" $ do
          let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
          runFailure _CliMissingEnvVars ["Domain"]
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT env
            & ignoreLoggerT
            & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
            & stubExceptT

        it "reports all missing environment variables at once in alphabetical order" $
          runFailure _CliMissingEnvVars ["Domain", "SecretsStore"]
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
            & stubExceptT

      context "the configuration has global and local environment variables" $ do
        let mConfig = pure $ DeploymentConfiguration "foo" []
              [ StackConfiguration "base" [] [("Base", Env)] False [] Nothing
              , StackConfiguration "server" [] [("Server1", Env), ("Server2", Env)] False [] Nothing
              , StackConfiguration "frontend" [] [] False [] Nothing
              ] [("Domain", Env), ("SecretsStore", Env)]

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

          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", baseTemplate) ]
            & stubEnvironmentT baseEnv
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  baseTemplate
                  expectedBaseParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

          let serverEnv = env <> [ ("Server1", "b"), ("Server2", "c") ]
              serverTemplate = template
                            <> "  Server1:\n"
                            <> "    Type: String\n"
                            <> "  Server2:\n"
                            <> "    Type: String\n"
              expectedServerParams = expectedParams <> [("Server1", Value "b"), ("Server2", Value "c")]
          runSuccess
            $ provisionCommand mConfig "server" "test" ["provision", "server", "test"]
            & stubFileSystemT
              [ ("server.yaml", serverTemplate) ]
            & stubEnvironmentT serverEnv
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-server") :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete) ]
            & stubExceptT

      context "the configuration has global tags" $ do
        let globalTags :: TagList
            globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
            mConfig = pure $ simpleConfig & tagSet .~ globalTags

        it "passes the value in each tag" $ do
          let expectedTags = rootExpectedTags <> globalTags
          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  expectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

      context "the configuration has global and local tags" $ do
        let globalTags :: TagList
            globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
            serverTags :: TagList
            serverTags = [("x", "z")]
            frontEndTags :: TagList
            frontEndTags = [("frontendTag1", "ft1"), ("frontendTag2", "ft2")]

            expectedGlobalTags = rootExpectedTags <> globalTags
            expectedServerTags = expectedGlobalTags <> serverTags

        let mConfig = pure $ DeploymentConfiguration "foo" globalTags
              [ StackConfiguration "base" [] [] False [] Nothing
              , StackConfiguration "server" serverTags [] False [] Nothing
              , StackConfiguration "frontend" frontEndTags [] False [] Nothing
              ] []

        it "passes the value in each tag to the proper stack" $ do
          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  expectedGlobalTags
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

          runSuccess
            $ provisionCommand mConfig "server" "test" ["provision", "server", "test"]
            & stubFileSystemT
              [ ("server.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-server") :-> ()
              , DescribeStack "test-foo-server" :-> Just (expectedStack "test-foo-server" SSCreateComplete) ]
            & stubExceptT

      context "flags" $ do
        let template = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
                    <> "  baz:\n"
                    <> "    Type: String\n"
                    <> "    Default: prod"

        let mConfig = pure $ DeploymentConfiguration "foo" []
              [ StackConfiguration "base" [] [] False [] Nothing ]
              [("baz", Flag)]

        it "accepts optional flags where the config calls for them" $
          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test", "--baz", "zab"]
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  template
                  (rootExpectedParams <> [("baz", Value "zab")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

        it "provides a default if an optional flag isn't provided" $
          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  template
                  (rootExpectedParams <> [("baz", Value "prod")])
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

        it "raises an error if a flag is provided that does not exist in the template" $ do
          let config' = pure $ simpleConfig
                & parameterSources .~ [("foo", Flag), ("bar", Flag), ("baz", Flag)]

          runFailure _CliExtraParameterFlags ["foo", "bar"]
            $ provisionCommand config' "base" "test" ["provision", "base", "test", "--foo", "oof", "--bar", "rab", "--baz", "zab"]
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT [DescribeStack "test-foo-base" :-> Nothing]
            & stubExceptT

        it "when updating a stack, flags are optional" $ do
          let baseTemplate = "Parameters:\n"
                      <> "  Env:\n"
                      <> "    Type: String\n"
                      <> "  baz:\n"
                      <> "    Type: String\n"
          runSuccess
            $ provisionCommand mConfig "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", baseTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

      context "global stacks" $ do
        let mConfig' = pure $ DeploymentConfiguration "foo" []
              [ StackConfiguration "repo" [] [] True [] Nothing
              , StackConfiguration "base" [] [] False [] Nothing
              ] []
        it "global stacks can be deployed into the global environment" $
          runSuccess
            $ provisionCommand mConfig' "repo" "global" ["provision", "repo", "global"]
            & stubFileSystemT
              [ ("repo.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "global-foo-repo" :-> Nothing
              , ComputeChangeset
                  "global-foo-repo"
                  CreateStack
                  rootTemplate
                  [("Env", Value "global")]
                  [("cj:application", "foo"), ("cj:environment", "global")]
                  :-> "csid"
              , RunChangeSet "csid" :-> ()
              , DescribeStack "global-foo-repo" :-> Just (expectedStack "global-foo-repo" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "global-foo-repo") :-> ()
              , DescribeStack "global-foo-repo" :-> Just (expectedStack "global-foo-repo" SSCreateComplete) ]
            & stubExceptT

        it "global stack may not be deployed into namespaces other than global" $
          runFailure _CliGlobalStackMustProvisionToGlobal "repo"
            $ provisionCommand mConfig' "repo" "test" ["provision", "repo", "test"]
            & stubFileSystemT
              [ ("repo.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT


        it "other stacks can not be deployed into the global namespace" $
          runFailure _CliStackNotGlobal "base"
            $ provisionCommand mConfig' "base" "global" ["provision", "base", "global"]
            & stubFileSystemT
              [("base.yaml", rootTemplate)]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

        it "other stacks treat global stacks as dependencies" $ do
          let mConfig'' = pure $ DeploymentConfiguration "foo" []
                [ StackConfiguration "repo" [] [] True [] Nothing
                , StackConfiguration "accountSettings" [] [] True [] Nothing
                , StackConfiguration "base" [] [] False [] Nothing
                ] []

          runSuccess
            $ provisionCommand mConfig'' "base" "test" ["provision", "base", "test"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
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
              , RunChangeSet "csid" :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateInProgress)
              , Wait StackCreateComplete (StackName "test-foo-base") :-> ()
              , DescribeStack "test-foo-base" :-> Just (expectedStack "test-foo-base" SSCreateComplete) ]
            & stubExceptT

      context "--no-wait option" $ do
        let mConfig' = pure $ DeploymentConfiguration "foo" []
              [ StackConfiguration "base" [] [] False [] Nothing ] []

        it "runs the change set without waiting" $
          runSuccess
            $ provisionCommand mConfig' "base" "test" ["provision", "base", "test", "--no-wait"]
            & stubFileSystemT
              [ ("base.yaml", rootTemplate) ]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset
                  "test-foo-base"
                  CreateStack
                  rootTemplate
                  rootExpectedParams
                  rootExpectedTags
                  :-> "csid"
              , RunChangeSet "csid" :-> () ]
            & stubExceptT
