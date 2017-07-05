{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.MainSpec (spec) where

import Control.Lens (review)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (MockT, WithResult(..), runMockT)
import Control.Monad.Mock.TH (makeAction, ts)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import Test.Hspec

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Main
import Network.CloudSeeder.Test.Stubs

makeAction "CloudAction" [ts| MonadCloud |]
mockCloudT :: Monad m => [WithResult CloudAction] -> MockT CloudAction m a -> m a
mockCloudT = runMockT

spec :: Spec
spec = do
  describe "cli" $ do
    let rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootParams = [("Env", "test")]

    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure p y x = runIdentity x `shouldBe` Left (review p y)

    it "fails if the template doesn't exist" $ do
      let config = deployment "foo" $ do
            stack_ "base"
            stack_ "server"
      runFailure _FileNotFound "server.yaml" $ cli config
        & stubFileSystemT []
        & stubExceptT
        & stubEnvironmentT []
        & stubArgumentsT (DeployStack "server" "test")
        & mockCloudT []

    it "fails if the template parameters can't be parsed" $ do
      let config = deployment "foo" $ do
            stack_ "base"
          err = "YAML parse exception at line 0, column 8,\nwhile scanning a directive:\nfound unknown directive name"
      runFailure _CliTemplateDecodeFail err $ cli config
        & stubFileSystemT [("base.yaml", "%invalid")]
        & stubExceptT
        & stubEnvironmentT []
        & stubArgumentsT (DeployStack "base" "test")
        & mockCloudT []

    it "fails if user attempts to deploy a stack that doesn't exist in the config" $ do
      let config = deployment "foo" $ do
            stack_ "base"
      runFailure _CliStackNotConfigured "foo" $ cli config
        & stubFileSystemT
          [ ("base.yaml", rootTemplate)]
        & stubExceptT
        & stubEnvironmentT []
        & stubArgumentsT (DeployStack "foo" "test")
        & mockCloudT []

    context "the configuration does not have environment variables" $ do
      let config = deployment "foo" $ do
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "applies a changeset to a stack" $ example $ do
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "passes only the outputs from previous stacks that are listed in this template's Parameters" $ do
        let servertemplate = rootTemplate
                          <> "  foo:\n"
                          <> "  bar:\n"
            baseOutputs = [ ("first", "output")
                          , ("foo", "baz")
                          , ("bar", "qux")
                          , ("last", "output") ]

        runSuccess $ cli config
          & stubFileSystemT
            [ ("server.yaml", servertemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "server" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , ComputeChangeset
                "test-foo-server"
                servertemplate
                (rootParams ++ [("foo", "baz"), ("bar", "qux")])
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]


        let frontendtemplate = rootTemplate
                            <> "  foo:"

        runSuccess $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", frontendtemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "frontend" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , GetStackOutputs "test-foo-server" :-> Just []
            , ComputeChangeset
                "test-foo-frontend"
                frontendtemplate
                (rootParams ++ [("foo", "baz")])
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails if a dependency stack does not exist" $ do
        runFailure _CliMissingDependencyStacks ["base"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "frontend" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Just [] ]

        runFailure _CliMissingDependencyStacks ["server"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "frontend" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , GetStackOutputs "test-foo-server" :-> Nothing ]

        runFailure _CliMissingDependencyStacks ["base", "server"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "frontend" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Nothing ]

    context "the configuration has global environment variables" $ do
      let config = deployment "foo" $ do
            environment ["Domain", "SecretsStore"]
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "passes the value in each global environment variable as a parameter" $ do
        let template = rootTemplate
                    <> "  Domain:\n"
                    <> "  SecretsStore:\n"
        let env = rootParams ++ [ ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT env
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" template env rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails when a global environment variable is missing" $ do
        let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
        runFailure _CliMissingEnvVars ["Domain"] $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT env
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT []

      it "reports all missing environment variables at once in alphabetical order" $ do
        runFailure _CliMissingEnvVars ["Domain", "SecretsStore"] $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT []

    context "the configuration has global and local environment variables" $ do
      let config = deployment "foo" $ do
            environment ["Domain", "SecretsStore"]
            stack "base" $ environment ["Base"]
            stack "server" $ environment ["Server1", "Server2"]
            stack "frontend" $ environment ["Frontend"]
      let template = rootTemplate
                  <> "  Domain:\n"
                  <> "  SecretsStore:\n"

      it "passes the value in each local environment variable to the proper stack" $ do
        let env = [ ("Env", "test"), ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
            baseEnv = env ++ [ ("Base", "a") ]
            baseTemplate = template
                        <> "  Base:\n"
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", baseTemplate) ]
          & stubExceptT
          & stubEnvironmentT baseEnv
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" baseTemplate baseEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let serverEnv = env ++ [ ("Server1", "b"), ("Server2", "c") ]
            serverTemplate = template
                          <> "  Server1:\n"
                          <> "  Server2:\n"
        runSuccess $ cli config
          & stubFileSystemT
            [ ("server.yaml", serverTemplate) ]
          & stubExceptT
          & stubEnvironmentT serverEnv
          & stubArgumentsT (DeployStack "server" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" serverTemplate serverEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "the configuration has global tags" $ do 
      let globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
          config = deployment "foo" $ do
            tags globalTags
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "passes the value in each tag" $ do
        let expectedTags = rootExpectedTags ++ globalTags
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams expectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "the configuration has global and local tags" $ do 
      let globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
          serverTags = [("x", "z")]

          expectedGlobalTags = rootExpectedTags ++ globalTags
          expectedServerTags = expectedGlobalTags ++ serverTags

          config = deployment "foo" $ do
            tags globalTags
            stack_ "base" 
            stack "server" $ tags serverTags
            stack "frontend" $ tags [("frontendTag1", "ft1"), ("frontendTag2", "ft2")]

      it "passes the value in each tag to the proper stack" $ do
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams expectedGlobalTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli config
          & stubFileSystemT
            [ ("server.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "server" "test")
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" rootTemplate rootParams expectedServerTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "monadic logic" $ do
      it "can make decisions based on the env passed in" $ do
        let template = "Parameters:\n"
                    <> "  Env:\n"
                    <> "  foo:\n"
                    <> "  baz:\n"
        let config = deployment "foo" $ do
              param "foo" "bar"
              whenEnv "prod" $ do
                param "baz" "qux"
              stack_ "base"

            expectedParams = [("foo", "bar"), ("baz", "qux"), ("Env", "prod")]
            expectedTags = [("cj:application","foo"),("cj:environment","prod")]

        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "prod")
          & mockCloudT
            [ ComputeChangeset "prod-foo-base" template expectedParams expectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" template (("foo", "bar") : rootParams) rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let config2 = deployment "foo" $ do
              param "foo" "bar"
              env <- getEnvArg
              when (env == "prod") $ do
                param "baz" "qux"
              stack_ "base"

        runSuccess $ cli config2
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "prod")
          & mockCloudT
            [ ComputeChangeset "prod-foo-base" template expectedParams expectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli config2
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT []
          & stubArgumentsT (DeployStack "base" "test")
          & mockCloudT
            [ ComputeChangeset "test-foo-base" template (("foo", "bar") : rootParams) rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]
