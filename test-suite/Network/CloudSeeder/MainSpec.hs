{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.MainSpec (spec) where

import Control.Lens (review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (MockT, WithResult(..), runMockT)
import Control.Monad.Mock.TH (makeAction, ts)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import Test.Hspec

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
        rootEnv = [("Env", "test")]
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]

    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure p y x = runIdentity x `shouldBe` Left (review p y)

    it "fails if the template doesn't exist" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
            stack_ "server"
      runFailure _FileNotFound "server.yaml" $ cli (DeployStack "server") config
        & stubFileSystemT []
        & stubExceptT
        & stubEnvironmentT rootEnv
        & mockCloudT []

    it "fails if the template parameters can't be parsed" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
          err = "YAML parse exception at line 0, column 8,\nwhile scanning a directive:\nfound unknown directive name"
      runFailure _CliTemplateDecodeFail err $ cli (DeployStack "base") config
        & stubFileSystemT [("base.yaml", "%invalid")]
        & stubExceptT
        & stubEnvironmentT rootEnv
        & mockCloudT []

    it "fails if user attempts to deploy a stack that doesn't exist in the config" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
      runFailure _CliStackNotConfigured "foo" $ cli (DeployStack "foo") config
        & stubFileSystemT
          [ ("base.yaml", rootTemplate)]
        & stubExceptT
        & stubEnvironmentT rootEnv
        & mockCloudT []

    context "the configuration does not have environment variables" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "applies a changeset to a stack" $ example $ do
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "passes only the outputs from previous stacks that are listed in this template's Parameters" $ do
        let servertemplate = rootTemplate
                          <> "  foo:\n"
                          <> "  bar:\n"
            baseOutputs = [ ("first", "output")
                          , ("foo", "baz")
                          , ("bar", "qux")
                          , ("last", "output") ]

        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", servertemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , ComputeChangeset
                "test-foo-server"
                servertemplate
                ([("foo", "baz"), ("bar", "qux")] ++ rootEnv)
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]


        let frontendtemplate = rootTemplate
                            <> "  foo:"

        runSuccess $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", frontendtemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , GetStackOutputs "test-foo-server" :-> Just []
            , ComputeChangeset
                "test-foo-frontend"
                frontendtemplate
                (("foo", "baz") : rootEnv)
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails if a dependency stack does not exist" $ do
        runFailure _CliMissingDependencyStacks ["base"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Just [] ]

        runFailure _CliMissingDependencyStacks ["server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , GetStackOutputs "test-foo-server" :-> Nothing ]

        runFailure _CliMissingDependencyStacks ["base", "server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Nothing ]

      it "fails when the Env environment variable is not specified" $ do
        runFailure _CliMissingEnvVars ["Env"] $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & mockCloudT []

    context "the configuration has global environment variables" $ do
      let config = runIdentity $ deployment "foo" $ do
            environment ["Domain", "SecretsStore"]
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "passes the value in each global environment variable as a parameter" $ do
        let template = rootTemplate
                    <> "  Domain:\n"
                    <> "  SecretsStore:\n"
        let env = [ ("Env", "test"), ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ ComputeChangeset "test-foo-base" template env rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails when a global environment variable is missing" $ do
        let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
        runFailure _CliMissingEnvVars ["Domain"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT []

      it "reports all missing environment variables at once in alphabetical order" $ do
        runFailure _CliMissingEnvVars ["Domain", "Env", "SecretsStore"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT []
          & mockCloudT []

    context "the configuration has global and local environment variables" $ do
      let config = runIdentity $ deployment "foo" $ do
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
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", baseTemplate) ]
          & stubExceptT
          & stubEnvironmentT baseEnv
          & mockCloudT
            [ ComputeChangeset "test-foo-base" baseTemplate baseEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let serverEnv = env ++ [ ("Server1", "b"), ("Server2", "c") ]
            serverTemplate = template
                          <> "  Server1:\n"
                          <> "  Server2:\n"
        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", serverTemplate) ]
          & stubExceptT
          & stubEnvironmentT serverEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" serverTemplate serverEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "the configuration has global tags" $ do 
      let globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
          config = runIdentity $ deployment "foo" $ do
            tags globalTags
            stack_ "base"
            stack_ "server"
            stack_ "frontend"

      it "passes the value in each tag" $ do
        let expectedTags = rootExpectedTags ++ globalTags
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootEnv expectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "the configuration has global and local tags" $ do 
      let globalTags = [("cj:squad", "lambda"), ("taggo", "oggat")]
          serverTags = [("x", "z")]

          expectedGlobalTags = rootExpectedTags ++ globalTags
          expectedServerTags = expectedGlobalTags ++ serverTags

          config = runIdentity $ deployment "foo" $ do
            tags globalTags
            stack_ "base" 
            stack "server" $ tags serverTags
            stack "frontend" $ tags [("frontendTag1", "ft1"), ("frontendTag2", "ft2")]

      it "passes the value in each tag to the proper stack" $ do
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootEnv expectedGlobalTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", rootTemplate) ]
          & stubExceptT
          & stubEnvironmentT rootEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" rootTemplate rootEnv expectedServerTags :-> "csid"
            , RunChangeSet "csid" :-> () ]
