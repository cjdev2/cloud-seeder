{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.MainSpec (spec) where

import Control.Lens (review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (MockT, WithResult(..), runMockT)
import Control.Monad.Mock.TH (makeAction, ts)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Main
import Network.CloudSeeder.Test.Stubs

makeAction "CloudAction" [ts| MonadCloud |]
mockCloudT :: Monad m => [WithResult CloudAction] -> MockT CloudAction m a -> m a
mockCloudT = runMockT

spec :: Spec
spec = parallel $ do
  describe "cli" $ do
    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure p y x = runIdentity x `shouldBe` Left (review p y)

    it "fails if the template doesn't exist" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
            stack_ "server"
          env = [("Env", "test")]
      runFailure _FileNotFound "server.yaml" $ cli (DeployStack "server") config
        & stubFileSystemT []
        & stubExceptT
        & stubEnvironmentT env
        & mockCloudT []

    it "fails if user attempts to deploy a stack that doesn't exist in the config" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
          env = [("Env", "test")]
      runFailure _CliStackNotConfigured "foo" $ cli (DeployStack "foo") config
        & stubFileSystemT
          [ ("base.yaml", "base.yaml contents")]
        & stubExceptT
        & stubEnvironmentT env
        & mockCloudT []

    context "the configuration does not have environment variables" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
            stack_ "server"
            stack_ "frontend"
          env = [("Env", "test")]

      it "applies a changeset to a stack" $ example $ do
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" env :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "passes the outputs from prior stacks" $ do
        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just [("foo", "bar")]
            , ComputeChangeset "test-foo-server" "server.yaml contents" (env ++ [("foo", "bar")]) :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just [("foo", "bar")]
            , GetStackOutputs "test-foo-server" :-> Just [("baz", "qux")]
            , ComputeChangeset "test-foo-frontend" "frontend.yaml contents" (env ++ [("foo", "bar"), ("baz", "qux")]) :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails if a dependency stack does not exist" $ do
        runFailure _CliMissingDependencyStacks ["base"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Just [] ]

        runFailure _CliMissingDependencyStacks ["server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , GetStackOutputs "test-foo-server" :-> Nothing ]

        runFailure _CliMissingDependencyStacks ["base", "server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Nothing ]

      it "fails when the Env environment variable is not specified" $ do
        runFailure _CliMissingEnvVars ["Env"] $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
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
        let env = [ ("Env", "test"), ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" env :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails when a global environment variable is missing" $ do
        let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
        runFailure _CliMissingEnvVars ["Domain"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & mockCloudT []

      it "reports all missing environment variables at once in alphabetical order" $ do
        runFailure _CliMissingEnvVars ["Domain", "Env", "SecretsStore"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT []
          & mockCloudT []

    context "the configuration has global and local environment variables" $ do
      let config = runIdentity $ deployment "foo" $ do
            environment ["Domain", "SecretsStore"]
            stack "base" $ environment ["Base"]
            stack "server" $ environment ["Server1", "Server2"]
            stack "frontend" $ environment ["Frontend"]

      it "passes the value in each local environment variable to the proper stack" $ do
        let env = [ ("Env", "test"), ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
            baseEnv = env ++ [ ("Base", "a") ]
        runSuccess $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT baseEnv
          & mockCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" baseEnv :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let serverEnv = env ++ [ ("Server1", "b"), ("Server2", "c") ]
        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT serverEnv
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" "server.yaml contents" serverEnv :-> "csid"
            , RunChangeSet "csid" :-> () ]
