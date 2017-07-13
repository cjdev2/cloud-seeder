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
import GHC.Exts (IsList(..))
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Main
import Network.CloudSeeder.Test.Stubs

import qualified Data.Text as T

makeAction "CloudAction" [ts| MonadCloud |]
mockCloudT :: Monad m => [WithResult CloudAction] -> MockT CloudAction m a -> m a
mockCloudT = runMockT

type TagList = forall a. (IsList a, Item a ~ (T.Text, T.Text)) => a

spec :: Spec
spec = do
  describe "cli" $ do
    let rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootParams = [("Env", "test")]
        serverTestArgs = ["deploy", "server", "test"]
        baseTestArgs = ["deploy", "base", "test"]

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
        & stubEnvironmentT []
        & stubArgumentsT serverTestArgs
        & stubExceptT
        & mockCloudT []

    it "fails if the template parameters can't be parsed" $ do
      let config = deployment "foo" $ do
            stack_ "base"
          err = "YAML parse exception at line 0, column 8,\nwhile scanning a directive:\nfound unknown directive name"
      runFailure _CliTemplateDecodeFail err $ cli config
        & stubFileSystemT [("base.yaml", "%invalid")]
        & stubEnvironmentT []
        & stubArgumentsT baseTestArgs
        & stubExceptT
        & mockCloudT []

    it "fails if user attempts to deploy a stack that doesn't exist in the config" $ do
      let config = deployment "foo" $ do
            stack_ "base"
          fakeCliInput = ["deploy", "foo", "test"]
      runFailure _CliStackNotConfigured "foo" $ cli config
        & stubFileSystemT
          [ ("base.yaml", rootTemplate)]
        & stubEnvironmentT []
        & stubArgumentsT fakeCliInput
        & stubExceptT
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
          & stubEnvironmentT []
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

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
          & stubArgumentsT serverTestArgs
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , ComputeChangeset
                "test-foo-server"
                serverTemplate
                (rootParams <> [("bar", "qux"), ("foo", "baz")])
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]


        let frontendtemplate = rootTemplate
                            <> "  foo:\n"
                            <> "    Type: String\n"

        runSuccess $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", frontendtemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT ["deploy", "frontend", "test"]
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just baseOutputs
            , GetStackOutputs "test-foo-server" :-> Just []
            , ComputeChangeset
                "test-foo-frontend"
                frontendtemplate
                (rootParams <> [("foo", "baz")])
                rootExpectedTags
                :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails if a dependency stack does not exist" $ do
        runFailure _CliMissingDependencyStacks ["base"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT ["deploy", "frontend", "test"]
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Nothing
            , GetStackOutputs "test-foo-server" :-> Just [] ]

        runFailure _CliMissingDependencyStacks ["server"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT ["deploy", "frontend", "test"]
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , GetStackOutputs "test-foo-server" :-> Nothing ]

        runFailure _CliMissingDependencyStacks ["base", "server"] $ cli config
          & stubFileSystemT
            [ ("frontend.yaml", rootTemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT ["deploy", "frontend", "test"]
          & stubExceptT
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
                    <> "    Type: String\n"
                    <> "  SecretsStore:\n"
                    <> "    Type: String\n"
        let env = rootParams <> [ ("Domain", "example.com"), ("SecretsStore", "arn::aws:1234") ]
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", template) ]
          & stubEnvironmentT env
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT
            [ ComputeChangeset "test-foo-base" template env rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails when a global environment variable is missing" $ do
        let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
        runFailure _CliMissingEnvVars ["Domain"] $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubEnvironmentT env
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT []

      it "reports all missing environment variables at once in alphabetical order" $ do
        runFailure _CliMissingEnvVars ["Domain", "SecretsStore"] $ cli config
          & stubFileSystemT
            [ ("base.yaml", rootTemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT []

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
        runSuccess $ cli config
          & stubFileSystemT
            [ ("base.yaml", baseTemplate) ]
          & stubEnvironmentT baseEnv
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT
            [ ComputeChangeset "test-foo-base" baseTemplate baseEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let serverEnv = env <> [ ("Server1", "b"), ("Server2", "c") ]
            serverTemplate = template
                          <> "  Server1:\n"
                          <> "    Type: String\n"
                          <> "  Server2:\n"
                          <> "    Type: String\n"
        runSuccess $ cli config
          & stubFileSystemT
            [ ("server.yaml", serverTemplate) ]
          & stubEnvironmentT serverEnv
          & stubArgumentsT serverTestArgs
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" serverTemplate serverEnv rootExpectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

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
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams expectedTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

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
          & stubArgumentsT baseTestArgs
          & stubExceptT
          & mockCloudT
            [ ComputeChangeset "test-foo-base" rootTemplate rootParams expectedGlobalTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli config
          & stubFileSystemT
            [ ("server.yaml", rootTemplate) ]
          & stubEnvironmentT []
          & stubArgumentsT serverTestArgs
          & stubExceptT
          & mockCloudT
            [ GetStackOutputs "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" rootTemplate rootParams expectedServerTags :-> "csid"
            , RunChangeSet "csid" :-> () ]

    context "monadic logic" $ do
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
              whenEnv "prod" $ do
                param "baz" "qux"
              stack_ "base"

            expectedParams = [("Env", "prod"), ("baz", "qux"), ("foo", "bar")]
            expectedTags = [("cj:application","foo"),("cj:environment","prod")]
            expectedParamsTest = [("foo", "bar")] <> rootParams

        it "can add a param based on prod" $ do
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubArgumentsT ["deploy", "base", "prod"]
            & stubExceptT
            & mockCloudT
              [ ComputeChangeset "prod-foo-base" template expectedParams expectedTags :-> "csid"
              , RunChangeSet "csid" :-> () ]

        it "does not provide prod-only params when not in prod" $ do
          runSuccess $ cli config
            & stubFileSystemT
              [ ("base.yaml", template) ]
            & stubEnvironmentT []
            & stubArgumentsT baseTestArgs
            & stubExceptT
            & mockCloudT
              [ ComputeChangeset "test-foo-base" template expectedParamsTest rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> () ]
    