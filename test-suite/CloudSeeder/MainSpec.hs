module CloudSeeder.MainSpec (spec) where

import Control.Lens (review)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import CloudSeeder.DSL
import CloudSeeder.Interfaces
import CloudSeeder.Main
import CloudSeeder.Test.Stubs

-- mainTest :: Options -> (((), DeployState), [ByteString])
-- mainTest options = runIdentity $ main
--   & runDeployT
--     [ (StackName "test-stack", Right $ Stack (StackName "test-stack") "" [("","")])
--     , (StackName "test-stack-2", Right $ Stack (StackName "test-stack-2") "" [("","")]
--     ) ]
--   & stubFileSystemT [("/files/template.yaml", "template contents")]
--   & runLoggerT
--   & runArgumentsT options


spec :: Spec
spec = parallel $ do
  -- describe "main" $ do
  --   it "prints the state of test-stack" $ do
  --     let (_, logMessages) = mainTest (Options $ DescribeStack $ DescribeOptions "test-stack")
  --     head logMessages `shouldBe` "stack test-stack deployed"
  --
  --   it "prints an error when stack doesn't exist" $ do
  --     let (_, logMessages) = mainTest (Options $ DescribeStack $ DescribeOptions "stack-that-doesn't-exist")
  --     head logMessages `shouldBe` "no stack exists with name: stack-that-doesn't-exist"
  --
  --   it "prints a stack id when deployment succeeds" $ do
  --     let (_, logMessages) = mainTest
  --           ( Options $ CL.DeployStack $ DeployOptions
  --               "test-stack-3"
  --               "/files/template.yaml"
  --               [("","")]
  --           )
  --     head logMessages `shouldBe` "stackId 123"
  --
  --   it "errors when template file doesn't exist" $
  --     (evaluate . force) (mainTest
  --       ( Options $ DeployStack $ DeployOptions
  --         "test-stack-3"
  --         "/files/nothing-here.yaml"
  --         [("","")]
  --       )) `shouldThrow` anyErrorCall
  --
  --   it "parameterizes the stack" $ do
  --     let ((_, DeployState state), _) = mainTest
  --           ( Options $ DeployStack $ DeployOptions
  --             "test-stack-4"
  --             "/files/template.yaml"
  --             [("param","marap")]
  --           )
  --     let result = case lookup (StackName "test-stack-4") state of
  --           Just (Right (Stack _ _ params)) -> params
  --           _ -> fail "invalid state"
  --     result `shouldBe` [("param","marap")]

    -- parallel $ describe "Interfaces" $ do
    --   describe "MonadDeploy" $ do
    --   describe "MonadArguments" $ do

    -- parallel $ describe "CommandLine" $ do

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
        & stubCloudT []

    it "fails if user attempts to deploy a stack that doesn't exist in the config" $ do
      let config = runIdentity $ deployment "foo" $ do
            stack_ "base"
          env = [("Env", "test")]
      runFailure _CliStackNotConfigured "foo" $ cli (DeployStack "foo") config
        & stubFileSystemT
          [ ("base.yaml", "base.yaml contents")]
        & stubExceptT
        & stubEnvironmentT env
        & stubCloudT []

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
          & stubCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" env :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "passes the outputs from prior stacks" $ do
        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Just [("foo", "bar")]
            , ComputeChangeset "test-foo-server" "server.yaml contents" (env ++ [("foo", "bar")]) :-> "csid"
            , RunChangeSet "csid" :-> () ]

        runSuccess $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Just [("foo", "bar")]
            , DescribeStack "test-foo-server" :-> Just [("baz", "qux")]
            , ComputeChangeset "test-foo-frontend" "frontend.yaml contents" (env ++ [("foo", "bar"), ("baz", "qux")]) :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails if a dependency stack does not exist" $ do
        runFailure _CliMissingDependencyStacks ["base"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Nothing
            , DescribeStack "test-foo-server" :-> Just [] ]

        runFailure _CliMissingDependencyStacks ["server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Just []
            , DescribeStack "test-foo-server" :-> Nothing ]

        runFailure _CliMissingDependencyStacks ["base", "server"] $ cli (DeployStack "frontend") config
          & stubFileSystemT
            [ ("frontend.yaml", "frontend.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Nothing
            , DescribeStack "test-foo-server" :-> Nothing ]

      it "fails when the Env environment variable is not specified" $ do
        runFailure _CliMissingEnvVars ["Env"] $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT []
          & stubCloudT []

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
          & stubCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" env :-> "csid"
            , RunChangeSet "csid" :-> () ]

      it "fails when a global environment variable is missing" $ do
        let env = [ ("Env", "test"), ("SecretsStore", "arn::aws:1234") ]
        runFailure _CliMissingEnvVars ["Domain"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT env
          & stubCloudT []

      it "reports all missing environment variables at once in alphabetical order" $ do
        runFailure _CliMissingEnvVars ["Domain", "Env", "SecretsStore"] $ cli (DeployStack "base") config
          & stubFileSystemT
            [ ("base.yaml", "base.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT []
          & stubCloudT []

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
          & stubCloudT
            [ ComputeChangeset "test-foo-base" "base.yaml contents" baseEnv :-> "csid"
            , RunChangeSet "csid" :-> () ]

        let serverEnv = env ++ [ ("Server1", "b"), ("Server2", "c") ]
        runSuccess $ cli (DeployStack "server") config
          & stubFileSystemT
            [ ("server.yaml", "server.yaml contents") ]
          & stubExceptT
          & stubEnvironmentT serverEnv
          & stubCloudT
            [ DescribeStack "test-foo-base" :-> Just []
            , ComputeChangeset "test-foo-server" "server.yaml contents" serverEnv :-> "csid"
            , RunChangeSet "csid" :-> () ]
