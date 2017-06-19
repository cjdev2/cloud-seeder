module CloudSeeder.CoreSpec where

import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import CloudSeeder.Core
import CloudSeeder.Test.Stubs

mainTest :: Options -> (((), DeployState), [ByteString])
mainTest options = runIdentity $ main
  & runDeployT
    [ (StackName "test-stack", Right $ Stack (StackName "test-stack") "" [("","")])
    , (StackName "test-stack-2", Right $ Stack (StackName "test-stack-2") "" [("","")]
    ) ]
  & runFileSystemT [("/files/template.yaml", "template contents")]
  & runLoggerT
  & runArgumentsT options


spec :: Spec
spec = parallel $ describe "main" $ do
  it "prints the state of test-stack" $ do
    let (_, logMessages) = mainTest (Options $ DescribeStack $ DescribeOptions "test-stack")
    head logMessages `shouldBe` "stack test-stack deployed"

  it "prints an error when stack doesn't exist" $ do
    let (_, logMessages) = mainTest (Options $ DescribeStack $ DescribeOptions "stack-that-doesn't-exist")
    head logMessages `shouldBe` "no stack exists with name: stack-that-doesn't-exist"

  it "prints a stack id when deployment succeeds" $ do
    let (_, logMessages) = mainTest
          ( Options $ DeployStack $ DeployOptions
              "test-stack-3"
              "/files/template.yaml"
              [("","")]
          )
    head logMessages `shouldBe` "stackId 123"

  it "errors when template file doesn't exist" $
    (evaluate . force) (mainTest
      ( Options $ DeployStack $ DeployOptions
        "test-stack-3"
        "/files/nothing-here.yaml"
        [("","")]
      )) `shouldThrow` anyErrorCall

  it "parameterizes the stack" $ do
    let ((_, DeployState state), _) = mainTest
          ( Options $ DeployStack $ DeployOptions
            "test-stack-4"
            "/files/template.yaml"
            [("param","marap")]
          )
    let result = case lookup (StackName "test-stack-4") state of
          Just (Right (Stack _ _ params)) -> params
          _ -> fail "state didn't look right"
    result `shouldBe` [("param","marap")]
