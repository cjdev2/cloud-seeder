module CloudSeeder.CoreSpec where

import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import CloudSeeder.Core
import CloudSeeder.Test.Stubs

spec :: Spec
spec = describe "main" $ do
  let ((), logMessages) = runIdentity $ main
        & runDeployT [ (StackName "test-stack", "it's runnin")]
        & runArgumentsT (Opts "test-stack")
        & runLoggerT

  it "prints the state of test-stack" $
    head logMessages `shouldBe` "it's runnin"

  let ((), logMessages2) = runIdentity $ main
        & runDeployT [ (StackName "test-stack", "it's runnin")]
        & runArgumentsT (Opts "stack-that-doesn't-exist")
        & runLoggerT

  it "prints an error when stack doesn't exist" $
    head logMessages2 `shouldBe` "no stack exists with name: stack-that-doesn't-exist"
