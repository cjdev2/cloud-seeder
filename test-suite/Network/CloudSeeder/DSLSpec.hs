module Network.CloudSeeder.DSLSpec (spec) where

import Control.Lens ((^.), (^..), each)
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import Network.CloudSeeder.DSL

spec :: Spec
spec =
  describe "deployment" $ do
    it "creates a DeploymentConfiguration with the given name" $ do
      let config = runIdentity $ deployment "foobar" $ return ()
      config ^. name `shouldBe` "foobar"

    describe "environment" $ do
      it "adds environment variables to a DeploymentConfiguration" $ do
        let vars = ["foo", "bar", "baz"]
            config = runIdentity $ deployment "" $ environment vars
        config ^. environmentVariables `shouldBe` vars

      it "adds to the environment variables that are already there" $ do
        let config = runIdentity $ deployment "" $ do
              environment ["foo"]
              environment ["bar"]
        config ^. environmentVariables `shouldBe` ["foo", "bar"]

    describe "stack_" $ do
      it "registers a stack with the given name" $ do
        let config = runIdentity $ deployment "" $ stack_ "foo"
        config ^.. stacks.each.name `shouldBe` ["foo"]

      it "adds multiple stacks to the DeploymentConfiguration" $ do
        let config = runIdentity $ deployment "" $ do
              stack_ "foo"
              stack_ "bar"
              stack_ "baz"
        config ^.. stacks.each.name `shouldBe` ["foo", "bar", "baz"]

    describe "stack" $ do
      describe "environment" $ do
        it "configures the current stack to use the given env variables" $ do
          let vars = ["foo", "bar", "baz"]
              config = runIdentity $ deployment "" $ stack "foo" (environment vars)
          config ^.. stacks.each.environmentVariables `shouldBe` [["foo", "bar", "baz"]]
