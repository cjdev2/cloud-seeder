module Network.CloudSeeder.DSLSpec (spec) where

import Control.Lens ((^.), (^..), each)
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import Network.CloudSeeder.DSL

spec :: Spec
spec = do
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
      
      describe "tags" $ do 
        it "adds environment variables to a DeploymentConfiguration" $ do
          let tagz = [ ("tag1", "val1"), ("tag2", "val2") ]
              config = runIdentity $ deployment "" $ tags tagz
          config ^. tagSet `shouldBe` tagz

        it "adds to the environment variables that are already there" $ do
          let tags1 = [("foo", "bar")]
              tags2 = [("baz", "qux"), ("bop", "dop")]
              config = runIdentity $ deployment "" $ do
                tags tags1
                tags tags2
          config ^. tagSet `shouldBe` (tags1 ++ tags2)

        it "tags the current stack with the provided key value pairs" $ do 
          let tagz = [("foo", "bar"), ("baz", "qux")]
              config = runIdentity $ deployment "" $ stack "foo" (tags tagz)
          config ^.. stacks.each.tagSet `shouldBe` [tagz]
