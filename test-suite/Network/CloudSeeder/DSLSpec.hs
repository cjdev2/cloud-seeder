module Network.CloudSeeder.DSLSpec (spec) where

import qualified Data.Text as T

import Control.Lens ((^.), (^..), each)
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import GHC.Exts (IsList(..))
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Types

type TagList = forall a. (IsList a, Item a ~ (T.Text, T.Text)) => a

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
        config ^. parameterSources `shouldBe` [("foo", Env), ("bar", Env), ("baz", Env)]

      it "adds to the environment variables that are already there" $ do
        let config = runIdentity $ deployment "" $ do
              environment ["foo"]
              environment ["bar"]
        config ^. parameterSources `shouldBe` [("foo", Env), ("bar", Env)]

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
      describe "environment" $
        it "configures the current stack to use the given env variables" $ do
          let vars = ["foo", "bar", "baz"]
              config = runIdentity $ deployment "" $ stack "foo" (environment vars)
          config ^.. stacks.each.parameterSources `shouldBe` [[("foo", Env), ("bar", Env), ("baz", Env)]]

      describe "tags" $ do
        it "adds tags to a DeploymentConfiguration" $ do
          let tagz :: TagList
              tagz = [ ("tag1", "val1"), ("tag2", "val2") ]
              config = runIdentity $ deployment "" $ tags tagz
          config ^. tagSet `shouldBe` tagz

        it "adds to the environment variables that are already there" $ do
          let tags1 :: TagList
              tags1 = [("foo", "bar")]
              tags2 :: TagList
              tags2 = [("baz", "qux"), ("bop", "dop")]
              config = runIdentity $ deployment "" $ do
                tags tags1
                tags tags2
          config ^. tagSet `shouldBe` (tags1 <> tags2)

        it "tags the current stack with the provided key value pairs" $ do
          let tagz :: TagList
              tagz = [("foo", "bar"), ("baz", "qux")]
              config = runIdentity $ deployment "" $ stack "foo" (tags tagz)
          config ^.. stacks.each.tagSet `shouldBe` [tagz]

      describe "flags" $ do
        it "adds flag variables to a DeploymentConfiguration" $ do
          let config = runIdentity $ deployment "" $ flag "baz"
          config ^. parameterSources `shouldBe` [("baz", Flag)]

        it "configures the current stack to require the given flags" $ do
          let config = runIdentity $ deployment "" $ stack "foo" (flag "foo")
          config ^.. stacks.each.parameterSources `shouldBe` [[("foo", Flag)]]

      describe "global" $
        it "marks a given stack to be provisioned globally" $ do
          let config = runIdentity $ deployment "foo" $ stack "repo" global
          config ^.. stacks.each.globalStack `shouldBe` [True]
