module Network.CloudSeeder.TeardownSpec (spec) where

import Control.Lens ((&), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Functor.Identity (runIdentity)
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Teardown
import Network.CloudSeeder.Test.Orphans ()
import Network.CloudSeeder.Test.Stubs

spec :: Spec
spec =
  describe "Teardown" $ do
    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)

        simpleConfig = DeploymentConfiguration "foo" []
          [ StackConfiguration "base" [] [] False [] Nothing
          , StackConfiguration "server" [] [] False [] Nothing
          , StackConfiguration "frontend" [] [] False [] Nothing
          ] []

    describe "teardownCommand" $ do
      describe "failures" $
        it "fails if user attempts to teardown a stack that doesn't exist in the config" $
          runFailure _CliStackNotConfigured "snipe"
            $ teardownCommand (pure simpleConfig) "snipe" "test"
            & ignoreLoggerT
            & mockActionT []
            & stubExceptT

      describe "happy path" $
        it "tears down a stack that exists in the config" $ example $
          runSuccess $ teardownCommand (pure simpleConfig) "base" "test"
            & ignoreLoggerT
            & mockActionT
              [ DeleteStack "test-foo-base" :-> () ]
            & stubExceptT