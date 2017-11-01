module Network.CloudSeeder.ProvisionSpec (spec) where

import Control.Lens ((&), review)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Mock (WithResult(..))
import Data.Functor.Identity (runIdentity)
import Data.Semigroup ((<>))
import Network.AWS.CloudFormation (StackStatus(..))
import Test.Hspec

import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Provision
import Network.CloudSeeder.Types
import Network.CloudSeeder.Test.Stubs

import qualified Data.Text as T
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B

spec :: Spec
spec =
  describe "Provision" $ do

    let stubExceptT :: ExceptT CliError m a -> m (Either CliError a)
        stubExceptT = runExceptT
        runSuccess x = runIdentity x `shouldBe` Right ()
        runFailure errPrism errContents action =
          runIdentity action `shouldBe` Left (review errPrism errContents)

        rootTemplate = "Parameters:\n"
                    <> "  Env:\n"
                    <> "    Type: String\n"
        rootExpectedTags = [("cj:application", "foo"), ("cj:environment", "test")]
        rootExpectedParams = [("Env", Value "test")]

        expectedStack :: T.Text -> StackStatus -> Stack
        expectedStack stackName = Stack
          Nothing
          (Just "csId")
          stackName
          []
          ["Env"]
          (Just "sId")

        expectedStackInfo :: B.ByteString -> StackStatus -> B.ByteString
        expectedStackInfo stackName status = B.unlines
          [ "Stack Info:"
          , "  name: " <> stackName
          , "  status: " <> B.pack (show status)
          , "  outputs: fromList []"
          ]

        config = DeploymentConfiguration "foo" [] [StackConfiguration "base" [] [] False []] []
        mConfig = pure config

    describe "provisionCommand" $ do
      it "fails if the template doesn't exist" $
        runFailure _FileNotFound "base.yaml" $ provisionCommand mConfig "base" "test" []
          & stubFileSystemT []
          & stubEnvironmentT []
          & ignoreLoggerT
          & mockActionT []
          & stubExceptT

      it "fails if the template parameters can't be parsed" $ do
        let err = "YAML parse exception at line 0, column 8,\nwhile scanning a directive:\nfound unknown directive name"
        runFailure _CliTemplateDecodeFail err $ provisionCommand mConfig "base" "test" []
          & stubFileSystemT [("base.yaml", "%invalid")]
          & stubEnvironmentT []
          & ignoreLoggerT
          & mockActionT []
          & stubExceptT

      it "fails if user attempts to deploy a stack that doesn't exist in the config" $
        runFailure _CliStackNotConfigured "snipe" $ provisionCommand mConfig "snipe" "test" []
          & stubFileSystemT []
          & stubEnvironmentT []
          & ignoreLoggerT
          & mockActionT []
          & stubExceptT

      it "fails if parameters required in the template are not supplied" $ do
        let baseTemplate =
                rootTemplate
              <> "  foo:\n"
              <> "    Type: String\n"
              <> "  bar:\n"
              <> "    Type: String\n"
        runFailure _CliMissingRequiredParameters ["foo", "bar"] $ provisionCommand mConfig "base" "test" ["provision", "test", "base"]
          & stubFileSystemT [("base.yaml", baseTemplate)]
          & stubEnvironmentT []
          & ignoreLoggerT
          & mockActionT [ DescribeStack "test-foo-base" :-> Nothing ]
          & stubExceptT

      context "the configuration does not have environment variables" $
        it "applies a changeset to a stack" $ example $
          runSuccess $ provisionCommand mConfig "base" "test" ["provision", "test", "base"]
            & stubFileSystemT
              [("base.yaml", rootTemplate)]
            & stubEnvironmentT []
            & ignoreLoggerT
            & mockActionT
              [ DescribeStack "test-foo-base" :-> Nothing
              , ComputeChangeset "test-foo-base" CreateStack rootTemplate rootExpectedParams rootExpectedTags :-> "csid"
              , RunChangeSet "csid" :-> 200 ]
            & stubExceptT
