module Network.CloudSeeder.RenderSpec (spec) where

import Network.AWS.CloudFormation.Types (ExecutionStatus(..))
import Network.AWS.Data.Text (toText)
import Test.Hspec

import Network.CloudSeeder.Render
import Network.CloudSeeder.Types

import qualified Network.AWS.CloudFormation as CF
import qualified Data.Map as M
import qualified Data.Text as T

spec :: Spec
spec =
  describe "Render" $ do
    context "helper functions" $
      context "indent" $
        it "indents by the given int" $
          indent 2 "hello" `shouldBe` "  hello"

    context "Render instances" $ do
      context "(T.Text, T.Text)" $
        it "renders correctly" $ do
          let x :: (T.Text, T.Text) = ("key","value")
          render x `shouldBe` "key: value"

      context "M.Map T.Text T.Text" $
        it "renders correctly" $ do
          let x :: M.Map T.Text T.Text = M.fromList [("key1", "val1"), ("key2", "val2")]
          let result = T.unlines
                [ "- key1: val1"
                , "- key2: val2"]
          render x `shouldBe` result

      context "[a]" $ do
        it "defers to the render method of each list item" $ do
          let x :: [T.Text] = ["hello", "yo", "sup"]
          let result = T.unlines
                [ "- hello"
                , "- yo"
                , "- sup" ]
          render x `shouldBe` result

        it "renders an empty list" $
          render ([] :: [T.Text]) `shouldBe` "[]"

      context "Stack" $
        it "renders minimal stack" $ do
          let x = minimalStack "boop" CF.SSCreateComplete
          let result = T.unlines
                [ "Stack Info:"
                , "  Name: boop"
                , "  Status: CREATE_COMPLETE"
                , "  Outputs: []"
                ]
          render x `shouldBe` result

      context "ChangeSet" $
        it "renders a minimal change set" $ do
          let x = minimalChangeSet "csid" Available
          let result = T.unlines
                [ "Change Set Info:"
                , "  ID: csid"
                , "  Status: AVAILABLE"
                , "  Parameters: []"
                , "  Changes: []"
                ]
          render x `shouldBe` result

      context "Parameter" $ do
        it "renders a Parameter with UsePreviousValue" $ do
          let x = Parameter ("Env", UsePreviousValue)
          render x `shouldBe` "UsePreviousValue"

        it "renders a Parameter with a value" $ do
          let x = Parameter ("Env", Value "test")
          render x `shouldBe` "Env: test"

      context "ChangeAdd" $
        it "renders" $ do
          let x = ChangeAdd "logicalId" (Just "physicalId") "resourceType"
          let result = T.unlines
                [ "Add: "
                , "  Logical ID: logicalId"
                , "  Physical ID: physicalId"
                , "  Resource Type: resourceType"
                ]
          render x `shouldBe` result

      context "ChangeRemove" $
        it "renders" $ do
          let x = ChangeRemove "logicalId" (Just "physicalId") "resourceType"
          let result = T.unlines
                [ "Remove: "
                , "  Logical ID: logicalId"
                , "  Physical ID: physicalId"
                , "  Resource Type: resourceType"
                ]
          render x `shouldBe` result

      context "ChangeModify" $
        it "renders" $ do
          let x = ChangeModify "logicalId" (Just "physicalId") "resourceType" [CF.Metadata] [CF.resourceChangeDetail] CF.True'
          let result = T.unlines
                [ "Modify: "
                , "  Logical ID: logicalId"
                , "  Physical ID: physicalId"
                , "  Resource Type: resourceType"
                , "  Scope: "
                , "  - Metadata"
                , "  Details: "
                , "  - Causing Entity:"
                , "    Change Source:"
                , "    Evaluation:"
                , "    Target:"
                , "  Replacement: True"
                ]
          render x `shouldBe` result

-- instance Render CF.ResourceChangeDetail where
--   render d = T.unlines
--     [ "Causing Entity: " <> render (d ^. CF.rcdCausingEntity)
--     , "Change Source: " <> render (d ^. CF.rcdChangeSource)
--     , "Evaluation: " <> render (d ^. CF.rcdEvaluation)
--     , "Target: " <> render (d ^. CF.rcdTarget)
--     ]

-- instance Render CF.ResourceTargetDefinition where
--   render t = T.unlines
--     [ "Attribute: " <> render (t ^. CF.rtdAttribute)
--     , "Requires Recreation: " <> render (t ^. CF.rtdRequiresRecreation)
--     , "Name: " <> render (t ^. CF.rtdName)
--     ]
