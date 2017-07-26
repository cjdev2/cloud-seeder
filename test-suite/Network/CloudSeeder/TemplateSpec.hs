module Network.CloudSeeder.TemplateSpec (spec) where

import Data.ByteString.Char8 (pack)
import Data.Yaml (decode)
import Test.Hspec

import Network.CloudSeeder.Template
import Network.CloudSeeder.Types

spec :: Spec
spec = do
  describe "template parsing" $ do
    it "parses a template with required parameters" $ do
      let template
             = "Parameters:\n"
            ++ "  Env:\n"
            ++ "    Type: blah\n"
            ++ "  Foo:\n"
            ++ "    Type: blah\n"
          expected = Template $ ParameterSpecs [Required "Env", Required "Foo"]
          parsed = decode $ pack template
      parsed `shouldBe` Just expected

    it "parses a template with optional parameters" $ do
      let template
             = "Parameters:\n"
            ++ "  Env:\n"
            ++ "    Type: boop\n"
            ++ "    Default: test\n"
            ++ "  Foo:\n"
            ++ "    Type: boop\n"
            ++ "    Default: bar\n"
          expected = Template $ ParameterSpecs [Optional "Env" "test", Optional "Foo" "bar"]
          parsed = decode $ pack template
      parsed `shouldBe` Just expected

    it "parses default values that are numbers" $ do
      let template
             = "Parameters:\n"
            ++ "  Port:\n"
            ++ "    Type: Number\n"
            ++ "    Default: 42\n"
          expected = Template $ ParameterSpecs [Optional "Port" "42.0"]
          parsed = decode $ pack template
      parsed `shouldBe` Just expected
