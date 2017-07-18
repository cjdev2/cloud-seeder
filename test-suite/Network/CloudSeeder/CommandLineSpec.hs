module Network.CloudSeeder.CommandLineSpec (spec) where

import Data.Maybe (fromJust)
import Options.Applicative (ParserResult(..), ParserInfo(..), execParserPure, defaultPrefs, getParseResult)
import Test.Hspec

import Network.CloudSeeder.CommandLine

spec :: Spec
spec = do
  let runParser :: ParserInfo a -> [String] -> ParserResult a
      runParser = execParserPure defaultPrefs

  describe "Command line" $ do
    let command = ["deploy", "stack", "env"]
    describe "argument parsing" $
      it "parses a command with no options after" $ do
        let expected = DeployStack "stack" "env"
            parsed = fromJust . getParseResult $ runParser parseArguments command
        parsed `shouldBe` expected

    describe "optional parsing" $ do
      it "parses an optional parameter" $ do
        let flags = [Optional "foo" "defVal"]
            input = command ++ ["--foo", "val"]
            expected = [("foo", "val")]
            parsed = fromJust . getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "returns a default value if an optional parameter is not present" $ do
        let flags = [Optional "bar" "defVal"]
            input = command
            expected = [("bar", "defVal")]
            parsed = fromJust . getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "parses a required parameter" $ do
        let flags = [Required "flag"]
            input = command ++ ["--flag", "val"]
            expected = [("flag", "val")]
            parsed = fromJust . getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "fails if a required parameter is not present" $ do
        let flags = [Required "flag"]
            input = command ++ ["--notFlag", "boop"]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Nothing
