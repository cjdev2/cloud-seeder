module Network.CloudSeeder.CommandLineSpec (spec) where

import Options.Applicative (ParserResult(..), ParserInfo(..), execParserPure, defaultPrefs, getParseResult)
import Test.Hspec

import Network.CloudSeeder.CommandLine

spec :: Spec
spec = do
  let runParser :: ParserInfo a -> [String] -> ParserResult a
      runParser = execParserPure defaultPrefs

  describe "Command line" $ do
    let command = ["deploy", "stack", "env"]
    describe "argument parsing" $ do
      it "parses a command with no options after" $ do
        let expected = DeployStack "stack" "env"
            parsed = getParseResult $ runParser parseArguments command
        parsed `shouldBe` Just expected

      it "parses a command and ignores any provided options" $ do
        let expected = DeployStack "stack" "env"
            input = command ++ ["--foo", "val"]
            parsed = getParseResult $ runParser parseArguments input
        parsed `shouldBe` Just expected

      it "produces help for --help if not all the arguments are supplied" $ do
        let input = ["deploy", "stack", "--help"]
            parsed = getParseResult $ runParser parseArguments input
        parsed `shouldBe` Nothing

      it "defers help for --help if all the arguments are supplied" $ do
        let expected = DeployStack "stack" "env"
            input = command ++ ["--help"]
            parsed = getParseResult $ runParser parseArguments input
        parsed `shouldBe` Just expected

    describe "optional parsing" $ do
      it "parses an optional parameter" $ do
        let flags = [Optional "foo" "defVal"]
            input = command ++ ["--foo", "val"]
            expected = [("foo", "val")]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Just expected

      it "returns a default value if an optional parameter is not present" $ do
        let flags = [Optional "bar" "defVal"]
            input = command
            expected = [("bar", "defVal")]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Just expected

      it "parses a required parameter" $ do
        let flags = [Required "flag"]
            input = command ++ ["--flag", "val"]
            expected = [("flag", "val")]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Just expected

      it "fails if a required parameter is not present" $ do
        let flags = [Required "flag"]
            input = command ++ ["--notFlag", "boop"]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Nothing
