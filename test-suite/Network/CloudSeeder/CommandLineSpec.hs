
module Network.CloudSeeder.CommandLineSpec (spec) where

import Options.Applicative
import Options.Applicative.Help.Types
import Test.Hspec

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Types

spec :: Spec
spec = do
  let runParser :: ParserInfo a -> [String] -> ParserResult a
      runParser = execParserPure defaultPrefs

  describe "Command line" $ do
    let provisionStackEnv = ["provision", "stack", "env"]
    describe "argument parsing" $ do
      describe "provision" $ do
        it "parses a provisionStackEnv with no options after" $ do
            let expected = ProvisionStack "stack" "env"
                parsed = getParseResult $ runParser (parseArguments []) provisionStackEnv
            parsed `shouldBe` Just expected

        it "parses a provisionStackEnv and ignores any provided options" $ do
            let expected = ProvisionStack "stack" "env"
                input = provisionStackEnv ++ ["--foo", "val"]
                parsed = getParseResult $ runParser (parseArguments []) input
            parsed `shouldBe` Just expected

        it "produces help for --help if not all the arguments are supplied that includes stacks that can be commanded" $ do
            let input = ["provision", "stack", "--help"]
                stacks = ["foo", "bar", "baz"]
                (Failure failure) = runParser (parseArguments stacks) input
                (h, _, _) = execFailure failure ""
                actual = renderHelp 0 h
                expected = "Usage:  provision STACK\n                  ENV\n  Provision\n  a\n  stack\n  in\n  an\n  environment\n\nSTACK\ncan\nbe\none\nof:\nfoo,\nbar,\nbaz"
            actual `shouldBe` expected

        it "defers help for --help if all the arguments are supplied" $ do
            let expected = ProvisionStack "stack" "env"
                input = provisionStackEnv ++ ["--help"]
                parsed = getParseResult $ runParser (parseArguments []) input
            parsed `shouldBe` Just expected

      describe "wait" $ do
        let waitCmd = ["wait", "stack", "env"]
        it "parses a provisionStackEnv with no options after" $ do
            let expected = Wait "stack" "env"
                parsed = getParseResult $ runParser (parseArguments []) waitCmd
            parsed `shouldBe` Just expected

    describe "optional parsing" $ do
      it "parses an optional parameter" $ do
        let flags = [Optional "foo" (Value "defVal")]
            input = provisionStackEnv ++ ["--foo", "val"]
            expected = Options [("foo", Value "val")] False
        parsed <- handleParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "returns a default value if an optional parameter is not present" $ do
        let flags = [Optional "bar" (Value "defVal")]
            input = provisionStackEnv
            expected = Options [("bar", Value "defVal")] False
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Just expected

      it "parses a required parameter" $ do
        let flags = [Required "flag"]
            input = provisionStackEnv ++ ["--flag", "val"]
            expected = Options [("flag", Value "val")] False
        parsed <- handleParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "fails if a required parameter is not present" $ do
        let flags = [Required "flag"]
            input = provisionStackEnv ++ ["--notFlag", "boop"]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Nothing
