module Network.CloudSeeder.CommandLineSpec (spec) where

import Data.Maybe (fromJust)
import Options.Applicative (ParserResult(..), ParserInfo(..), execParserPure, defaultPrefs, getParseResult)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Set as S

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.Types

spec :: Spec
spec = do 
  let runParser :: ParserInfo a -> [String] -> ParserResult a
      runParser p = execParserPure defaultPrefs p

  describe "Command line" $ do 
    let command = ["deploy", "stack", "env"]
    describe "argument parsing" $ do 
      it "parses a command with no options after" $ do 
        let expected = DeployStack "stack" "env"
            parsed = fromJust . getParseResult $ runParser parseArguments command
        parsed `shouldBe` expected

    describe "optional parsing" $ do 
      it "parses a required parameter" $ do 
        let flags :: S.Set (T.Text, ParameterSource)
            flags = [("flag", Flag)]
            input = command ++ ["--flag", "val"]
            expected = [("flag", "val")]
            parsed = fromJust . getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` expected

      it "fails if a required parameter is not present" $ do 
        let flags :: S.Set (T.Text, ParameterSource)
            flags = [("flag", Flag)]
            input = command ++ ["--notFlag", "boop"]
            parsed = getParseResult $ runParser (parseOptions flags) input
        parsed `shouldBe` Nothing
