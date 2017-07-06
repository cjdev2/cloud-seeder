module Network.CloudSeeder.CommandLineSpec (spec) where

import Data.Maybe (fromJust)
import Options.Applicative (defaultPrefs, execParserPure)
import Options.Applicative.Extra (getParseResult)
import Test.Hspec

import Network.CloudSeeder.CommandLine

spec :: Spec
spec = do 
  let parse p = getParseResult . execParserPure defaultPrefs p
  describe "Parse command line" $ do 
    it "parses a simple deploy command" $ do 
      let input = ["deploy", "stack", "env"]
          expected = (DeployStack "stack" "env" [])
      let parsed = fromJust $ parse parseCommandWithInfo input
      parsed `shouldBe` expected
