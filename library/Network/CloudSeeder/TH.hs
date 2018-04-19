module Network.CloudSeeder.TH
  ( capitalize
  , capTagOptions
  ) where

import Data.Aeson.TH (Options(..), defaultOptions)
import Data.Char (toUpper)

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize _ = ""

capTagOptions :: Options
capTagOptions = defaultOptions{constructorTagModifier = capitalize}
