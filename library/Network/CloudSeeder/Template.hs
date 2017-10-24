{-# LANGUAGE TemplateHaskell #-}

-- |The `Template` module exports functions dealing with parsing YAML CloudFormation templates.
module Network.CloudSeeder.Template
  ( Template(..)
  , HasParameterSpecs(..)
  ) where

import Control.Lens (makeFields)
import Data.Aeson.Types (typeMismatch)
import Data.Yaml (FromJSON(..), Value(..), (.:))

import Network.CloudSeeder.Types

newtype Template = Template
  { _templateParameterSpecs :: ParameterSpecs
  } deriving (Eq, Show)
makeFields ''Template

instance FromJSON Template where
  parseJSON (Object v) = Template
    <$> v .: "Parameters"
  parseJSON invalid = typeMismatch "Template" invalid
