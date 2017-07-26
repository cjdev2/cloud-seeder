{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.CloudSeeder.Types
  ( ParameterSource(..)
  , AsParameterSource(..)

  , Parameter(..)

  , ParameterSpec(..)
  , AsParameterSpec(..)
  , ParameterSpecs(..)
  , parameterKey

  , ParameterMap(..)
  ) where

import Control.Lens (Lens', lens, makeClassyPrisms, makeWrapped)
import Data.Aeson.Types (typeMismatch)
import Data.Yaml (FromJSON(..), Value(..), (.:?))

import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

data ParameterSource
  = Constant T.Text -- ^ @'Constant' "param value"@
  | Env
  | Flag
  | Outputs
  | PreviousValue
  deriving (Eq, Show, Ord)

makeClassyPrisms ''ParameterSource

data Parameter = Parameter ParameterSource T.Text T.Text
  deriving (Eq, Show)

data ParameterSpec
  = Required T.Text
  | Optional T.Text T.Text
  deriving (Eq, Show, Ord)

makeClassyPrisms ''ParameterSpec

parameterKey :: Lens' ParameterSpec T.Text
parameterKey = lens get set
  where
    get (Required x) = x
    get (Optional x _) = x

    set (Required _) x = Required x
    set (Optional _ y) x = Optional x y


newtype ParameterSpecs = ParameterSpecs (S.Set ParameterSpec)
  deriving (Eq, Show, Ord)

makeWrapped ''ParameterSpecs

instance FromJSON ParameterSpecs where
  parseJSON (Object pSpecs) =
    ParameterSpecs . S.fromList <$> mapM parseParamSpec (H.toList pSpecs)
    where
      parseParamSpec (k, Object pSpec) = do
        defVal <- pSpec .:? "Default"
        return $ maybe (Required k) (Optional k) defVal
      parseParamSpec (k, invalid) = typeMismatch (T.unpack k) invalid
  parseJSON invalid = typeMismatch "Parameters" invalid

newtype ParameterMap = ParameterMap (M.Map T.Text (ParameterSource, T.Text))
  deriving (Eq, Show)
