{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.CloudSeeder.Types
  ( ParameterSource(..)
  , AsParameterSource(..)

  , ProvisionType(..)
  , AsProvisionType(..)

  , ParameterValue(..)
  , AsParameterValue(..)

  , ParameterSpec(..)
  , AsParameterSpec(..)
  , ParameterSpecs(..)
  , parameterKey

  , ParameterMap(..)

  , StackName(..)

  , Stack(..)
  , HasStackStatusReason(..)
  , HasChangeSetId(..)
  , HasOutputs(..)
  , HasStackId(..)
  , minimalStack

  , HasParameters(..)
  , HasStackStatus(..)
  , HasName(..)

  , ChangeAdd(..)
  , ChangeModify(..)
  , ChangeRemove(..)
  , Change(..)

  , HasLogicalId(..)
  , HasPhysicalId(..)
  , HasResourceType(..)
  , HasScope(..)
  , HasDetails(..)
  , HasReplacement(..)

  , ChangeSet(..)
  , HasCsId(..)
  , HasExecutionStatus(..)
  , HasChanges(..)
  , HasStatusReason(..)
  , minimalChangeSet

  , Parameter(..)
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens, makeFields, makeClassyPrisms, makeWrapped)
import Data.Aeson.Types (typeMismatch)
import Data.String (IsString)
import Data.Yaml (FromJSON(..), Parser, Value(..), (.:?))
import GHC.Generics (Generic)
import Network.AWS.CloudFormation.Types (ExecutionStatus(..))

import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Network.AWS.CloudFormation as CF

newtype StackName = StackName T.Text
  deriving (Eq, Show, Generic, IsString)
instance NFData StackName

data Stack = Stack
  { _stackStackStatusReason :: Maybe T.Text
  , _stackChangeSetId :: Maybe T.Text
  , _stackName :: T.Text
  , _stackOutputs :: M.Map T.Text T.Text
  , _stackParameters :: S.Set T.Text
  , _stackStackId :: Maybe T.Text
  , _stackStackStatus :: CF.StackStatus
  }
  deriving (Eq, Show, Ord)
makeFields ''Stack

minimalStack :: T.Text -> CF.StackStatus -> Stack
minimalStack name' stackStatus' = Stack Nothing Nothing name' mempty mempty Nothing stackStatus'

data ParameterSource
  = Constant T.Text -- ^ @'Constant' "param value"@
  | Env
  | Flag
  | Outputs
  deriving (Eq, Show, Ord)
makeClassyPrisms ''ParameterSource

data ProvisionType
  = CreateStack
  | UpdateStack (S.Set T.Text) -- ^ @'Update' "list of parameter keys"@
  deriving (Eq, Show, Ord)
makeClassyPrisms ''ProvisionType

data ParameterValue
  = UsePreviousValue
  | Value T.Text
  deriving (Eq, Show, Ord)
makeClassyPrisms ''ParameterValue

data ParameterSpec
  = Required T.Text
  | Optional T.Text ParameterValue
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
        let defParser :: FromJSON a => Parser (Maybe a)
            defParser = pSpec .:? "Default"
        defVal <- defParser
              -- try parsing as a double if parsing fails as a string
              <|> fmap (fmap (T.pack . show)) (defParser @Double)
        pure $ maybe (Required k) (\v -> Optional k (Value v)) defVal
      parseParamSpec (k, invalid) = typeMismatch (T.unpack k) invalid
  parseJSON invalid = typeMismatch "Parameters" invalid

newtype ParameterMap = ParameterMap (M.Map T.Text (ParameterSource, T.Text))
  deriving (Eq, Show)

newtype Parameter = Parameter (T.Text, ParameterValue)
  deriving (Eq, Show, Ord)

data ChangeAdd = ChangeAdd
  { _changeAddLogicalId :: T.Text
  , _changeAddPhysicalId :: T.Text
  , _changeAddResourceType :: T.Text
  } deriving (Eq, Show)
makeFields ''ChangeAdd

data ChangeModify = ChangeModify
  { _changeModifyLogicalId :: T.Text
  , _changeModifyPhysicalId :: T.Text
  , _changeModifyResourceType :: T.Text
  , _changeModifyScope :: [CF.ResourceAttribute]
  , _changeModifyDetails :: [CF.ResourceChangeDetail]
  , _changeModifyReplacement :: CF.Replacement
  } deriving (Eq, Show)
makeFields ''ChangeModify

data ChangeRemove = ChangeRemove
  { _changeRemoveLogicalId :: T.Text
  , _changeRemovePhysicalId :: T.Text
  , _changeRemoveResourceType :: T.Text
  } deriving (Eq, Show)
makeFields ''ChangeRemove

data Change
  = Add ChangeAdd
  | Modify ChangeModify
  | Remove ChangeRemove
  deriving (Show, Eq)
makeClassyPrisms ''Change

data ChangeSet = ChangeSet
  { _changeSetStatusReason :: Maybe T.Text
  , _changeSetCsId :: T.Text
  , _changeSetParameters :: [Parameter]
  , _changeSetExecutionStatus :: ExecutionStatus
  , _changeSetChanges :: [Change]
  }
  deriving (Eq, Show)
makeFields ''ChangeSet

minimalChangeSet :: T.Text -> ExecutionStatus -> ChangeSet
minimalChangeSet csid execStatus = ChangeSet Nothing csid [] execStatus []
