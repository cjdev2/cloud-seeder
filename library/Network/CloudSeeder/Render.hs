module Network.CloudSeeder.Render
  ( indent
  , render
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.AWS.CloudFormation as CF

import Control.Lens ((^.))
import Data.Semigroup ((<>))
import Network.AWS.Data.Text (toText)

import Network.CloudSeeder.Types

indent :: Int -> T.Text -> T.Text
indent n msg = T.concat (replicate n " ") <> msg

class Render a where
  render :: a -> T.Text

instance Render T.Text where
  render = id

instance Render String where
  render = T.pack

instance (Render a, Render b) => Render (a, b) where
  render (k,v) = render k <> ": " <> render v

instance (Render a, Render b) => Render (M.Map a b) where
  render = render . M.toList

instance (Render a) => Render (Maybe a) where
  render (Just a) = render a
  render Nothing = ""

instance (Render a) => Render [a] where
  render [] = "[]"
  render xs = "- " <> T.intercalate "\n- " (render <$> xs) <> "\n"

instance Render Stack where
  render s = T.unlines $ T.stripEnd <$>
    [ "Stack Info: " ]
    <> (indent 2 <$>
    [ "Name: " <> s^.name
    , "Status: " <> render (s^.stackStatus)
    , "Outputs: " <> render (s^.outputs) ])

instance Render ChangeSet where
  render cs = T.unlines $ T.stripEnd <$>
    [ "Change Set Info: " ]
    <> (indent 2 <$>
    [ "ID: " <> cs ^. csId
    , "Status: " <> render (cs ^. executionStatus)
    , "Parameters: " <> render (cs ^. parameters)
    , "Changes: " <> render (cs ^. changes) ])

instance Render Parameter where
  render (Parameter (key, val)) = case val of
    Value v -> render (key, render v)
    UsePreviousValue -> "UsePreviousValue"

instance Render Change where
  render (Add x) = render x
  render (Remove x) = render x
  render (Modify x) = render x

instance Render ChangeAdd where
  render c = T.unlines $
    [ "Add: " ]
    <> (indent 2 <$>
    [ "Logical ID: " <> c ^. logicalId
    , "Physical ID: " <> render (c ^. physicalId)
    , "Resource Type: " <> c ^. resourceType ])

instance Render ChangeRemove where
  render c = T.unlines $
    [ "Remove: " ]
    <> (indent 2 <$>
    [ "Logical ID: " <> c ^. logicalId
    , "Physical ID: " <> render (c ^. physicalId)
    , "Resource Type: " <> c ^. resourceType ])

instance Render ChangeModify where
  render c = T.unlines $
    [ "Modify: " ]
    <> (indent 2 <$>
    [ "Logical ID: " <> c ^. logicalId
    , "Physical ID: " <> render (c ^. physicalId)
    , "Resource Type: " <> c ^. resourceType
    , "Scope: " <> T.concat ((\s -> "\n  - " <> render s) <$> (c ^. scope))
    , "Details: " <> T.unlines ((\d -> "\n - " <> render d) <$> (c ^. details))
    , "Replacement: " <> render (c ^. replacement) ])

instance Render CF.ResourceChangeDetail where
  render d = T.unlines
    [ "Causing Entity: " <> render (d ^. CF.rcdCausingEntity)
    , "Change Source: " <> render (d ^. CF.rcdChangeSource)
    , "Evaluation: " <> render (d ^. CF.rcdEvaluation)
    , "Target: " <> render (d ^. CF.rcdTarget)
    ]

instance Render CF.ResourceTargetDefinition where
  render t = T.unlines
    [ "Attribute: " <> render (t ^. CF.rtdAttribute)
    , "Requires Recreation: " <> render (t ^. CF.rtdRequiresRecreation)
    , "Name: " <> render (t ^. CF.rtdName)
    ]

instance Render CF.ChangeSource where
  render = toText
instance Render CF.EvaluationType where
  render = toText
instance Render CF.ExecutionStatus where
  render = toText
instance Render CF.Replacement where
  render = toText
instance Render CF.RequiresRecreation where
  render = toText
instance Render CF.ResourceAttribute where
  render = toText
instance Render CF.StackStatus where
  render = toText
