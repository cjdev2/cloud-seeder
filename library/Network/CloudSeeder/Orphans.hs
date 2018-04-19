{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.CloudSeeder.Orphans () where

import Data.Aeson.TH

import qualified Network.AWS.CloudFormation as CF

import Network.CloudSeeder.TH

$(deriveJSON capTagOptions ''CF.ChangeSource)
$(deriveJSON capTagOptions ''CF.ExecutionStatus)
$(deriveJSON capTagOptions ''CF.EvaluationType)
$(deriveJSON capTagOptions ''CF.StackStatus)
$(deriveJSON capTagOptions ''CF.Replacement)
$(deriveJSON capTagOptions ''CF.RequiresRecreation)
$(deriveJSON capTagOptions ''CF.ResourceAttribute)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = capitalize} ''CF.ResourceChangeDetail)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = capitalize} ''CF.ResourceTargetDefinition)
