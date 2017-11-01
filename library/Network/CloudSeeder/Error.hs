{-# LANGUAGE TemplateHaskell #-}

module Network.CloudSeeder.Error
  ( CliError(..)
  , HasCliError(..)
  , AsCliError(..)
  , renderCliError
  ) where

import Control.Lens (makeClassy, makeClassyPrisms)
import Data.Semigroup ((<>))

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Types

data CliError
  = CliMissingEnvVars [T.Text]
  | CliFileSystemError FileSystemError
  | CliStackDoesNotExist T.Text
  | CliStackNeedsChangeSetReview T.Text
  | CliStackNotConfigured T.Text
  | CliStackNotGlobal T.Text
  | CliGlobalStackMustProvisionToGlobal T.Text
  | CliMissingDependencyStacks [T.Text]
  | CliTemplateDecodeFail String
  | CliMissingRequiredParameters (S.Set T.Text)
  | CliMissingRequiredOutput T.Text
  | CliDuplicateParameterValues (M.Map T.Text [ParameterValue])
  | CliDuplicateTagValues (M.Map T.Text [T.Text])
  | CliExtraParameterFlags (S.Set T.Text)
  | CliCloudError CloudError
  | CliParseFailure T.Text
  deriving (Eq, Show)

makeClassy ''CliError
makeClassyPrisms ''CliError

renderCliError :: CliError -> T.Text
renderCliError (CliMissingEnvVars vars)
  =  "the following required environment variables were not set:\n"
  <> T.unlines (map ("  " <>) vars)
renderCliError (CliFileSystemError (FileNotFound path))
  = "file not found: ‘" <> path <> "’\n"
renderCliError (CliStackDoesNotExist stackName)
  = "stack doesn't exist: '" <> stackName <> "'\n"
renderCliError (CliStackNeedsChangeSetReview stackName)
  = "stack '" <> stackName <> "' cannot complete until its change set is reviewed\n"
renderCliError (CliStackNotConfigured stackName)
  = "stack name not present in configuration: ‘" <> stackName <> "’\n"
renderCliError (CliStackNotGlobal stackName)
  = "stack is not marked as global in configuration: '" <> stackName <> "'\n"
renderCliError (CliGlobalStackMustProvisionToGlobal stackName)
  = "global stack must be deployed into the global environment: '" <> stackName <> "'\n"
renderCliError (CliMissingDependencyStacks stackNames)
  =  "the following dependency stacks do not exist in AWS:\n"
  <> T.unlines (map ("  " <>) stackNames)
renderCliError (CliTemplateDecodeFail decodeFailure)
  = "template YAML decoding failed: " <> T.pack decodeFailure
renderCliError (CliMissingRequiredParameters params)
  = "the following required parameters were not supplied:\n"
  <> T.unlines (map (" " <>) (S.toAscList params))
renderCliError (CliDuplicateParameterValues params)
  = "the following parameters were supplied more than one value:\n"
  <> renderKeysToManyParameterValues params
renderCliError (CliDuplicateTagValues ts)
  = "the following tags were supplied more than one value:\n"
  <> renderKeysToManyVals ts
renderCliError (CliExtraParameterFlags ts)
  = "parameter flags defined in config that were not present in template:\n"
  <> T.unlines (map (" " <>) (S.toAscList ts))
renderCliError (CliMissingRequiredOutput t)
  = "the following required output was not present: '" <> t <> "'\n"
renderCliError (CliCloudError (CloudErrorInternal msg))
  = "the impossible happened while interacting with AWS: '" <> msg
  <> "'. Please submit a bug report to the cloud-seeder project.\n"
renderCliError (CliCloudError (CloudErrorUser msg))
  = "user error when interacting with AWS: '" <> msg <> "' \n"
renderCliError (CliParseFailure msg)
  = msg

renderKeysToManyVals :: M.Map T.Text [T.Text] -> T.Text
renderKeysToManyVals xs = T.unlines $ map renderKeyToVals (M.toAscList xs)
  where renderKeyToVals (k, vs) = k <> ": " <> T.intercalate ", " vs

renderKeysToManyParameterValues :: M.Map T.Text [ParameterValue] -> T.Text
renderKeysToManyParameterValues xs = T.unlines $ map renderKeyToMaybeVals (M.toAscList xs)
  where renderKeyToMaybeVals (k, vs) = k <> ": " <> T.intercalate ", " (map renderParameterValue vs)
        renderParameterValue UsePreviousValue = "use previous value"
        renderParameterValue (Value x) = x

instance AsFileSystemError CliError where
  _FileSystemError = _CliFileSystemError

instance AsCloudError CliError where
  _CloudError = _CliCloudError
