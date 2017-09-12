{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.CloudSeeder.Main
  ( AppM
  , cli
  , cliIO
  ) where

import Control.Applicative.Lift (Errors, failure, runErrors)
import Control.Arrow (second)
import Control.Lens (Getting, Prism', (^.), (^..), (^?), _1, _2, _Wrapped, anyOf, aside, each, filtered, folded, has, only, to)
import Control.Monad (unless, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List (find, groupBy, sort)
import Data.Text.Encoding (encodeUtf8)
import Data.Semigroup (Endo, (<>))
import Data.Yaml (decodeEither)
import Network.AWS (Credentials(Discover), Env, newEnv)
import System.Exit (exitFailure)

import Prelude hiding (readFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S

import Network.CloudSeeder.CommandLine
import Network.CloudSeeder.DSL
import Network.CloudSeeder.Error
import Network.CloudSeeder.Interfaces
import Network.CloudSeeder.Template
import Network.CloudSeeder.Types

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (ReaderT Env (ExceptT CliError (LoggingT IO)) a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO
           , MonadCatch, MonadThrow, MonadReader Env, MonadError CliError
           , MonadLogger, MonadEnvironment )

instance MonadBaseControl IO AppM where
  type StM AppM a = StM (ReaderT Env (ExceptT CliError (LoggingT IO))) a
  liftBaseWith f = AppM (liftBaseWith (\g -> f (\(AppM x) -> g x)))
  restoreM = AppM . restoreM

instance MonadFileSystem CliError AppM where
  readFile = readFile'

instance MonadCLI AppM where
  getArgs = getArgs'
  getOptions = getOptions'

instance MonadCloud AppM where
  computeChangeset = computeChangeset'
  getStackInfo = getStackInfo'
  getStackOutputs = getStackOutputs'
  runChangeSet = runChangeSet'
  encrypt = encrypt'
  upload = upload'
  generateSecret = generateSecret'

runAppM :: AppM a -> IO a
runAppM (AppM x) = do
  env <- newEnv Discover
  result <- runStderrLoggingT . runExceptT $ runReaderT x env
  either (\err -> T.putStr (renderCliError err) >> exitFailure) return result

--------------------------------------------------------------------------------
-- Logic

cli
  :: (MonadCLI m, MonadCloud m, MonadFileSystem CliError m, MonadEnvironment m)
  => m (DeploymentConfiguration m) -> m ()
cli mConfig = do
  config <- mConfig
  (ProvisionStack nameToProvision env) <- getArgs

  let dependencies = takeWhile (\stak -> (stak ^. name) /= nameToProvision) (config ^.. stacks.each)
      appName = config ^. name

  stackToProvision <- getStackToProvision config nameToProvision

  let isGlobalStack = stackToProvision ^. globalStack
  when (env == "global" && not isGlobalStack) $
    throwing _CliStackNotGlobal nameToProvision
  when (env /= "global" && isGlobalStack) $
    throwing _CliGlobalStackMustProvisionToGlobal nameToProvision

  templateBody <- readFile $ nameToProvision <> ".yaml"
  template <- decodeTemplate templateBody

  let paramSources = (config ^. parameterSources) <> (stackToProvision ^. parameterSources)
      paramSpecs = template ^. parameterSpecs._Wrapped
  newStackOrPreviousValues <- getStackProvisionType $ mkFullStackName env appName nameToProvision
  allTags <- getTags config stackToProvision env appName

  allParams <- getParameters newStackOrPreviousValues config stackToProvision paramSources paramSpecs dependencies env appName

  let fullStackName = mkFullStackName env appName nameToProvision
  csId <- computeChangeset fullStackName newStackOrPreviousValues templateBody allParams allTags
  runChangeSet csId

getStackProvisionType :: (MonadCloud m) => StackName -> m ProvisionType
getStackProvisionType stackName = do
  maybeStack <- getStackInfo stackName
  case maybeStack of
    Nothing -> return CreateStack
    Just aStack -> return $ UpdateStack $ aStack ^. parameters

getStackToProvision
  :: (AsCliError e, MonadError e m)
  => DeploymentConfiguration m -> T.Text -> m (StackConfiguration m)
getStackToProvision config nameToProvision = do
  let maybeStackToProvision = config ^. stacks.to (find (has (name.only nameToProvision)))
  maybe (throwing _CliStackNotConfigured nameToProvision) return maybeStackToProvision

decodeTemplate :: (AsCliError e, MonadError e m) => T.Text -> m Template
decodeTemplate templateBody = do
  let decodeOrFailure = decodeEither (encodeUtf8 templateBody) :: Either String Template
  either (throwing _CliTemplateDecodeFail) return decodeOrFailure

mkFullStackName :: T.Text -> T.Text -> T.Text -> StackName
mkFullStackName env appName stackName = StackName $ env <> "-" <> appName <> "-" <> stackName

getTags
  :: (MonadError e m, AsCliError e)
  => DeploymentConfiguration m -> StackConfiguration m -> T.Text -> T.Text -> m (M.Map T.Text T.Text)
getTags config stackToProvision env appName =
   assertUnique _CliDuplicateTagValues (baseTags <> globalTags <> localTags)
  where
    baseTags :: S.Set (T.Text, T.Text)
    baseTags = [("cj:environment", env), ("cj:application", appName)]
    globalTags = config ^. tagSet
    localTags = stackToProvision ^. tagSet

-- | Fetches parameter values for all param sources, handling potential errors
-- and misconfigurations.
getParameters
  :: forall e m. (AsCliError e, MonadError e m, MonadCLI m, MonadEnvironment m, MonadCloud m)
  => ProvisionType
  -> DeploymentConfiguration m
  -> StackConfiguration m
  -> S.Set (T.Text, ParameterSource) -- ^ parameter sources to fetch values for
  -> S.Set ParameterSpec -- ^ parameter specs from the template currently being deployed
  -> [StackConfiguration m] -- ^ stack dependencies
  -> T.Text -- ^ name of environment being deployed to
  -> T.Text -- ^ name of application being deployed
  -> m (M.Map T.Text ParameterValue)
getParameters provisionType config stackToProvision paramSources allParamSpecs dependencies env appName = do
    let paramSpecs = setRequiredSpecsWithPreviousValuesToOptional allParamSpecs
        constants  = paramSources ^..* folded.aside _Constant.to (second Value)
    outputs' <- outputs
    flags' <- flags paramSpecs
    envVars' <- envVars
    let fetchedParams = S.unions [envVars', flags', S.map (second Value) outputs']
    let initialParams = S.insert ("Env", Value env) (constants <> fetchedParams)
    validInitialParams <- validateParameters paramSpecs initialParams
    postHookParams <- runCreateHooks validInitialParams outputs'
    assertNoMissingRequiredParameters postHookParams paramSpecs
    return postHookParams
  where
    setRequiredSpecsWithPreviousValuesToOptional :: S.Set ParameterSpec -> S.Set ParameterSpec
    setRequiredSpecsWithPreviousValuesToOptional pSpecs = do
      let previousParamKeys = provisionType ^. _UpdateStack
          requiredParamSpecs = pSpecs ^..* folded._Required
          optionalParamSpecs = pSpecs ^..* folded._Optional
          mkOptionalIfPreviousValue key = if key `S.member` previousParamKeys
            then Optional key UsePreviousValue
            else Required key
          tupleToOptional (key, val) = Optional key val
      (mkOptionalIfPreviousValue `S.map` requiredParamSpecs) `S.union` (tupleToOptional `S.map` optionalParamSpecs)

    envVars :: m (S.Set (T.Text, ParameterValue))
    envVars = do
      let requiredEnvVars = paramSources ^.. folded.filtered (has (_2._Env))._1
      maybeEnvValues <- mapM (\envVarKey -> (envVarKey,) <$> getEnv envVarKey) requiredEnvVars
      let envVarsOrFailure = runErrors $ traverse (extractResult (,)) maybeEnvValues
      either (throwing _CliMissingEnvVars . sort) (return . S.fromList . fmap (second Value)) envVarsOrFailure

    flags :: S.Set ParameterSpec -> m (S.Set (T.Text, ParameterValue))
    flags paramSpecs = do
      let paramFlags = paramSources ^..* folded.filtered (has (_2._Flag))._1
          flaggedParamSpecs = paramSpecs ^..* folded.filtered (anyOf parameterKey (`elem` paramFlags))
          paramSpecNames = paramSpecs ^..* folded.parameterKey
          paramFlagsNotInTemplate = paramFlags ^..* folded.filtered (`notElem` paramSpecNames)

      unless (S.null paramFlagsNotInTemplate) $
        throwing _CliExtraParameterFlags paramFlagsNotInTemplate
      S.fromList . M.toList <$> getOptions flaggedParamSpecs

    outputs :: m (S.Set (T.Text, T.Text))
    outputs = do
        maybeLocalOutputs <- getOutputs (\s -> not (s ^. globalStack)) env
        maybeGlobalOutputs <- getOutputs (^. globalStack) "global"

        let maybeOutputs = maybeLocalOutputs <> maybeGlobalOutputs
            outputsOrFailure = runErrors $ traverse (extractResult (flip const)) maybeOutputs
        either (throwing _CliMissingDependencyStacks) (return . S.fromList . concatMap M.toList) outputsOrFailure
      where
        getOutputs predicate envName = do
          let filteredDependencies = dependencies ^.. folded.filtered predicate
              filteredDependencyNames = filteredDependencies ^.. each.name
          mapM (\stackName -> (stackName,) <$> getStackOutputs (mkFullStackName envName appName stackName)) filteredDependencyNames

    validateParameters :: S.Set ParameterSpec -> S.Set (T.Text, ParameterValue) -> m (M.Map T.Text ParameterValue)
    validateParameters paramSpecs params = do

      uniqueParams <- assertUnique _CliDuplicateParameterValues params

      let allowedParamNames = paramSpecs ^..* folded.parameterKey
      let previousParams = M.fromList $ S.toAscList $ paramSpecs ^..* folded._Optional.filtered (has $ _2._UsePreviousValue)
      let allUniqueParams = M.union uniqueParams previousParams

      return $ allUniqueParams `M.intersection` M.fromSet (const ()) allowedParamNames

    runCreateHooks :: M.Map T.Text ParameterValue -> S.Set (T.Text, T.Text) -> m (M.Map T.Text ParameterValue)
    runCreateHooks params outputs' = do
      let createHooks = coerce <$> (stackToProvision ^. hooksCreate)
          initialParamValues = M.mapMaybe (^? _Value) params
      let hookContext = HookContext outputs' config stackToProvision
      createHookParamsOrFailure <- runReaderT (runExceptT $ execStateT (sequence createHooks) initialParamValues) hookContext
      createHookParams <- either (throwing _CliError) return createHookParamsOrFailure
      return $ M.map Value createHookParams `M.union` params

    assertNoMissingRequiredParameters :: M.Map T.Text ParameterValue -> S.Set ParameterSpec -> m ()
    assertNoMissingRequiredParameters params paramSpecs = do
      let requiredParamNames = paramSpecs ^..* folded._Required
      uniqueParams <- assertUnique _CliDuplicateParameterValues (S.fromList $ M.toAscList params)
      let missingParamNames = requiredParamNames S.\\ M.keysSet uniqueParams
      unless (S.null missingParamNames) $
        throwing _CliMissingRequiredParameters missingParamNames

cliIO :: AppM (DeploymentConfiguration AppM) -> IO ()
cliIO mConfig = runAppM $ cli mConfig

-- | Given a set of tuples that represent a mapping between keys and values,
-- assert the keys are all unique, and produce a map as a result. If any keys
-- are duplicated, the provided prism will be used to signal an error.
assertUnique
  :: forall k v e m. (Ord k, MonadError e m)
  => Prism' e (M.Map k [v]) -> S.Set (k, v) -> m (M.Map k v)
assertUnique _Err paramSet = case duplicateParams of
    [] -> return $ M.fromList paramList
    _ -> throwing _Err duplicateParams
  where
    paramList = S.toAscList paramSet
    paramsGrouped = tuplesToMap paramList
    duplicateParams = M.filter ((> 1) . length) paramsGrouped

    tuplesToMap :: [(k, v)] -> M.Map k [v]
    tuplesToMap xs = M.fromList $ map concatGroup grouped
      where
        grouped = groupBy ((==) `on` fst) xs
        concatGroup ys = (fst (head ys), map snd ys)

-- | Applies a function to the members of a tuple to produce a result, unless
-- the tuple contains 'Nothing', in which case this logs an error in the
-- 'Errors' applicative using the left side of the tuple as a label.
--
-- >>> runErrors $ extractResult (,) ("foo", Just True)
-- Right ("foo", True)
-- >>> runErrors $ extractResult (,) ("foo", Nothing)
-- Left ["foo"]
extractResult :: (a -> b -> c) -> (a, Maybe b) -> Errors [a] c
extractResult f (k, m) = do
  v <- maybe (failure [k]) pure m
  pure (f k v)

-- | Like '^..', but collects the result into a 'S.Set' instead of a list.
infixl 8 ^..*
(^..*) :: Ord a => s -> Getting (Endo [a]) s a -> S.Set a
x ^..* l = S.fromList (x ^.. l)
