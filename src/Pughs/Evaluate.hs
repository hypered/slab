{-# LANGUAGE RecordWildCards #-}

module Pughs.Evaluate
  ( PreProcessError (..)
  , preProcessPugFile
  , evaluatePugFile
  , evaluate
  , emptyEnv
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Pughs.Parse qualified as Parse
import Pughs.Syntax
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  }

data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

-- Similarly to `parsePugFile` but pre-process the include statements.
preProcessPugFile :: FilePath -> IO (Either PreProcessError [PugNode])
preProcessPugFile = runExceptT . preProcessPugFileE

preProcessPugFileE :: FilePath -> ExceptT PreProcessError IO [PugNode]
preProcessPugFileE path = do
  pugContent <- liftIO $ T.readFile path
  let mnodes = first PreProcessParseError $ Parse.parsePug path pugContent
  nodes <- except mnodes
  let ctx =
        Context
          { ctxStartPath = path
          }
  preProcessNodesE ctx nodes

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preProcessNodesE :: Context -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
preProcessNodesE ctx@Context {..} (PugExtends path _ : nodes) = do
  -- An extends is treated like an include used to define a fragment, then
  -- directly calling that fragment.
  let includedPath = takeDirectory ctxStartPath </> path
      pugExt = takeExtension includedPath == ".pug"
  exists <- liftIO $ doesFileExist includedPath
  if exists && not pugExt
    then
      throwE $ PreProcessError $ "Extends requires a .pug file"
    else do
      -- Parse and process the .pug file.
      let includedPath' = if pugExt then includedPath else includedPath <.> ".pug"
      nodes' <- preProcessPugFileE includedPath'
      let def = PugFragmentDef (T.pack path) nodes'
      nodes'' <- mapM (preProcessNodeE ctx) nodes
      let    call = PugFragmentCall (T.pack path) nodes''
      pure [def, call]
preProcessNodesE ctx nodes = mapM (preProcessNodeE ctx) nodes

evaluatePugFile :: FilePath -> IO (Either PreProcessError [PugNode])
evaluatePugFile path = runExceptT (preProcessPugFileE path >>= evaluate emptyEnv)

data Env = Env
  { envFragments :: [(Text, [PugNode])]
  , envVariables :: [(Text, Code)]
  }
  deriving Show

emptyEnv = Env [] []

lookupFragment name Env {..} = lookup name envFragments

lookupVariable name Env {..} = lookup name envVariables

augmentFragments Env {..} xs = Env { envFragments = xs <> envFragments, .. }

augmentVariables Env {..} xs = Env { envVariables = xs <> envVariables, .. }

-- Process mixin calls. This should be done after processing the include statement
-- since mixins may be defined in included files.
evaluate :: Env -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
evaluate env nodes = do
  let combs = extractCombinators nodes
      assigns = extractAssignments nodes
  let env' = augmentVariables (augmentFragments env $ combs) assigns
  mapM (eval env') nodes

preProcessNodeE :: Context -> PugNode -> ExceptT PreProcessError IO PugNode
preProcessNodeE ctx@Context {..} = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  node@(PugCode _) -> pure node
  PugInclude path _ -> do
    let includedPath = takeDirectory ctxStartPath </> path
        pugExt = takeExtension includedPath == ".pug"
    exists <- liftIO $ doesFileExist includedPath
    if exists && not pugExt
      then do
        -- Include the file content as-is.
        content <- liftIO $ T.readFile includedPath
        let nodes' = map (PugText Include) $ T.lines content
        pure $ PugInclude path (Just nodes')
      else do
        -- Parse and process the .pug file.
        let includedPath' = if pugExt then includedPath else includedPath <.> ".pug"
        nodes' <- preProcessPugFileE includedPath'
        pure $ PugInclude path (Just nodes')
  PugMixinDef name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugMixinDef name nodes'
  node@(PugMixinCall _ _) -> pure node
  PugFragmentDef name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugFragmentDef name nodes'
  node@(PugFragmentCall _ _) -> pure node
  node@(PugEach _ _ _ _) -> pure node
  node@(PugComment _ _) -> pure node
  node@(PugFilter _ _) -> pure node
  node@(PugRawElem _ _) -> pure node
  PugBlock what name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugBlock what name nodes'
  PugExtends _ _ ->
    throwE $ PreProcessError $ "Extends must be the first node in a file\""
  PugReadJson name path _ -> do
    content <- liftIO $ BL.readFile path
    case Aeson.eitherDecode content of
      Right val ->
        pure $ PugReadJson name path $ Just val
      Left err ->
        throwE $ PreProcessError $ "Can't decode JSON: " <> T.pack err

eval :: Env -> PugNode -> ExceptT PreProcessError IO PugNode
eval env = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- evaluate env nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  node@(PugCode code) -> do
    code' <- evalCode env code
    pure $ PugCode code'
  PugInclude path mnodes -> do
    case mnodes of
      Just nodes -> do
        nodes' <- evaluate env nodes
        pure $ PugInclude path (Just nodes')
      Nothing ->
        pure $ PugInclude path Nothing
  PugMixinDef name nodes -> do
    nodes' <- evaluate env nodes
    pure $ PugMixinDef name nodes'
  PugMixinCall name _ ->
    case lookupFragment name env of
      Just body ->
        pure $ PugMixinCall name (Just body)
      Nothing -> throwE $ PreProcessError $ "Can't find mixin \"" <> name <> "\""
  PugFragmentDef name nodes -> do
    nodes' <- evaluate env nodes
    pure $ PugFragmentDef name nodes'
  PugFragmentCall name args -> do
    case lookupFragment name env of
      Just body -> do
        -- TODO Either evaluate the args before constructing the env, or capture
        -- the env in a thunk.
        env' <- mapM namedBlock args
        let env'' = augmentFragments env env'
        body' <- evaluate env'' body
        pure $ PugFragmentCall name body'
      Nothing -> throwE $ PreProcessError $ "Can't find fragment \"" <> name <> "\""
  node@(PugEach name mindex values nodes) -> do
    -- Re-use PugEach to construct a single node to return.
    let zero :: Int
        zero = 0
    values' <- evalCode env values
    let collection = case values' of
          List xs -> zip xs $ map (SingleQuoteString . T.pack . show) [zero..]
          Object xs -> map (\(k, v) -> (v, k)) xs
    nodes' <- forM collection $ \(value, index) -> do
      let env' = case mindex of
            Just idxname -> augmentVariables env [(name, value), (idxname, index)]
            Nothing -> augmentVariables env [(name, value)]
      evaluate env' nodes
    pure $ PugEach name mindex values $ concat nodes'
  node@(PugComment _ _) -> pure node
  node@(PugFilter _ _) -> pure node
  node@(PugRawElem _ _) -> pure node
  PugBlock WithinDef name nodes -> do
    -- If the block is not given as an argument, we return the default block,
    -- but recursively trying to replace the blocks found within its own body.
    case lookupFragment name env of
      Nothing -> do
        nodes' <- evaluate env nodes
        pure $ PugBlock WithinDef name nodes'
      Just nodes' -> pure $ PugBlock WithinDef name nodes'
  PugBlock WithinCall name nodes -> do
    nodes' <- evaluate env nodes
    pure $ PugBlock WithinCall name nodes'
  PugExtends _ _ ->
    throwE $ PreProcessError $ "Extends must be preprocessed before evaluation\""
  node@(PugReadJson _ _ _) -> pure node

namedBlock :: Monad m => PugNode -> ExceptT PreProcessError m (Text, [PugNode])
namedBlock (PugBlock _ name content) = pure (name, content)
namedBlock _ = throwE $ PreProcessError $ "Not a named block argument"

evalCode :: Env -> Code -> ExceptT PreProcessError IO Code
evalCode env = \case
  Variable name ->
    case lookupVariable name env of
      Just val -> pure val
      Nothing -> throwE $ PreProcessError $ "Can't find variable \"" <> name <> "\""
  Lookup name key ->
    case lookupVariable name env of
      Just (Object obj) -> do
        -- key' <- evalCode env key
        case lookup key obj of
          Just val -> pure val
          Nothing ->
            throwE $ PreProcessError $ "Key lookup failed. Key: " <> T.pack (show key) <> T.pack (show obj)
      Just _ -> throwE $ PreProcessError $ "Variable \"" <> name <> "\" is not an object"
      Nothing -> throwE $ PreProcessError $ "Can't find variable \"" <> name <> "\""
  code -> pure code
