{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Slab.PreProcess
-- Description : Parse and process included and imported files
--
-- @Slab.PreProcess@ recursively parses files, following includes and imports.
-- This is also responsible of reading JSON files referenced in the expression
-- language.
module Slab.PreProcess
  ( Context (..)
  , preprocessFile
  , preprocessFileE
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Slab.Error qualified as Error
import Slab.Parse qualified as Parse
import Slab.Syntax
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  }

--------------------------------------------------------------------------------

-- | Similar to `parseFile` but pre-process the include statements.
preprocessFile :: Maybe FilePath -> IO (Either Error.Error [Block])
preprocessFile = runExceptT . preprocessFileE

preprocessFileE :: Maybe FilePath -> ExceptT Error.Error IO [Block]
preprocessFileE mpath = do
  nodes <- Parse.parseFileE mpath
  let ctx =
        Context
          { ctxStartPath = maybe "." id mpath
          }
  preprocess ctx nodes

--------------------------------------------------------------------------------

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preprocess :: Context -> [Block] -> ExceptT Error.Error IO [Block]
preprocess ctx nodes = mapM (preproc ctx) nodes

preproc :: Context -> Block -> ExceptT Error.Error IO Block
preproc ctx@Context {..} = \case
  node@BlockDoctype -> pure node
  BlockElem name mdot attrs nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockElem name mdot attrs nodes'
  node@(BlockText _ _) -> pure node
  BlockInclude mname path _ -> do
    includedPath <- liftIO $ canonicalizePath $ takeDirectory ctxStartPath </> path
    let slabExt = takeExtension includedPath == ".slab"
    exists <- liftIO $ doesFileExist includedPath
    if
        | exists && (not slabExt || mname == Just "escape-html") -> do
            -- Include the file content as-is.
            content <- liftIO $ T.readFile includedPath
            let node = Parse.parserTextInclude content
            pure $ BlockInclude mname path (Just [node])
        | exists -> do
            -- Parse and process the .slab file.
            nodes' <- preprocessFileE (Just includedPath)
            pure $ BlockInclude mname path (Just nodes')
        | otherwise ->
            throwE $ Error.PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  BlockFragmentDef usage name params nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockFragmentDef usage name params nodes'
  BlockFragmentCall name mdot attrs values nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockFragmentCall name mdot attrs values nodes'
  node@(BlockFor _ _ _ _) -> pure node
  node@(BlockComment _ _) -> pure node
  node@(BlockFilter _ _) -> pure node
  node@(BlockRawElem _ _) -> pure node
  BlockDefault name nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockDefault name nodes'
  BlockImport path _ args -> do
    -- An import is treated like an include used to define a fragment, then
    -- directly calling that fragment.
    let includedPath = takeDirectory ctxStartPath </> path
        slabExt = takeExtension includedPath == ".slab"
    exists <- liftIO $ doesFileExist includedPath
    if
        | exists && not slabExt ->
            throwE $ Error.PreProcessError $ "Extends requires a .slab file"
        | exists -> do
            -- Parse and process the .slab file.
            body <- preprocessFileE (Just includedPath)
            args' <- mapM (preproc ctx) args
            pure $ BlockImport path (Just body) args'
        | otherwise ->
            throwE $ Error.PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  node@(BlockRun _ _ _) -> pure node
  BlockAssignVars pairs -> do
    let f (name, JsonPath path) = do
          let path' = takeDirectory ctxStartPath </> path
          content <- liftIO $ BL.readFile path'
          case Aeson.eitherDecode content of
            Right val ->
              pure (name, jsonToExpr val)
            Left err ->
              throwE $ Error.PreProcessError $ "Can't decode JSON: " <> T.pack err
        f pair = pure pair
    pairs' <- mapM f pairs
    pure $ BlockAssignVars pairs'
  BlockIf cond as bs -> do
    -- File inclusion is done right away, without checking the condition.
    as' <- preprocess ctx as
    bs' <- preprocess ctx bs
    pure $ BlockIf cond as' bs'
  BlockList nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockList nodes'
  node@(BlockCode _) -> pure node

jsonToExpr :: Aeson.Value -> Expr
jsonToExpr = \case
  Aeson.String s -> SingleQuoteString s
  Aeson.Array xs ->
    List $ map jsonToExpr (V.toList xs)
  Aeson.Object kvs ->
    let f (k, v) = (SingleQuoteString $ Aeson.Key.toText k, jsonToExpr v)
     in Object $ map f (Aeson.KeyMap.toList kvs)
  x -> error $ "jsonToExpr: " <> show x
