{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Slab.PreProcess
  ( Context (..)
  , PreProcessError (..)
  , preprocessFile
  , preprocessFileE
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Slab.Parse qualified as Parse
import Slab.Syntax
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  }

data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | Similar to `parseFile` but pre-process the include statements.
preprocessFile :: FilePath -> IO (Either PreProcessError [Block])
preprocessFile = runExceptT . preprocessFileE

preprocessFileE :: FilePath -> ExceptT PreProcessError IO [Block]
preprocessFileE path = do
  nodes <- withExceptT PreProcessParseError $ Parse.parseFileE path
  let ctx =
        Context
          { ctxStartPath = path
          }
  preprocess ctx nodes

--------------------------------------------------------------------------------

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preprocess :: Context -> [Block] -> ExceptT PreProcessError IO [Block]
preprocess ctx nodes = mapM (preproc ctx) nodes

preproc :: Context -> Block -> ExceptT PreProcessError IO Block
preproc ctx@Context {..} = \case
  node@BlockDoctype -> pure node
  BlockElem name mdot attrs nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockElem name mdot attrs nodes'
  node@(BlockText _ _) -> pure node
  BlockInclude mname path _ -> do
    let includedPath = takeDirectory ctxStartPath </> path
        slabExt = takeExtension includedPath == ".slab"
    exists <- liftIO $ doesFileExist includedPath
    if
        | exists && (not slabExt || mname == Just "escape-html") -> do
            -- Include the file content as-is.
            content <- liftIO $ T.readFile includedPath
            let node = Parse.pugTextInclude content
            pure $ BlockInclude mname path (Just [node])
        | exists -> do
            -- Parse and process the .slab file.
            nodes' <- preprocessFileE includedPath
            pure $ BlockInclude mname path (Just nodes')
        | otherwise ->
            throwE $ PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  BlockFragmentDef name params nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockFragmentDef name params nodes'
  BlockFragmentCall name values nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockFragmentCall name values nodes'
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
            throwE $ PreProcessError $ "Extends requires a .slab file"
        | exists -> do
            -- Parse and process the .slab file.
            body <- preprocessFileE includedPath
            args' <- mapM (preproc ctx) args
            pure $ BlockImport path (Just body) args'
        | otherwise ->
            throwE $ PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  node@(BlockRun _ _) -> pure node
  BlockReadJson name path _ -> do
    let path' = takeDirectory ctxStartPath </> path
    content <- liftIO $ BL.readFile path'
    case Aeson.eitherDecode content of
      Right val ->
        pure $ BlockReadJson name path $ Just val
      Left err ->
        throwE $ PreProcessError $ "Can't decode JSON: " <> T.pack err
  node@(BlockAssignVar _ _) -> pure node
  BlockIf cond as bs -> do
    -- File inclusion is done right away, without checking the condition.
    as' <- preprocess ctx as
    bs' <- preprocess ctx bs
    pure $ BlockIf cond as' bs'
  BlockList nodes -> do
    nodes' <- preprocess ctx nodes
    pure $ BlockList nodes'
  node@(BlockCode _) -> pure node
