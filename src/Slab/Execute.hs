{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Slab.Execute
-- Description : Run external commands referenced by an AST
--
-- @Slab.Execute@ implements the execution stage of Slab, i.e. running external
-- commands (for instance referenced by the @run@ syntax). This is done after
-- the evaluation stage (implemented in "Slab.Evaluate").
--
-- After execution, the resulting blocks can be rendered to HTML by
-- "Slab.Render".
module Slab.Execute
  ( executeFile
  , execute
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text qualified as T
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.PreProcess qualified as PreProcess
import Slab.Syntax qualified as Syntax
import System.Process (cwd, readCreateProcess, shell)

--------------------------------------------------------------------------------

-- | Similar to `evaluateFile` but run external commands.
executeFile :: FilePath -> IO (Either Error.Error [Syntax.Block])
executeFile path =
  runExceptT $
    PreProcess.preprocessFileE path
      >>= Evaluate.evaluate Evaluate.defaultEnv ["toplevel"]
      >>= execute path

--------------------------------------------------------------------------------
execute
  :: FilePath
  -> [Syntax.Block]
  -> ExceptT Error.Error IO [Syntax.Block]
execute ctx = mapM (exec ctx)

exec :: FilePath -> Syntax.Block -> ExceptT Error.Error IO Syntax.Block
exec ctx = \case
  node@Syntax.BlockDoctype -> pure node
  Syntax.BlockElem name mdot attrs nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockElem name mdot attrs nodes'
  node@(Syntax.BlockText _ _) -> pure node
  Syntax.BlockInclude mname path mbody -> do
    mbody' <- traverse (execute ctx) mbody
    pure $ Syntax.BlockInclude mname path mbody'
  Syntax.BlockFragmentDef name params nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockFragmentDef name params nodes'
  Syntax.BlockFragmentCall name mdot attrs values nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockFragmentCall name mdot attrs values nodes'
  node@(Syntax.BlockFor _ _ _ _) -> pure node
  node@(Syntax.BlockComment _ _) -> pure node
  node@(Syntax.BlockFilter _ _) -> pure node
  node@(Syntax.BlockRawElem _ _) -> pure node
  Syntax.BlockDefault name nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockDefault name nodes'
  Syntax.BlockImport path mbody args -> do
    mbody' <- traverse (execute ctx) mbody
    pure $ Syntax.BlockImport path mbody' args
  node@(Syntax.BlockRun _ (Just _)) -> pure node
  Syntax.BlockRun cmd Nothing -> do
    out <-
      liftIO $
        readCreateProcess ((shell $ T.unpack cmd) {cwd = Just "/tmp/"}) ""
    pure $
      Syntax.BlockRun cmd $
        Just [Syntax.BlockText Syntax.RunOutput [Syntax.Lit $ T.pack out]]
  node@(Syntax.BlockReadJson _ _ _) -> pure node
  node@(Syntax.BlockAssignVars _) -> pure node
  Syntax.BlockIf cond as bs -> do
    as' <- execute ctx as
    bs' <- execute ctx bs
    pure $ Syntax.BlockIf cond as' bs'
  Syntax.BlockList nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockList nodes'
  node@(Syntax.BlockCode _) -> pure node
