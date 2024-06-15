{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Slab.Execute
  ( run
  , executeFile
  , execute
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text qualified as T
import Slab.Evaluate qualified as Evaluate
import Slab.Syntax qualified as Syntax
import System.Process (cwd, readCreateProcess, shell)

--------------------------------------------------------------------------------
run :: FilePath -> [Syntax.Block] -> IO (Either Evaluate.PreProcessError [Syntax.Block])
run path nodes = do
  let ctx =
        Evaluate.Context
          { ctxStartPath = path
          }
  nodes' <- runExceptT $ execute ctx nodes
  pure nodes'

executeFile :: FilePath -> IO (Either Evaluate.PreProcessError [Syntax.Block])
executeFile path = do
  let ctx =
        Evaluate.Context
          { ctxStartPath = path
          }
  runExceptT $
    Evaluate.preprocessFileE path
      >>= Evaluate.evaluate Evaluate.defaultEnv ["toplevel"]
      >>= execute ctx

execute :: Evaluate.Context -> [Syntax.Block]
  -> ExceptT Evaluate.PreProcessError IO [Syntax.Block]
execute ctx = mapM (exec ctx)

exec :: Evaluate.Context -> Syntax.Block -> ExceptT Evaluate.PreProcessError IO Syntax.Block
exec ctx@Evaluate.Context {..} = \case
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
  Syntax.BlockFragmentCall name values nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockFragmentCall name values nodes'
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
    out <- liftIO $
      readCreateProcess ((shell $ T.unpack cmd) { cwd = Just "/tmp/" }) ""
    pure $ Syntax.BlockRun cmd $
      Just [Syntax.BlockText Syntax.RunOutput [Syntax.Lit $ T.pack out]]
  node@(Syntax.BlockReadJson _ _ _) -> pure node
  node@(Syntax.BlockAssignVar _ _) -> pure node
  Syntax.BlockIf cond as bs -> do
    as' <- execute ctx as
    bs' <- execute ctx bs
    pure $ Syntax.BlockIf cond as' bs'
  Syntax.BlockList nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockList nodes'
  node@(Syntax.BlockCode _) -> pure node
