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
  ( Context (..)
  , executeFile
  , execute
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text qualified as T
import Slab.Command qualified as Command
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.PreProcess qualified as PreProcess
import Slab.Syntax qualified as Syntax
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, shell)

--------------------------------------------------------------------------------
data Context = Context
  { ctxPath :: Maybe FilePath
  , ctxRunMode :: Command.RunMode
  }

-- | Similar to `evaluateFile` but run external commands.
executeFile :: Context -> IO (Either Error.Error [Syntax.Block])
executeFile ctx@(Context {..}) =
  runExceptT $
    PreProcess.preprocessFileE ctxPath
      >>= Evaluate.evaluate Evaluate.defaultEnv ["toplevel"]
      >>= execute ctx

--------------------------------------------------------------------------------
execute
  :: Context
  -> [Syntax.Block]
  -> ExceptT Error.Error IO [Syntax.Block]
execute ctx = mapM (exec ctx)

exec :: Context -> Syntax.Block -> ExceptT Error.Error IO Syntax.Block
exec ctx@(Context {..}) = \case
  node@Syntax.BlockDoctype -> pure node
  Syntax.BlockElem name mdot attrs nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockElem name mdot attrs nodes'
  node@(Syntax.BlockText _ _) -> pure node
  Syntax.BlockInclude mname path mbody -> do
    mbody' <- traverse (execute ctx) mbody
    pure $ Syntax.BlockInclude mname path mbody'
  Syntax.BlockFragmentDef usage name params nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockFragmentDef usage name params nodes'
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
  node@(Syntax.BlockRun _ _ (Just _)) -> pure node
  Syntax.BlockRun cmd minput Nothing -> do
    (code, out, err) <-
      liftIO $
        readCreateProcessWithExitCode (shell $ T.unpack cmd) $
          maybe "" T.unpack minput
    case code of
      ExitSuccess ->
        pure $
          Syntax.BlockRun cmd minput $
            Just [Syntax.BlockText Syntax.RunOutput [Syntax.Lit $ T.pack out]]
      ExitFailure _ -> case ctxRunMode of
        Command.RunNormal ->
          throwE $
            Error.ExecuteError $
              T.pack err <> T.pack out
        Command.RunPassthrough ->
          pure $
            Syntax.BlockRun cmd minput $
              Just $
                [ Syntax.BlockElem
                    Syntax.Pre
                    Syntax.NoSym
                    []
                    [ Syntax.BlockElem
                        Syntax.Code
                        Syntax.HasDot
                        []
                        [Syntax.BlockText Syntax.RunOutput [Syntax.Lit $ T.pack (err <> out)]]
                    ]
                ]
  node@(Syntax.BlockAssignVars _) -> pure node
  Syntax.BlockIf cond as bs -> do
    as' <- execute ctx as
    bs' <- execute ctx bs
    pure $ Syntax.BlockIf cond as' bs'
  Syntax.BlockList nodes -> do
    nodes' <- execute ctx nodes
    pure $ Syntax.BlockList nodes'
  node@(Syntax.BlockCode _) -> pure node
