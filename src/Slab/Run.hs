module Slab.Run
  ( run
  ) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Slab.Build qualified as Build
import Slab.Command qualified as Command
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Execute qualified as Execute
import Slab.Generate.Haskell qualified as Generate
import Slab.Parse qualified as Parse
import Slab.PreProcess qualified as PreProcess
import Slab.Render qualified as Render
import Slab.Report qualified as Report
import Slab.Serve qualified as Serve
import Slab.Syntax qualified as Syntax
import Slab.Watch qualified as Watch
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.Build srcDir renderMode distDir) = Build.buildDir srcDir renderMode distDir
run (Command.Watch srcDir renderMode distDir) =
  Watch.run srcDir (Build.buildFile srcDir renderMode distDir)
run (Command.Serve distDir) = Serve.run distDir
run (Command.Report srcDir) = Report.run srcDir
run (Command.Generate path) = Generate.renderHs path
run (Command.CommandWithPath path pmode (Command.Render Command.RenderNormal)) = do
  nodes <- executeWithMode path pmode >>= Error.unwrap
  TL.putStrLn . Render.renderHtmls $ Render.renderBlocks nodes
run (Command.CommandWithPath path pmode (Command.Render Command.RenderPretty)) = do
  nodes <- executeWithMode path pmode >>= Error.unwrap
  T.putStr . Render.prettyHtmls $ Render.renderBlocks nodes
run (Command.CommandWithPath path pmode Command.Execute) = do
  nodes <- executeWithMode path pmode >>= Error.unwrap
  TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode (Command.Evaluate simpl)) = do
  nodes <- evaluateWithMode path pmode >>= Error.unwrap
  if simpl
    then TL.putStrLn $ pShowNoColor $ Evaluate.simplify nodes
    else TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode Command.Parse) = do
  nodes <- parseWithMode path pmode >>= Error.unwrap
  TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode Command.Classes) = do
  nodes <- parseWithMode path pmode >>= Error.unwrap
  mapM_ T.putStrLn $ Syntax.extractClasses nodes
run (Command.CommandWithPath path pmode (Command.Fragments mname)) = do
  nodes <- parseWithMode path pmode >>= Error.unwrap
  let ms = Syntax.extractFragments nodes
  case mname of
    Just name -> case Syntax.findFragment name ms of
      Just m -> TL.putStrLn $ pShowNoColor m
      Nothing -> putStrLn "No such fragment."
    Nothing -> TL.putStrLn $ pShowNoColor ms

--------------------------------------------------------------------------------
parseWithMode
  :: FilePath
  -> Command.ParseMode
  -> IO (Either Error.Error [Syntax.Block])
parseWithMode path pmode = runExceptT $ parseWithModeE path pmode

evaluateWithMode
  :: FilePath
  -> Command.ParseMode
  -> IO (Either Error.Error [Syntax.Block])
evaluateWithMode path pmode = runExceptT $ evaluateWithModeE path pmode

executeWithMode
  :: FilePath
  -> Command.ParseMode
  -> IO (Either Error.Error [Syntax.Block])
executeWithMode path pmode = runExceptT $ executeWithModeE path pmode

--------------------------------------------------------------------------------
parseWithModeE
  :: FilePath
  -> Command.ParseMode
  -> ExceptT Error.Error IO [Syntax.Block]
parseWithModeE path pmode =
  case pmode of
    Command.ParseShallow -> Parse.parseFileE path
    Command.ParseDeep -> PreProcess.preprocessFileE path

evaluateWithModeE
  :: FilePath
  -> Command.ParseMode
  -> ExceptT Error.Error IO [Syntax.Block]
evaluateWithModeE path pmode = do
  parsed <- parseWithModeE path pmode
  Evaluate.evaluate Evaluate.defaultEnv ["toplevel"] parsed

executeWithModeE
  :: FilePath
  -> Command.ParseMode
  -> ExceptT Error.Error IO [Syntax.Block]
executeWithModeE path pmode =
  evaluateWithModeE path pmode >>= Execute.execute path
