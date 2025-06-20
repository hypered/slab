-- |
-- Module      : Slab.Run
-- Description : Implementation of Slab's CLI
--
-- @Slab.Run@ accepts commands defined in the "Slab.Command" module and
-- runs them.
module Slab.Run
  ( run
  , parse
  , eval
  , render
  , calc
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, withExceptT)
import Data.Text (Text)
import Data.Text qualified as T
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
import System.FilePath (takeExtension)
import Text.Pretty.Simple (pPrintNoColor, pShowNoColor)

--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.Build srcDir renderMode passthrough distDir) =
  Build.buildDir srcDir renderMode passthrough distDir
run (Command.Watch srcDir renderMode passthrough distDir) =
  Watch.run srcDir $ \path -> do
    when (takeExtension path == ".slab") $
      Build.buildFile srcDir renderMode passthrough distDir path
run (Command.Serve srcDir distDir) = Serve.run srcDir distDir
run (Command.ReportPages srcDir) = Report.reportPages srcDir
run (Command.ReportHeadings path) = Report.reportHeadings path
run (Command.ReportElement i mpath) = Report.reportElement i mpath
run (Command.Generate path) = Generate.renderHs path
run (Command.CommandWithPath path pmode (Command.Render Command.RenderNormal passthrough)) = do
  nodes <- executeWithMode path pmode passthrough >>= Error.unwrap
  TL.putStrLn . Render.renderHtmls $ Render.renderBlocks nodes
run (Command.CommandWithPath path pmode (Command.Render Command.RenderPretty passthrough)) = do
  nodes <- executeWithMode path pmode passthrough >>= Error.unwrap
  T.putStr . Render.prettyHtmls $ Render.renderBlocks nodes
run (Command.CommandWithPath path pmode (Command.Execute simpl passthrough)) = do
  nodes <- executeWithMode path pmode passthrough >>= Error.unwrap
  if simpl
    then TL.putStrLn $ pShowNoColor $ Evaluate.simplify nodes
    else TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode (Command.Evaluate simpl mname)) = do
  case mname of
    Nothing -> do
      nodes <- evaluateWithMode path pmode >>= Error.unwrap
      if simpl
        then TL.putStrLn $ pShowNoColor $ Evaluate.simplify nodes
        else TL.putStrLn $ pShowNoColor nodes
    Just name -> do
      val <- evaluateVarWithMode path pmode name >>= Error.unwrap
      if simpl
        then TL.putStrLn $ pShowNoColor $ Evaluate.simplifyVal val
        else TL.putStrLn $ pShowNoColor val
run (Command.CommandWithPath mpath pmode Command.Parse) = do
  nodes <- parseWithMode mpath pmode >>= Error.unwrap
  TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath mpath pmode Command.Classes) = do
  nodes <- parseWithMode mpath pmode >>= Error.unwrap
  mapM_ T.putStrLn $ Syntax.extractClasses nodes
run (Command.CommandWithPath mpath pmode (Command.Fragments mname)) = do
  nodes <- parseWithMode mpath pmode >>= Error.unwrap
  let ms = Syntax.extractFragments nodes
  case mname of
    Just name -> case Syntax.findFragment name ms of
      Just m -> TL.putStrLn $ pShowNoColor m
      Nothing -> putStrLn "No such fragment."
    Nothing -> TL.putStrLn $ pShowNoColor ms

--------------------------------------------------------------------------------
parseWithMode
  :: (Maybe FilePath)
  -> Command.ParseMode
  -> IO (Either Error.Error [Syntax.Block])
parseWithMode mpath pmode = runExceptT $ parseWithModeE mpath pmode

evaluateWithMode
  :: Maybe FilePath
  -> Command.ParseMode
  -> IO (Either Error.Error [Syntax.Block])
evaluateWithMode mpath pmode = runExceptT $ evaluateWithModeE mpath pmode

evaluateVarWithMode
  :: Maybe FilePath
  -> Command.ParseMode
  -> Text
  -> IO (Either Error.Error Syntax.Expr)
evaluateVarWithMode mpath pmode name = runExceptT $ evaluateVarWithModeE mpath pmode name

executeWithMode
  :: Maybe FilePath
  -> Command.ParseMode
  -> Command.RunMode
  -> IO (Either Error.Error [Syntax.Block])
executeWithMode mpath pmode passthrough =
  runExceptT $ executeWithModeE mpath pmode passthrough

--------------------------------------------------------------------------------
parseWithModeE
  :: Maybe FilePath
  -> Command.ParseMode
  -> ExceptT Error.Error IO [Syntax.Block]
parseWithModeE mpath pmode =
  case pmode of
    Command.ParseShallow -> Parse.parseFileE mpath
    Command.ParseDeep -> PreProcess.preprocessFileE mpath

evaluateWithModeE
  :: Maybe FilePath
  -> Command.ParseMode
  -> ExceptT Error.Error IO [Syntax.Block]
evaluateWithModeE mpath pmode = do
  parsed <- parseWithModeE mpath pmode
  Evaluate.evaluate Evaluate.defaultEnv [maybe "-" T.pack mpath] parsed

evaluateVarWithModeE
  :: Maybe FilePath
  -> Command.ParseMode
  -> Text
  -> ExceptT Error.Error IO Syntax.Expr
evaluateVarWithModeE mpath pmode name = do
  parsed <- parseWithModeE mpath pmode
  Evaluate.evaluateVar Evaluate.defaultEnv [maybe "-" T.pack mpath] parsed name

executeWithModeE
  :: Maybe FilePath
  -> Command.ParseMode
  -> Command.RunMode
  -> ExceptT Error.Error IO [Syntax.Block]
executeWithModeE mpath pmode passthrough =
  evaluateWithModeE mpath pmode
    >>= Execute.execute (Execute.Context mpath passthrough)

--------------------------------------------------------------------------------
-- Play with the whole language.

parse :: Text -> IO ()
parse s = do
  blocks <- runExceptT $ withExceptT Error.ParseError . except $ Parse.parse "-" s
  pPrintNoColor blocks

-- | "eval" parses a string as a "Syntax.Syntax", and evaluates it. This doesn't
-- run the proprocessing stage.
--
-- @
--     Run.eval "p= 1 + 2 * 3"
-- @
eval :: Text -> IO ()
eval s = do
  x <- runExceptT $ parseAndEvaluateBlocks s
  pPrintNoColor x

-- | Run "eval" and render the result.
render :: Text -> IO ()
render s = do
  x <- runExceptT (parseAndEvaluateBlocks s) >>= Error.unwrap
  T.putStr . Render.prettyHtmls $ Render.renderBlocks x

parseAndEvaluateBlocks :: Text -> ExceptT Error.Error IO [Syntax.Block]
parseAndEvaluateBlocks s = do
  blocks <- withExceptT Error.ParseError . except $ Parse.parse "-" s
  Evaluate.evaluate Evaluate.defaultEnv [] blocks

--------------------------------------------------------------------------------
-- Play with the expression language.

-- | "calc" parses a string as a "Syntax.Expr", and evaluates it. I.e. it
-- doens't use the fragment syntax, or imports and includes.
--
-- @
--     Run.calc "1 + 2 * 3"
-- @
calc :: Text -> IO ()
calc s = do
  x <- runExceptT $ parseAndEvaluateExpr s
  pPrintNoColor x

parseAndEvaluateExpr :: Text -> ExceptT Error.Error IO Syntax.Expr
parseAndEvaluateExpr s = do
  expr <- withExceptT Error.ParseError . except $ Parse.parseExpr s
  Evaluate.evalExpr Evaluate.defaultEnv expr
