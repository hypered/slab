module Slab.Run
  ( run
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Slab.Build qualified as Build
import Slab.Command qualified as Command
import Slab.Evaluate qualified as Evaluate
import Slab.Parse qualified as Parse
import Slab.Render qualified as Render
import Slab.Report qualified as Report
import Slab.Serve qualified as Serve
import Slab.Syntax qualified as Syntax
import Slab.Watch qualified as Watch
import Text.Megaparsec hiding (parse)
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.Build srcDir renderMode distDir) = Build.buildDir srcDir renderMode distDir
run (Command.Watch srcDir renderMode distDir) =
  Watch.run srcDir (Build.buildFile srcDir renderMode distDir)
run (Command.Serve distDir) = Serve.run distDir
run (Command.Report srcDir) = Report.run srcDir
run (Command.CommandWithPath path pmode (Command.Render Command.RenderNormal)) = do
  evaluated <- evaluateWithMode path pmode
  case evaluated of
    Left (Evaluate.PreProcessParseError err) -> T.putStrLn . T.pack $ errorBundlePretty err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> TL.putStrLn . Render.renderHtmls $ Render.nodesToHtml nodes
run (Command.CommandWithPath path pmode (Command.Render Command.RenderPretty)) = do
  evaluated <- evaluateWithMode path pmode
  case evaluated of
    Left (Evaluate.PreProcessParseError err) -> T.putStrLn . T.pack $ errorBundlePretty err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> T.putStr . Render.prettyHtmls $ Render.nodesToHtml nodes
run (Command.CommandWithPath path pmode (Command.Evaluate simpl)) = do
  evaluated <- evaluateWithMode path pmode
  case evaluated of
    Left (Evaluate.PreProcessParseError err) ->
      T.putStrLn . Parse.parseErrorPretty $ err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes ->
      if simpl
        then TL.putStrLn $ pShowNoColor $ Evaluate.simplify nodes
        else TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode Command.Parse) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left (Evaluate.PreProcessParseError err) ->
      T.putStrLn . Parse.parseErrorPretty $ err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode Command.Classes) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> mapM_ T.putStrLn $ Syntax.extractClasses nodes
run (Command.CommandWithPath path pmode (Command.Fragments mname)) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> do
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
  -> IO (Either Evaluate.PreProcessError [Syntax.Block])
parseWithMode path pmode =
  case pmode of
    Command.ParseShallow -> first Evaluate.PreProcessParseError <$> Parse.parseFile path
    Command.ParseDeep -> Evaluate.preprocessFile path

evaluateWithMode
  :: FilePath
  -> Command.ParseMode
  -> IO (Either Evaluate.PreProcessError [Syntax.Block])
evaluateWithMode path pmode = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left err -> pure $ Left err
    Right nodes -> do
      evaluated <- runExceptT $ Evaluate.evaluate Evaluate.defaultEnv ["toplevel"] nodes
      pure evaluated
