module Pughs.Run where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Pughs.Command qualified as Command
import Pughs.Parse qualified as Parse
import Pughs.Render qualified as Render
import qualified Text.Blaze.Html5 as H
import Text.Megaparsec hiding (parse)
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.CommandWithPath path pmode (Command.Render Command.RenderNormal)) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left (Parse.PreProcessParseError err) -> T.putStrLn . T.pack $ errorBundlePretty err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> TL.putStrLn . Render.renderHtmls $ Render.pugNodesToHtml nodes
run (Command.CommandWithPath path pmode (Command.Render Command.RenderPretty)) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left (Parse.PreProcessParseError err) -> T.putStrLn . T.pack $ errorBundlePretty err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> T.putStrLn . Render.prettyHtmls $ Render.pugNodesToHtml nodes
run (Command.CommandWithPath path pmode Command.Parse) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left (Parse.PreProcessParseError err) ->
      T.putStrLn . Parse.parseErrorPretty $ err
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> TL.putStrLn $ pShowNoColor nodes
run (Command.CommandWithPath path pmode Command.Classes) = do
  parsed <- parseWithMode path pmode
  case parsed of
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> mapM_ T.putStrLn $ Parse.extractClasses nodes

--------------------------------------------------------------------------------
parseWithMode :: FilePath
              -> Command.ParseMode
              -> IO (Either Parse.PreProcessError [Parse.PugNode])
parseWithMode path pmode =
  case pmode of
    Command.ParseShallow -> first Parse.PreProcessParseError <$> Parse.parsePugFile path
    Command.ParseDeep -> Parse.preProcessPugFile path

--------------------------------------------------------------------------------
renderPretty :: FilePath -> IO (Either Text Text)
renderPretty path = do
  pugContent <- T.readFile path
  let parsedHtml = parse path pugContent
  pure $ fmap Render.prettyHtmls parsedHtml

parse :: FilePath -> Text -> Either Text [H.Html]
parse fn =
  either
    (Left . T.pack . errorBundlePretty)
    (Right . Render.pugNodesToHtml)
  . Parse.parsePug fn
