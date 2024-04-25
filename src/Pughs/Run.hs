module Pughs.Run where

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
run (Command.Render Command.RenderNormal path) = do
  pugContent <- T.readFile path
  let parsedHtml = parse pugContent
      mrenderedHtml = fmap Render.renderHtmls parsedHtml
  either T.putStrLn TL.putStrLn mrenderedHtml
run (Command.Render Command.RenderPretty path) = do
  mrenderedHtml <- renderPretty path
  either T.putStrLn T.putStrLn mrenderedHtml
run (Command.Parse path) = do
  pugContent <- T.readFile path
  let parsed = Parse.parsePug pugContent
  TL.putStrLn $ pShowNoColor parsed
run (Command.Classes path) = do
  pugContent <- T.readFile path
  let parsed = Parse.parsePug pugContent
  case parsed of
    Left err -> TL.putStrLn $ pShowNoColor err
    Right nodes -> mapM_ T.putStrLn $ Parse.extractClasses nodes

--------------------------------------------------------------------------------
renderPretty :: FilePath -> IO (Either Text Text)
renderPretty path = do
  pugContent <- T.readFile path
  let parsedHtml = parse pugContent
  pure $ fmap Render.prettyHtmls parsedHtml

parse :: Text -> Either Text [H.Html]
parse =
  either
    (Left . T.pack . errorBundlePretty)
    (Right . Render.pugNodesToHtml)
  . Parse.parsePug
