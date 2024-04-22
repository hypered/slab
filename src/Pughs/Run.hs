module Pughs.Run where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pughs.Command qualified as Command
import Pughs.Parse qualified as Parse
import Pughs.Render qualified as Render
import qualified Text.Blaze.Html5 as H
import Text.Megaparsec hiding (parse)

--------------------------------------------------------------------------------
run :: Command.Command -> IO ()
run (Command.Render path) = do
  mrenderedHtml <- render path
  either T.putStrLn T.putStrLn mrenderedHtml

render :: FilePath -> IO (Either Text Text)
render path = do
  pugContent <- T.readFile path
  let parsedHtml = parse pugContent
  pure $ fmap Render.renderHtmls parsedHtml

--------------------------------------------------------------------------------
parse :: Text -> Either Text [H.Html]
parse =
  either
    (Left . T.pack . errorBundlePretty)
    (Right . Render.pugNodesToHtml)
  . Parse.parsePug
