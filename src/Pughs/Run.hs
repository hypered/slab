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
run Command.Render = do
  pugContent <- T.readFile "example.pug"
  let parsedHtml = parse pugContent
  either T.putStrLn putStrLn $ fmap Render.renderHtmls parsedHtml

--------------------------------------------------------------------------------
parse :: Text -> Either Text [H.Html]
parse =
  either
    (Left . T.pack . errorBundlePretty)
    (Right . Render.pugNodesToHtml)
  . Parse.parsePug
