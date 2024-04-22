module Pughs.Render where

import Data.Text (Text)
import Data.Text qualified as T
import Pughs.Parse qualified as Parse
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

--------------------------------------------------------------------------------
renderHtmls :: [Html] -> Text
renderHtmls = T.pack . concat . map renderHtml

pugNodesToHtml :: [Parse.PugNode] -> [H.Html]
pugNodesToHtml = map pugNodeToHtml

pugNodeToHtml :: Parse.PugNode -> H.Html
pugNodeToHtml (Parse.PugDiv className children) =
  H.div ! A.class_ (H.toValue className) $ mconcat $ map pugNodeToHtml children
