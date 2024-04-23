module Pughs.Render where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Pughs.Parse qualified as Parse
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html.Renderer.Pretty qualified as Pretty (renderHtml)

--------------------------------------------------------------------------------
prettyHtmls :: [Html] -> Text
prettyHtmls = T.pack . concat . map Pretty.renderHtml

renderHtmls :: [Html] -> TL.Text
renderHtmls = TL.concat . map renderHtml

--------------------------------------------------------------------------------
pugNodesToHtml :: [Parse.PugNode] -> [H.Html]
pugNodesToHtml = map pugNodeToHtml

pugNodeToHtml :: Parse.PugNode -> H.Html
pugNodeToHtml (Parse.PugElem name attrs children) =
  mAddClass $ pugElemToHtml name $ mconcat $ map pugNodeToHtml children
 where
  mAddClass :: H.Html -> H.Html
  mAddClass e =
    if classNames == []
    then e
    else e ! A.class_ (H.toValue classNames')
  classNames =
    concatMap
      ( \case
          Parse.Class c -> [c]
          Parse.AttrList pairs -> concatMap (f) pairs
      )
      attrs
  f ("class", x) = [x]
  f _ = []
  classNames' :: Text
  classNames' = T.intercalate " " classNames

pugNodeToHtml (Parse.PugText _ s) | s == T.empty = mempty
                                  | otherwise = H.preEscapedText s -- TODO

pugElemToHtml :: Parse.Elem -> Html -> Html
pugElemToHtml = \case
  Parse.Html -> H.html
  Parse.Body -> H.body
  Parse.Div -> H.div
  Parse.Span -> H.span
  Parse.H1 -> H.h1
  Parse.A -> H.a
  Parse.P -> H.p
  Parse.Ul -> H.ul
  Parse.Li -> H.li
  Parse.Figure -> H.figure
  Parse.Blockquote -> H.blockquote
  Parse.Figcaption -> H.figcaption
