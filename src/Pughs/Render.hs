module Pughs.Render where

import Data.String (fromString)
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
pugNodeToHtml Parse.PugDoctype = H.docType

pugNodeToHtml (Parse.PugElem name mdot attrs children) =
  mAddAttr $ mAddClass $ pugElemToHtml name $ mconcat $
    if mdot == Parse.HasDot
    then
      [pugTextsToHtml children]
    else
      map pugNodeToHtml children
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
          Parse.AttrList pairs -> concatMap f pairs
      )
      attrs
  f ("class", Just x) = [x]
  f _ = []
  classNames' :: Text
  classNames' = T.intercalate " " classNames

  mAddAttr :: H.Html -> H.Html
  mAddAttr =
    flip (foldl (\e (a, b) -> e ! H.customAttribute (fromString a) (H.toValue b))) attrs'
  attrs' =
    concatMap
      ( \case
          Parse.Class _ -> []
          Parse.AttrList pairs -> concatMap g pairs
      )
      attrs
  g ("class", _) = []
  g (a, Just b) = [(T.unpack a, b)]
  g (a, Nothing) = [(T.unpack a, a)]

pugNodeToHtml (Parse.PugText _ s) | s == T.empty = mempty
                                  | otherwise = H.preEscapedText s -- TODO

pugNodeToHtml (Parse.PugInclude _ (Just nodes)) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Parse.PugInclude path Nothing) = H.stringComment $ "include " <> path

pugNodeToHtml (Parse.PugComment _) = mempty -- TODO Should it appear in the HTML ?

pugTextsToHtml :: [Parse.PugNode] -> H.Markup
pugTextsToHtml xs = H.preEscapedText xs'
 where
  xs' = T.intercalate "\n" $ map f xs
  f Parse.PugDoctype = error "pugTextsToHtml called on a PugDoctype"
  f (Parse.PugElem _ _ _ _) = error "pugTextsToHtml called on a PugElem"
  f (Parse.PugText _ s) = s
  f (Parse.PugInclude _ _) = error "pugTextsToHtml called on a PugInclude"
  f (Parse.PugComment _) = error "pugTextsToHtml called on a PugComment"

pugElemToHtml :: Parse.Elem -> Html -> Html
pugElemToHtml = \case
  Parse.Html -> H.html
  Parse.Body -> H.body
  Parse.Div -> H.div
  Parse.Span -> H.span
  Parse.Hr -> const H.hr
  Parse.H1 -> H.h1
  Parse.H2 -> H.h2
  Parse.H3 -> H.h3
  Parse.H4 -> H.h4
  Parse.H5 -> H.h5
  Parse.H6 -> H.h6
  Parse.Header -> H.header
  Parse.Head -> H.head
  Parse.Meta -> const H.meta
  Parse.Main -> H.main
  Parse.Link -> const H.link
  Parse.A -> H.a
  Parse.P -> H.p
  Parse.Ul -> H.ul
  Parse.Li -> H.li
  Parse.Footer -> H.footer
  Parse.Figure -> H.figure
  Parse.Blockquote -> H.blockquote
  Parse.Figcaption -> H.figcaption
  Parse.Audio -> H.audio
  Parse.Small -> H.small
  Parse.Source -> const H.source
  Parse.Pre -> H.pre
  Parse.Code -> H.code
  Parse.Img -> const H.img
  Parse.I -> H.i
