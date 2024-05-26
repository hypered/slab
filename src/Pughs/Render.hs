module Pughs.Render
  ( prettyHtmls
  , renderHtmls
  , pugNodesToHtml
  ) where

import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Pughs.Syntax qualified as Syntax
import Text.Blaze.Html.Renderer.Pretty qualified as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Svg11 qualified as S

--------------------------------------------------------------------------------
prettyHtmls :: [Html] -> Text
prettyHtmls = T.pack . concat . map Pretty.renderHtml

renderHtmls :: [Html] -> TL.Text
renderHtmls = TL.concat . map renderHtml

--------------------------------------------------------------------------------
pugNodesToHtml :: [Syntax.PugNode] -> [H.Html]
pugNodesToHtml = map pugNodeToHtml

pugNodeToHtml :: Syntax.PugNode -> H.Html
pugNodeToHtml Syntax.PugDoctype = H.docType
pugNodeToHtml (Syntax.PugElem name mdot attrs children) =
  mAddAttr $
    mAddId $
    mAddClass $
      pugElemToHtml name $
        mconcat $
          if mdot == Syntax.HasDot
            then [pugTextsToHtml children]
            else map pugNodeToHtml children
 where
  mAddId :: H.Html -> H.Html
  mAddId e =
    if idNames == []
      then e
      else e ! A.id (H.toValue idNames')
  idNames =
    concatMap
      ( \case
          Syntax.Id i -> [i]
          Syntax.Class _ -> []
          Syntax.AttrList pairs -> concatMap fId pairs
      )
      attrs
  fId ("id", Just x) = [x]
  fId _ = []
  idNames' :: Text
  idNames' = T.intercalate "-" idNames -- TODO Refuse multiple Ids in some kind of validation step after parsing ?
  mAddClass :: H.Html -> H.Html
  mAddClass e =
    if classNames == []
      then e
      else e ! A.class_ (H.toValue classNames')
  classNames =
    concatMap
      ( \case
          Syntax.Id _ -> []
          Syntax.Class c -> [c]
          Syntax.AttrList pairs -> concatMap f pairs
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
          Syntax.Id _ -> []
          Syntax.Class _ -> []
          Syntax.AttrList pairs -> concatMap g pairs
      )
      attrs
  g ("id", _) = []
  g ("class", _) = []
  g (a, Just b) = [(T.unpack a, b)]
  g (a, Nothing) = [(T.unpack a, a)]
pugNodeToHtml (Syntax.PugText _ s)
  | s == T.empty = mempty
  | otherwise = H.preEscapedText s -- TODO
pugNodeToHtml (Syntax.PugCode (Syntax.SingleQuoteString s))
  | s == T.empty = mempty
  | otherwise = H.text s -- Should be already escaped in the AST ?
pugNodeToHtml (Syntax.PugCode (Syntax.Variable s)) =
  H.textComment $ "code variable " <> s
pugNodeToHtml (Syntax.PugInclude _ (Just nodes)) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugInclude path Nothing) = H.stringComment $ "include " <> path
pugNodeToHtml (Syntax.PugMixinDef _ _) = mempty
pugNodeToHtml (Syntax.PugMixinCall _ (Just nodes)) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugMixinCall name Nothing) = H.textComment $ "+" <> name
pugNodeToHtml (Syntax.PugFragmentDef _ _) = mempty
pugNodeToHtml (Syntax.PugFragmentCall _ nodes) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugEach _ _ nodes) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugComment b content) =
  if b then H.textComment content else mempty
pugNodeToHtml (Syntax.PugFilter "escape-html" content) =
  H.text content
pugNodeToHtml (Syntax.PugFilter name _) = error $ "Unknown filter name " <> T.unpack name
pugNodeToHtml (Syntax.PugRawElem content children) = do
  H.preEscapedText content -- TODO Construct a proper tag ?
  mapM_ pugNodeToHtml children
pugNodeToHtml (Syntax.PugBlock _ _ nodes) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugExtends _ (Just nodes)) = mapM_ pugNodeToHtml nodes
pugNodeToHtml (Syntax.PugExtends path Nothing) = H.stringComment $ "extends " <> path

pugTextsToHtml :: [Syntax.PugNode] -> H.Markup
pugTextsToHtml xs = H.preEscapedText xs'
 where
  xs' = T.intercalate "\n" $ map f xs
  f Syntax.PugDoctype = error "pugTextsToHtml called on a PugDoctype"
  f (Syntax.PugElem _ _ _ _) = error "pugTextsToHtml called on a PugElem"
  f (Syntax.PugText _ s) = s
  f (Syntax.PugCode _) = error "pugTextsToHtml called on a PugCode"
  f (Syntax.PugInclude _ _) = error "pugTextsToHtml called on a PugInclude"
  f (Syntax.PugMixinDef _ _) = error "pugTextsToHtml called on a PugMixinDef"
  f (Syntax.PugMixinCall _ _) = error "pugTextsToHtml called on a PugMixinCall"
  f (Syntax.PugFragmentDef _ _) = error "pugTextsToHtml called on a PugFragmentDef"
  f (Syntax.PugFragmentCall _ _) = error "pugTextsToHtml called on a PugFragmentCall"
  f (Syntax.PugEach _ _ _) = error "pugTextsToHtml called on a PugEach"
  f (Syntax.PugComment _ _) = error "pugTextsToHtml called on a PugComment"
  f (Syntax.PugFilter _ _) = error "pugTextsToHtml called on a PugFilter"
  f (Syntax.PugRawElem _ _) = error "pugTextsToHtml called on a PugRawElem"
  f (Syntax.PugBlock _ _ _) = error "pugTextsToHtml called on a PugBlock"
  f (Syntax.PugExtends _ _) = error "pugTextsToHtml called on a PugExtends"

pugElemToHtml :: Syntax.Elem -> Html -> Html
pugElemToHtml = \case
  Syntax.Html -> H.html
  Syntax.Body -> H.body
  Syntax.Div -> H.div
  Syntax.Span -> H.span
  Syntax.Hr -> const H.hr
  Syntax.H1 -> H.h1
  Syntax.H2 -> H.h2
  Syntax.H3 -> H.h3
  Syntax.H4 -> H.h4
  Syntax.H5 -> H.h5
  Syntax.H6 -> H.h6
  Syntax.Header -> H.header
  Syntax.Head -> H.head
  Syntax.Meta -> const H.meta
  Syntax.Main -> H.main
  Syntax.Link -> const H.link
  Syntax.A -> H.a
  Syntax.P -> H.p
  Syntax.Ul -> H.ul
  Syntax.Li -> H.li
  Syntax.Title -> H.title
  Syntax.Table -> H.table
  Syntax.Thead -> H.thead
  Syntax.Tbody -> H.tbody
  Syntax.Tr -> H.tr
  Syntax.Td -> H.td
  Syntax.Dl -> H.dl
  Syntax.Dt -> H.dt
  Syntax.Dd -> H.dd
  Syntax.Footer -> H.footer
  Syntax.Figure -> H.figure
  Syntax.Form -> H.form
  Syntax.Label -> H.label
  Syntax.Blockquote -> H.blockquote
  Syntax.Button -> H.button
  Syntax.Figcaption -> H.figcaption
  Syntax.Audio -> H.audio
  Syntax.Script -> H.script
  Syntax.Style -> H.style
  Syntax.Small -> H.small
  Syntax.Source -> const H.source
  Syntax.Pre -> H.pre
  Syntax.Code -> H.code
  Syntax.Img -> const H.img
  Syntax.IFrame -> H.iframe
  Syntax.Input -> const H.input
  Syntax.I -> H.i
  Syntax.Svg -> S.svg
