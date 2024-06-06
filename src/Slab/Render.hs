module Slab.Render
  ( prettyHtmls
  , renderHtmls
  , renderBlocks
  ) where

import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Slab.Syntax qualified as Syntax
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
renderBlocks :: [Syntax.Block] -> [H.Html]
renderBlocks = map renderBlock

renderBlock :: Syntax.Block -> H.Html
renderBlock Syntax.BlockDoctype = H.docType
renderBlock (Syntax.PugElem name mdot attrs children) =
  mAddAttrs $
    mAddId $
      mAddClass $
        renderElem name $
          mconcat $
            if mdot == Syntax.HasDot
              then [renderTexts children]
              else map renderBlock children
 where
  mAddId :: H.Html -> H.Html
  mAddId e =
    if idNames == []
      then e
      else e ! A.id (H.toValue idNames')
  idNames = Syntax.idNamesFromAttrs attrs
  idNames' :: Text
  idNames' = T.intercalate "-" idNames -- TODO Refuse multiple Ids in some kind of validation step after parsing ?

  mAddClass :: H.Html -> H.Html
  mAddClass e =
    if classNames == []
      then e
      else e ! A.class_ (H.toValue classNames')
  classNames = Syntax.classNamesFromAttrs attrs
  classNames' :: Text
  classNames' = T.intercalate " " classNames

  mAddAttrs :: H.Html -> H.Html
  mAddAttrs =
    flip (foldl (\e (a, b) -> e ! H.customAttribute (fromString $ T.unpack a) (H.toValue b))) attrs'
  attrs' = Syntax.namesFromAttrs attrs
renderBlock (Syntax.PugText _ []) =
  H.preEscapedText "\n" -- This allows to force some whitespace.
renderBlock (Syntax.PugText _ [Syntax.Lit s])
  | s == T.empty = H.preEscapedText "\n" -- This allows to force some whitespace.
  | otherwise = H.preEscapedText s -- TODO
renderBlock (Syntax.PugText _ _) = error "Template is not rendered."
renderBlock (Syntax.PugInclude _ (Just nodes)) = mapM_ renderBlock nodes
renderBlock (Syntax.PugInclude path Nothing) = H.stringComment $ "include " <> path
renderBlock (Syntax.PugFragmentDef _ _ _) = mempty
renderBlock (Syntax.PugFragmentCall _ _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.PugEach _ _ _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.PugComment b content) =
  if b then H.textComment content else mempty
renderBlock (Syntax.PugFilter "escape-html" content) =
  H.text content
renderBlock (Syntax.PugFilter name _) = error $ "Unknown filter name " <> T.unpack name
renderBlock (Syntax.PugRawElem content children) = do
  H.preEscapedText content -- TODO Construct a proper tag ?
  mapM_ renderBlock children
renderBlock (Syntax.PugDefault _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.PugImport _ (Just nodes) _) = mapM_ renderBlock nodes
renderBlock (Syntax.PugImport path Nothing _) = H.stringComment $ "extends " <> path
renderBlock (Syntax.PugReadJson _ _ _) = mempty
renderBlock (Syntax.PugAssignVar _ _) = mempty
renderBlock (Syntax.PugIf _ as bs) = do
  -- The evaluation code transforms a PugIf into a PugList, so this should
  -- not be called.
  mapM_ renderBlock as
  mapM_ renderBlock bs
renderBlock (Syntax.PugList nodes) =
  mapM_ renderBlock nodes
renderBlock (Syntax.BlockCode (Syntax.SingleQuoteString s))
  | s == T.empty = mempty
  | otherwise = H.text s -- Should be already escaped in the AST ?
renderBlock (Syntax.BlockCode (Syntax.Variable s)) =
  H.textComment $ "code variable " <> s
renderBlock (Syntax.BlockCode (Syntax.Int i)) =
  H.string $ show i
renderBlock (Syntax.BlockCode (Syntax.Object _)) =
  H.text "<Object>"
renderBlock (Syntax.BlockCode c) = error $ "renderBlock called on BlockCode " <> show c

renderTexts :: [Syntax.Block] -> H.Html
renderTexts xs = H.preEscapedText xs'
 where
  xs' = T.intercalate "\n" $ map f xs
  f Syntax.BlockDoctype = error "renderTexts called on a BlockDoctype"
  f (Syntax.PugElem _ _ _ _) = error "renderTexts called on a PugElem"
  f (Syntax.PugText _ [Syntax.Lit s]) = s
  f (Syntax.PugText _ _) = error "renderTexts called on unevaluated PugText"
  f (Syntax.PugInclude _ _) = error "renderTexts called on a PugInclude"
  f (Syntax.PugFragmentDef _ _ _) = error "renderTexts called on a PugFragmentDef"
  f (Syntax.PugFragmentCall _ _ _) = error "renderTexts called on a PugFragmentCall"
  f (Syntax.PugEach _ _ _ _) = error "renderTexts called on a PugEach"
  f (Syntax.PugComment _ _) = error "renderTexts called on a PugComment"
  f (Syntax.PugFilter _ _) = error "renderTexts called on a PugFilter"
  f (Syntax.PugRawElem _ _) = error "renderTexts called on a PugRawElem"
  f (Syntax.PugDefault _ _) = error "renderTexts called on a PugDefault"
  f (Syntax.PugImport _ _ _) = error "renderTexts called on a PugImport"
  f (Syntax.PugReadJson _ _ _) = error "renderTexts called on a PugReadJson"
  f (Syntax.PugAssignVar _ _) = error "renderTexts called on a PugAssignVar"
  f (Syntax.PugIf _ _ _) = error "renderTexts called on a PugIf"
  f (Syntax.PugList _) = error "renderTexts called on a PugList"
  f (Syntax.BlockCode _) = error "renderTexts called on a BlockCode"

renderElem :: Syntax.Elem -> Html -> Html
renderElem = \case
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
  Syntax.Textarea -> H.textarea
  Syntax.Canvas -> H.canvas
