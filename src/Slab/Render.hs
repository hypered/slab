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
renderBlock (Syntax.BlockElem name mdot attrs children) =
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
renderBlock (Syntax.BlockText _ []) =
  H.preEscapedText "\n" -- This allows to force some whitespace.
renderBlock (Syntax.BlockText _ [Syntax.Lit s])
  | s == T.empty = H.preEscapedText "\n" -- This allows to force some whitespace.
  | otherwise = H.preEscapedText s -- TODO
renderBlock (Syntax.BlockText _ _) = error "Template is not rendered."
renderBlock (Syntax.BlockInclude (Just "escape-html") _ (Just nodes)) =
  escapeTexts nodes
renderBlock (Syntax.BlockInclude _ _ (Just nodes)) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockInclude _ path Nothing) = H.stringComment $ "include " <> path
renderBlock (Syntax.BlockFragmentDef _ _ _) = mempty
renderBlock (Syntax.BlockFragmentCall _ _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockFor _ _ _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockComment b content) =
  case b of
    Syntax.PassthroughComment -> H.textComment content
    Syntax.NormalComment -> mempty
renderBlock (Syntax.BlockFilter "escape-html" content) =
  H.text content
renderBlock (Syntax.BlockFilter name _) = error $ "Unknown filter name " <> T.unpack name
renderBlock (Syntax.BlockRawElem content children) = do
  H.preEscapedText content -- TODO Construct a proper tag ?
  mapM_ renderBlock children
renderBlock (Syntax.BlockDefault _ nodes) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockImport _ (Just nodes) _) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockRun _ (Just nodes)) = mapM_ renderBlock nodes
renderBlock (Syntax.BlockRun cmd _) = H.textComment $ "run " <> cmd
renderBlock (Syntax.BlockImport path Nothing _) = H.stringComment $ "extends " <> path
renderBlock (Syntax.BlockReadJson _ _ _) = mempty
renderBlock (Syntax.BlockAssignVar _ _) = mempty
renderBlock (Syntax.BlockIf _ as bs) = do
  -- The evaluation code transforms a BlockIf into a BlockList, so this should
  -- not be called.
  mapM_ renderBlock as
  mapM_ renderBlock bs
renderBlock (Syntax.BlockList nodes) =
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
  xs' = T.intercalate "\n" $ map extractText xs

escapeTexts :: [Syntax.Block] -> H.Html
escapeTexts xs = H.text xs'
 where
  xs' = T.intercalate "\n" $ map extractText xs

extractText :: Syntax.Block -> Text
extractText = f
 where
  f Syntax.BlockDoctype = error "extractTexts called on a BlockDoctype"
  f (Syntax.BlockElem _ _ _ _) = error "extractTexts called on a BlockElem"
  f (Syntax.BlockText _ [Syntax.Lit s]) = s
  f (Syntax.BlockText _ _) = error "extractTexts called on unevaluated BlockText"
  f (Syntax.BlockInclude _ _ _) = error "extractTexts called on a BlockInclude"
  f (Syntax.BlockFragmentDef _ _ _) = error "extractTexts called on a BlockFragmentDef"
  f (Syntax.BlockFragmentCall _ _ _) = error "extractTexts called on a BlockFragmentCall"
  f (Syntax.BlockFor _ _ _ _) = error "extractTexts called on a BlockFor"
  f (Syntax.BlockComment _ _) = error "extractTexts called on a BlockComment"
  f (Syntax.BlockFilter _ _) = error "extractTexts called on a BlockFilter"
  f (Syntax.BlockRawElem _ _) = error "extractTexts called on a BlockRawElem"
  f (Syntax.BlockDefault _ _) = error "extractTexts called on a BlockDefault"
  f (Syntax.BlockImport _ _ _) = error "extractTexts called on a BlockImport"
  f (Syntax.BlockRun _ _) = error "extractTexts called on a BlockRun"
  f (Syntax.BlockReadJson _ _ _) = error "extractTexts called on a BlockReadJson"
  f (Syntax.BlockAssignVar _ _) = error "extractTexts called on a BlockAssignVar"
  f (Syntax.BlockIf _ _ _) = error "extractTexts called on a BlockIf"
  f (Syntax.BlockList _) = error "extractTexts called on a BlockList"
  f (Syntax.BlockCode _) = error "extractTexts called on a BlockCode"

renderElem :: Syntax.Elem -> Html -> Html
renderElem = \case
  Syntax.Html -> H.html
  Syntax.Body -> H.body
  Syntax.Div -> H.div
  Syntax.Span -> H.span
  Syntax.Br -> const H.br
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
