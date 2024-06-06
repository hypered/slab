module Slab.Generate.Haskell
  ( renderHs
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import Slab.Evaluate qualified as Evaluate
import Slab.Syntax qualified as Syntax

--------------------------------------------------------------------------------
renderHs :: FilePath -> IO ()
renderHs path = do
  preprocessed <- Evaluate.preprocessFile path
  case preprocessed of
    Left err -> print err
    Right blocks -> T.putStrLn $ renderBlocks blocks

renderBlocks :: [Syntax.Block] -> Text
renderBlocks = renderStrict . layoutPretty defaultLayoutOptions . prettyBlocks

--------------------------------------------------------------------------------
prettyBlocks :: [Syntax.Block] -> Doc ann
prettyBlocks = vsep . map prettyBlock

prettyBlock :: Syntax.Block -> Doc ann
prettyBlock (Syntax.PugElem name _ attrs children) = prettyPugElem (name, attrs', children)
 where
  attrs' = Syntax.groupAttrs attrs

-- | Render an element, aligning the @!@ character:
--
-- @
--   elem ! a
--        ! b $ do
--     child0
--     child1
-- @
prettyPugElem :: (Syntax.Elem, [Syntax.Attr], [Syntax.Block]) -> Doc ann
prettyPugElem (t1, ts_, as) =
  case ts_ of
    [] ->
      let header = prettyT1 <+> dollar
      in vsep [header, footer]
    [t] ->
      let header = prettyT1 <+> "!" <+> prettyAttr t <+> dollar
      in vsep [header, footer]
    t:ts ->
      let header = prettyT1 <+> "!" <+> prettyAttr t
          body = indent lengthT1 $ vsep (map (("!" <+>) . prettyAttr) ts) <+> dollar
      in vsep [header, body, footer]
 where
  prettyT1 = prettyElem t1
  lengthT1 = succ . T.length . renderStrict $ layoutPretty defaultLayoutOptions prettyT1
  dollar = if length as > 1 then "$ do" else "$"
  footer = case as of
    [] -> indent 2 "mempty"
    _ -> vsep $ map (indent 2 . prettyBlock) as

prettyElem :: Syntax.Elem -> Doc ann
prettyElem = \case
  Syntax.Html -> "H.html"
  Syntax.Body -> "H.body"
  Syntax.Div -> "H.div"

prettyAttr :: Syntax.Attr -> Doc ann
prettyAttr (Syntax.Id t) = pretty $ "A.id (H.toValue \"" <> t <> "\")"
prettyAttr (Syntax.Class t) = pretty $ "A.class_ (H.toValue \"" <> t <> "\")"
