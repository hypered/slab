-- |
-- Module      : Slab.Generate.Haskell
-- Description : Translate from the Slab syntax to Haskell
--
-- @Slab.Generate.Haskell@ is an attempt to translate Slab to Haskell. This
-- could make it possible to use Slab within Haskell projects without needing
-- to interpret Slab templates at runtime. If this proves useful, other
-- languages could also be supported.
module Slab.Generate.Haskell
  ( renderHs
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import Slab.Error qualified as Error
import Slab.PreProcess qualified as PreProcess
import Slab.Syntax qualified as Syntax

--------------------------------------------------------------------------------
renderHs :: FilePath -> IO ()
renderHs path = do
  blocks <- PreProcess.preprocessFile path >>= Error.unwrap
  T.putStrLn $ renderModule blocks

renderModule :: [Syntax.Block] -> Text
renderModule = renderStrict . layoutPretty defaultLayoutOptions . prettyModule

renderBlocks :: [Syntax.Block] -> Text
renderBlocks = renderStrict . layoutPretty defaultLayoutOptions . prettyBlocks

--------------------------------------------------------------------------------
prettyModule :: [Syntax.Block] -> Doc ann
prettyModule blocks =
  vsep
    [ moduleHeader
    , indent 2 $ prettyBlocks blocks
    ]

moduleHeader :: Doc ann
moduleHeader =
  vsep
    [ "module Main where"
    , mempty
    , "import Data.Text (Text)"
    , "import Text.Blaze.Html5 (Html, (!))"
    , "import Text.Blaze.Html5 qualified as H"
    , "import Text.Blaze.Html5.Attributes qualified as A"
    , "import Text.Blaze.Html.Renderer.Pretty (renderHtml)"
    , mempty
    , "main :: IO ()"
    , "main = putStrLn . renderHtml $"
    ]

--------------------------------------------------------------------------------
prettyBlocks :: [Syntax.Block] -> Doc ann
prettyBlocks = vsep . map prettyBlock

prettyBlock :: Syntax.Block -> Doc ann
prettyBlock (Syntax.BlockElem name _ attrs children) = prettyBlockElem (name, attrs', children)
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
prettyBlockElem :: (Syntax.Elem, [Syntax.Attr], [Syntax.Block]) -> Doc ann
prettyBlockElem (t1, ts_, as) =
  case ts_ of
    [] ->
      let header = prettyT1 <+> dollar
       in vsep [header, footer]
    [t] ->
      let header = prettyT1 <+> "!" <+> prettyAttr t <+> dollar
       in vsep [header, footer]
    t : ts ->
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
prettyAttr (Syntax.Id t) = pretty $ "A.id (H.toValue (\"" <> t <> "\" :: Text))"
prettyAttr (Syntax.Class t) = pretty $ "A.class_ (H.toValue (\"" <> t <> "\" :: Text))"
prettyAttr (Syntax.Attr a b) =
  "H.customAttribute" <+> pretty a <+> prettyExpr b

prettyExpr :: Syntax.Expr -> Doc ann
prettyExpr _ = "TODO"
