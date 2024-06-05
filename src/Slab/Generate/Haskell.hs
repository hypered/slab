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
  attrs' = elemId <> elemClass

  idNames = Syntax.idNamesFromAttrs attrs
  idNames' :: Text
  idNames' = T.intercalate " " idNames
  elemId =
    if idNames == []
    then []
    else [Id' idNames']

  classNames = Syntax.classNamesFromAttrs attrs
  classNames' :: Text
  classNames' = T.intercalate " " classNames
  elemClass =
    if classNames == []
    then []
    else [Class' classNames']

-- | Render an element, aligning the @!@ character:
--
-- @
--   elem ! a
--        ! b $ do
--     child0
--     child1
-- @
prettyPugElem :: (Syntax.Elem, [Attr'], [Syntax.Block]) -> Doc ann
prettyPugElem (t1, [], as) =
  let dollar = if length as > 1 then "$ do" else "$"
      header = prettyElem t1 <+> dollar
      footer = case as of
        [] -> indent 2 "mempty"
        _ -> vsep $ map (indent 2 . prettyBlock) as
  in vsep [header, footer]
prettyPugElem (t1, [t], as) =
  let dollar = if length as > 1 then "$ do" else "$"
      header = prettyElem t1 <+> "!" <+> prettyAttr t <+> dollar
      footer = case as of
        [] -> indent 2 "mempty"
        _ -> vsep $ map (indent 2 . prettyBlock) as
  in vsep [header, footer]
prettyPugElem (t1, t:ts, as) =
  let e = prettyElem t1
      n = succ . T.length . renderStrict $ layoutPretty defaultLayoutOptions e
      dollar = if length as > 1 then "$ do" else "$"
      header = e <+> "!" <+> prettyAttr t
      body = indent n $ vsep (map (("!" <+>) . prettyAttr) ts) <+> dollar
      footer = case as of
        [] -> indent 2 "mempty"
        _ -> vsep $ map (indent 2 . prettyBlock) as
  in vsep [header, body, footer]

prettyElem :: Syntax.Elem -> Doc ann
prettyElem = \case
  Syntax.Html -> "H.html"
  Syntax.Body -> "H.body"
  Syntax.Div -> "H.div"

-- A version of Attr where multiple classes are supposed to be grouped together.
data Attr' = Id' Text | Class' Text

prettyAttr :: Attr' -> Doc ann
prettyAttr (Id' t) = pretty $ "A.id (H.toValue \"" <> t <> "\")"
prettyAttr (Class' t) = pretty $ "A.class_ (H.toValue \"" <> t <> "\")"
