module Slab.Generate.Haskell
  ( renderHs
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Slab.Evaluate qualified as Evaluate
import Slab.Syntax qualified as Syntax

--------------------------------------------------------------------------------
renderHs :: FilePath -> IO ()
renderHs path = do
  preprocessed <- Evaluate.preprocessFile path
  case preprocessed of
    Left err -> print err
    Right blocks ->
      TL.putStrLn . TL.fromChunks . intersperse "\n" . blocksToHs 0 $ blocks

--------------------------------------------------------------------------------
blocksToHs :: Int -> [Syntax.Block] -> [Text]
blocksToHs indent = concatMap $ blockToHs indent

-- | Same as 'blocksToHs' but output @mempty@ instead of nothing when the list
-- of blocks is empty.
blocksToHs' :: Int -> [Syntax.Block] -> [Text]
blocksToHs' indent [] = [indent' <> "mempty"]
 where
  indent' = T.replicate (indent * 2) " "
blocksToHs' indent blocks = blocksToHs indent blocks

blockToHs :: Int -> Syntax.Block -> [Text]
blockToHs indent = \case
  Syntax.BlockDoctype -> [indent' <> "H.docType"]
  Syntax.PugElem name _ attrs children ->
    [indent' <> elemToHs name]
      <> if classNames == []
        then []
        else
          [indent' <> "  ! A.class_ (H.toValue \"" <> classNames' <> "\")"]
            <> [indent' <> "  $"]
            <> blocksToHs' (indent + 1) children
   where
    classNames = Syntax.classNamesFromAttrs attrs
    classNames' :: Text
    classNames' = T.intercalate " " classNames
 where
  indent' = T.replicate (indent * 2) " "

elemToHs :: Syntax.Elem -> Text
elemToHs = \case
  Syntax.Html -> "H.html"
  Syntax.Body -> "H.body"
  Syntax.Div -> "H.div"
