module Slab.Error
  ( Error (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

--------------------------------------------------------------------------------

-- | Represent all errors emitted by Slab. I.e. this is used in each stage
-- (`Slab.Parse`, `Slab.PreProcess`, `Slab.Evaluate`).
data Error
  = ParseError (ParseErrorBundle Text Void)
  | MiscError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)
