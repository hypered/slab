module Slab.Error
  ( Error (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

--------------------------------------------------------------------------------

-- | Represent all errors emitted by Slab. I.e. this is used in each stage
-- (`Slab.Parse`, `Slab.PreProcess`, `Slab.Evaluate`, `Slab.ExecuteError`).
data Error
  = ParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  | EvaluateError Text -- TODO Add specific variants instead of using Text.
  | ExecuteError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)
