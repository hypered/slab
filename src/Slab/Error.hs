module Slab.Error
  ( PreProcessError (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

--------------------------------------------------------------------------------

-- | Represent all errors emitted by Slab. I.e. this is used in each stage
-- (`Slab.PreProcess`, `Slab.Evaluate`).
data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)
