module Slab.Error
  ( Error (..)
  , unwrap
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import System.Exit (exitFailure)
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------

-- | Represent all errors emitted by Slab. I.e. this is used in each stage
-- (`Slab.Parse`, `Slab.PreProcess`, `Slab.Evaluate`, `Slab.ExecuteError`).
data Error
  = ParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  | EvaluateError Text -- TODO Add specific variants instead of using Text.
  | ExecuteError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

-- | Extract a Right value, or die, emitting an error message.
unwrap :: Either Error a -> IO a
unwrap = \case
  Left (ParseError err) -> do
    -- TODO Probably use this instead:
    -- T.putStrLn . Parse.parseErrorPretty $ err
    T.putStrLn . T.pack $ errorBundlePretty err
    exitFailure
  Left err -> do
    TL.putStrLn $ pShowNoColor err
    exitFailure
  Right a -> pure a
