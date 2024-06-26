-- |
-- Module      : Slab.Error
-- Description : Errors that the different Slab stages can produce
--
-- @Slab.Error@ provides a data type to represent all the errors emitted by
-- Slab. It also provides helper functions to report errors in a human-readable
-- way.
module Slab.Error
  ( Error (..)
  , unwrap
  , display
  ) where

import Data.List.NonEmpty qualified as NE (toList)
import Data.Set qualified as S (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)
import Text.Megaparsec qualified as M
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
  Left err -> do
    display err
    exitFailure
  Right a -> pure a

display :: Error -> IO ()
display = \case
  ParseError err ->
    -- Our custom function seems actually worse than errorBundlePretty.
    -- T.putStrLn . parseErrorPretty $ err
    T.putStrLn . T.pack $ errorBundlePretty err
  EvaluateError err ->
    T.putStrLn $ "Error during evaluation: " <> err
  err ->
    TL.putStrLn $ pShowNoColor err

--------------------------------------------------------------------------------
-- Convert parse errors to a user-friendly message.
parseErrorPretty :: ParseErrorBundle Text Void -> Text
parseErrorPretty (ParseErrorBundle errors posState) =
  case NE.toList errors of
    [] -> "Unknown error"
    (e : _) -> case e of
      TrivialError offset unexpected expected ->
        let
          pos = pstateSourcePos $ reachOffsetNoLine offset posState
          errorPos =
            T.pack (show (unPos (sourceLine pos)))
              <> ":"
              <> T.pack (show (unPos (sourceColumn pos)))
          unexpectedMsg =
            maybe
              "Unexpected end of input."
              (\u -> "Unexpected " <> errorItemPretty u <> ".")
              unexpected
          expectedMsg =
            if null expected
              then ""
              else "Expected " <> (T.intercalate ", " . map errorItemPretty . S.toList $ expected) <> "."
         in
          T.unwords
            [ "Error at"
            , errorPos
            , "-"
            , unexpectedMsg
            , if T.null expectedMsg then "." else expectedMsg
            ]
      FancyError offset err -> "Complex error at position " <> T.pack (show offset) <> ": " <> TL.toStrict (pShowNoColor err)

errorItemPretty :: ErrorItem Char -> Text
errorItemPretty = \case
  Tokens ts -> "character '" <> T.pack (NE.toList ts) <> "'"
  M.Label label -> T.pack (NE.toList label)
  EndOfInput -> "end of input"
