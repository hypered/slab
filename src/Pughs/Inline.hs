-- Text interpolation modeled on the @template@ library by Johan Tibell.
-- - This uses Megaparsec instead of an internal State monad parser.
-- - This uses @#@ instead of @$@.
-- - Only the safe parsers are provided.
-- - Only the applicative interface is provided.
-- TODO Mention the BSD-3 license and Johan.

module Pughs.Inline
  ( -- * The @Template@ type
    Template (..)
  , Inline (..)

    -- * The @Context@ type
  , Context

    -- * Basic interface
  , parse
  , parseInlines

    -- * Applicative interface
  , render
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Prelude hiding (takeWhile)

--------------------------------------------------------------------------------

-- | A representation of a 'Data.Text' template, supporting efficient
-- rendering. Use 'parse' to create a template from a text containing
-- placeholders.
newtype Template = Template [Inline]
  deriving (Eq, Show)

-- | A template fragment.
data Inline = Lit {-# UNPACK #-} !Text | Var {-# UNPACK #-} !Text !Bool
  deriving (Eq, Show)

-- | A mapping from placeholders in the template to values with an applicative
-- lookup function. For instance the lookup function can fail, returning
-- 'Nothing' or 'Left'.
type Context f = Text -> f Text

--------------------------------------------------------------------------------
-- Basic interface

-- | Create a template from a template string. A malformed template
-- string will cause 'parse' to return a parse error.
parse :: Text -> Either (M.ParseErrorBundle Text Void) Template
parse =
  either Left (Right . Template) . M.parse (parseInlines <* M.eof) "-"

combineLits :: [Inline] -> [Inline]
combineLits [] = []
combineLits xs =
  let (lits, xs') = span isLit xs
   in case lits of
        [] -> gatherVars xs'
        [lit] -> lit : gatherVars xs'
        _ -> Lit (T.concat (map fromLit lits)) : gatherVars xs'
 where
  gatherVars [] = []
  gatherVars ys =
    let (vars, ys') = span isVar ys
     in vars <> combineLits ys'

  isLit (Lit _) = True
  isLit _ = False

  isVar = not . isLit

  fromLit (Lit v) = v
  fromLit _ = undefined

-- | Perform the template substitution, returning a new Text. The lookup can
-- have side effects.  The lookups are performed in order that they are needed
-- to generate the resulting text.
--
-- You can use this e.g. to report errors when a lookup cannot be made
-- successfully.  For example, given a list @context@ of key-value pairs
-- and a 'Template' @template@:
--
-- > render template (flip lookup context)
--
-- will return 'Nothing' if any of the placeholders in the template
-- don't appear in @ctx@ and @Just text@ otherwise.
render :: Applicative f => Template -> Context f -> f TL.Text
render (Template frags) context = TL.fromChunks <$> traverse renderInline frags
 where
  renderInline (Lit s) = pure s
  renderInline (Var x _) = context x

--------------------------------------------------------------------------------
-- Template parser

type Parser = M.Parsec Void Text

parseInlines :: Parser [Inline]
parseInlines = combineLits <$> M.many parseInline

parseInline :: Parser Inline
parseInline =
  M.choice
    [ parseLit
    , parseLitEntity
    , parseEscape
    , parseBracketVar
    , parseVar
    ]

-- TODO The \n condition could be optional if we want this module to be useful
-- outside Pughs.
parseLit :: Parser Inline
parseLit = do
  s <- M.takeWhile1P (Just "literal") (\c -> c /= '#' && c /= '\n' && c /= '&')
  pure $ Lit s

-- Allow # to appear for character entities using numeric references.
parseLitEntity :: Parser Inline
parseLitEntity = do
  _ <- C.string $ T.pack "&#"
  n <- M.some C.digitChar
  _ <- C.string $ T.pack ";"
  pure $ Lit $ T.pack $ "&#" <> n <> ";"

parseEscape :: Parser Inline
parseEscape = do
  _ <- C.string $ T.pack "##"
  pure $ Lit $ T.pack "#"

parseBracketVar :: Parser Inline
parseBracketVar = do
  _ <- C.string $ T.pack "#{"
  name <- parseIdentifier
  _ <- C.string $ T.pack "}"
  pure $ Var name True

parseVar :: Parser Inline
parseVar = do
  _ <- C.string $ T.pack "#"
  name <- parseIdentifier
  pure $ Var name False

parseIdentifier :: Parser Text
parseIdentifier = do
  a <- C.letterChar
  as <- M.many (C.alphaNumChar <|> M.oneOf ("-_" :: String))
  pure $ T.pack (a : as)

-- Example usage.
{-
main :: IO ()
main = do
  let mtemplate = parse $ T.pack "Hello #{name} #name ##"
      context =
        maybe (Left "Key not found") Right
        . flip lookup [(T.pack "name", T.pack "Alice")]
  case mtemplate of
    Right template -> case render template context of
      Right rendered -> TL.putStrLn rendered
      Left err -> print err
    Left err -> print err
-}
