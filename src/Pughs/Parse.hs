module Pughs.Parse where

import Control.Monad (void)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
data PugNode
  = PugElem Elem TrailingDot [Attr] [PugNode]
  | PugText Pipe Text
  deriving (Show, Eq)

hasTrailingDot :: PugNode -> Bool
hasTrailingDot (PugElem _ HasDot _ _) = True
hasTrailingDot _ = False

data Elem
  = Html
  | Body
  | Div
  | Span
  | H1
  | A
  | P
  | Ul
  | Li
  | Figure
  | Blockquote
  | Figcaption
  | Audio
  | Source
  | Pre
  | Code
  deriving (Show, Eq)

data TrailingDot = HasDot | NoDot
  deriving (Show, Eq)

data Attr = AttrList [(Text, Maybe Text)] | Class Text
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data Pipe
  = Normal -- ^ The text follows an element on the same line.
  | Pipe -- ^ The text follows a pipe character.
  | Dot -- ^ The text is part of a text block following a trailing dot.
  deriving (Show, Eq)

extractClasses :: [PugNode] -> [Text]
extractClasses = nub . sort . concatMap f
 where
  f (PugElem _ _ attrs children) = concatMap g attrs <> extractClasses children
  f (PugText _ _) = []
  g (AttrList xs) = concatMap h xs
  g (Class c) = [c]
  h ("class", Just c) = [c]
  h _ = []

--------------------------------------------------------------------------------
parsePug :: Text -> Either (ParseErrorBundle Text Void) [PugNode]
parsePug = runParser (many pugElement <* eof) ""

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

pugElement :: Parser PugNode
pugElement = L.indentBlock scn (p <|> p')
  where
    p = do
      header <- pugDiv
      mcontent <- optional pugText
      case mcontent of
        Nothing ->
          if hasTrailingDot $ header []
          then
            pure $ L.IndentMany Nothing (pure . header) pugTexts
          else
            pure $ L.IndentMany Nothing (pure . header) pugElement
        Just content ->
          pure $ L.IndentNone $ header [PugText Normal content]
    p' = do
      _ <- lexeme $ string "|"
      mcontent <- optional pugText
      pure $ L.IndentNone $ PugText Pipe $ maybe "" id mcontent

pugTexts :: Parser PugNode
pugTexts = PugText Dot <$> pugText

-- E.g. div, div.a, .a
pugDiv :: Parser ([PugNode] -> PugNode)
pugDiv =
  pugElemWithAttrs <|> pugAttrs

-- E.g. div, div.a, div()
pugElemWithAttrs :: Parser ([PugNode] -> PugNode)
pugElemWithAttrs = do
  (name, attrs, mdot) <- lexeme
    ( do
        a <- pugElem
        -- `try` because we want to backtrack if there is a dot
        -- not followed by a class name, for mdot to succeed.
        b <- many (try pugClass <|> pugAttrList)
        mdot <- optional (string ".")
        pure (a, b, maybe NoDot (const HasDot) mdot)
    ) <?> "div tag"
  pure $ PugElem name mdot attrs

pugElem :: Parser Elem
pugElem = choice
  [ string "html" *> pure Html
  , string "body" *> pure Body
  , string "div" *> pure Div
  , string "span" *> pure Span
  , string "h1" *> pure H1
  , string "audio" *> pure Audio
  , string "a" *> pure A
  , string "code" *> pure Code
  , string "pre" *> pure Pre
  , string "p" *> pure P
  , string "ul" *> pure Ul
  , string "li" *> pure Li
  , string "figure" *> pure Figure
  , string "blockquote" *> pure Blockquote
  , string "figcaption" *> pure Figcaption
  , string "source" *> pure Source
  ]

-- E.g. .a, ()
pugAttrs :: Parser ([PugNode] -> PugNode)
pugAttrs = do
  (attrs, mdot) <- lexeme
    ( do
        attrs <- some (pugClass <|> pugAttrList)
        mdot <- optional (string ".")
        pure (attrs, maybe NoDot (const HasDot) mdot)
    ) <?> "attributes"
  pure $ PugElem Div mdot attrs

-- E.g. .a
pugClass :: Parser Attr
pugClass = Class . T.pack <$>
  (char '.' *> some (alphaNumChar <|> char '-')) <?> "class name"

-- E.g. (), (class='a')
pugAttrList :: Parser Attr
pugAttrList = (<?> "attribute") $ do
  _ <- string "("
  pairs <- many pugPair
  _ <- string ")"
  pure $ AttrList pairs

pugPair :: Parser (Text, Maybe Text)
pugPair = do
  a <- T.pack <$> (some (noneOf (",()= \n" :: String))) <?> "key"
  mb <- optional $ do
    _ <- string "="
    b <- lexeme pugString
    pure b
  _ <- optional (lexeme $ string ",")
  pure (a, mb)

pugString :: Parser Text
pugString = do
  _ <- string "'"
  s <- T.pack <$> (some (noneOf ("'\n" :: String))) <?> "string"
  _ <- string "'"
  pure s

pugText :: Parser Text
pugText = T.pack <$> lexeme (some (noneOf ['\n'])) <?> "text content"

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
