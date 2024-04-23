module Pughs.Parse where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

data PugNode
  = PugElem Elem [Attr] [PugNode]
  | PugText Pipe Text
  deriving (Show, Eq)

data Elem
  = Html
  | Body
  | Div
  | Span
  | H1
  | A
  | P
  deriving (Show, Eq)

data Attr = AttrList [(Text, Text)] | Class Text
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data Pipe
  = Normal -- ^ The text follows an element on the same line.
  | Pipe -- ^ The text follows a pipe character.
  deriving (Show, Eq)

parsePug :: Text -> Either (ParseErrorBundle Text Void) [PugNode]
parsePug = runParser (many pugElement <* eof) ""

pugElement :: Parser PugNode
pugElement = L.indentBlock scn (p <|> p')
  where
    p = do
      header <- pugDiv
      mcontent <- optional pugText
      case mcontent of
        Nothing ->
          pure (L.IndentMany Nothing (return . header) pugElement)
        Just content ->
          pure $ L.IndentNone $ header [PugText Normal content]
    p' = do
      _ <- lexeme $ string "|"
      mcontent <- optional pugText
      pure $ L.IndentNone $ PugText Pipe $ maybe "" id mcontent

-- E.g. div, div.a, .a
pugDiv :: Parser ([PugNode] -> PugNode)
pugDiv =
  pugElemWithAttrs <|> pugAttrs

-- E.g. div, div.a, div()
pugElemWithAttrs :: Parser ([PugNode] -> PugNode)
pugElemWithAttrs = do
  (name, attrs) <- lexeme
    ( do
        a <- pugElem
        b <- many (pugClass <|> pugAttrList)
        pure (a, b)
    ) <?> "div tag"
  pure $ PugElem name attrs

pugElem :: Parser Elem
pugElem = choice
  [ string "html" *> pure Html
  , string "body" *> pure Body
  , string "div" *> pure Div
  , string "span" *> pure Span
  , string "h1" *> pure H1
  , string "a" *> pure A
  , string "p" *> pure P
  ]

-- E.g. .a, ()
pugAttrs :: Parser ([PugNode] -> PugNode)
pugAttrs = do
  attrs <- lexeme (some (pugClass <|> pugAttrList)) <?> "attributes"
  pure $ PugElem Div attrs

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

pugPair :: Parser (Text, Text)
pugPair = do
  a <- T.pack <$> (some (noneOf (",()= \n" :: String))) <?> "key"
  _ <- string "="
  b <- lexeme pugString
  _ <- optional (lexeme $ string ",")
  pure (a, b)

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
