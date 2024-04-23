module Pughs.Parse where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

data PugNode = PugDiv String [PugNode]
  deriving (Show, Eq)

parsePug :: Text -> Either (ParseErrorBundle Text Void) [PugNode]
parsePug = runParser (many pugElement <* eof) ""

pugElement :: Parser PugNode
pugElement = L.indentBlock scn p
  where
    p = do
      header <- pugDiv
      return (L.IndentMany Nothing (return . header) pugElement)

pugDiv :: Parser ([PugNode] -> PugNode)
pugDiv = do
  pugDiv' <|> (PugDiv <$> pugClass)

pugDiv' :: Parser ([PugNode] -> PugNode)
pugDiv' = do
  _ <- lexeme (string "div") <?> "div tag"
  pure $ PugDiv ""

pugClass :: Parser String
pugClass = lexeme (char '.' *> some (alphaNumChar <|> char '-')) <?> "class name"

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
