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

data PugNode = PugDiv [Text] [PugNode]
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
pugDiv =
  pugDivWithClasses <|> pugClasses

pugDivWithClasses :: Parser ([PugNode] -> PugNode)
pugDivWithClasses = do
  classes <- lexeme (string "div" *> many pugClass) <?> "div tag"
  pure $ PugDiv classes

pugClasses :: Parser ([PugNode] -> PugNode)
pugClasses = do
  classes <- lexeme (some pugClass)
  pure $ PugDiv classes

pugClass :: Parser Text
pugClass = T.pack <$> (char '.' *> some (alphaNumChar <|> char '-')) <?> "class name"

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
