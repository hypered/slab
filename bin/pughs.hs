{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (indentBlock, nonIndented, IndentOpt(..))
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  pugContent <- T.readFile "example.pug"
  let parsedHtml = parse pugContent
  either T.putStrLn putStrLn $ fmap renderHtmls parsedHtml

parse :: Text -> Either Text [H.Html]
parse input =
  either (Left . T.pack . errorBundlePretty) (Right . pugNodesToHtml) $ parsePug input

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
      header <- pugClass
      return (L.IndentMany Nothing (return . PugDiv header) pugElement)

pugClass :: Parser String
pugClass = lexeme (char '.' *> some (alphaNumChar <|> char '-')) <?> "class name"

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--------------------------------------------------------------------------------
renderHtmls :: [Html] -> String
renderHtmls = concat . map renderHtml

pugNodesToHtml :: [PugNode] -> [H.Html]
pugNodesToHtml = map pugNodeToHtml

pugNodeToHtml :: PugNode -> H.Html
pugNodeToHtml (PugDiv className children) =
  H.div ! A.class_ (H.toValue className) $ mconcat $ map pugNodeToHtml children
