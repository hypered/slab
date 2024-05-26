{-# LANGUAGE RecordWildCards #-}

module Pughs.Parse
  ( parsePugFile
  , parsePug
  , parseErrorPretty
  ) where

import Control.Monad (void)
import Data.List.NonEmpty qualified as NE (toList)
import Data.Maybe (isJust)
import Data.Set qualified as S (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Pughs.Syntax
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
parsePugFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [PugNode])
parsePugFile path = do
  pugContent <- T.readFile path
  pure $ parsePug path pugContent

parsePug :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [PugNode]
parsePug fn = runParser (many (pugNode WithinDef) <* eof) fn

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

pugNode :: What -> Parser PugNode
pugNode what =
  L.indentBlock scn $
    choice
      [ pugDoctype
      , try pugInclude
      , pugElement what
      , pugPipe
      , pugCode'
      , pugMixinDef what
      , pugMixinCall
      , pugFragmentDef what
      , pugComment
      , pugFilter
      , pugRawElement what
      , pugBlock what
      , pugExtends
      , pugEach what
      , pugFragmentCall what
      ]

pugElement :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugElement what = do
  ref <- L.indentLevel
  header <- pugDiv
  case trailingSym $ header [] of
    HasDot -> do
      mcontent <- optional pugText
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [PugText Dot content]
        Nothing -> do
          scn
          items <- textBlock ref pugText
          let items' = realign items
          pure $ L.IndentNone $ header [PugText Dot $ T.intercalate "\n" items']
    HasEqual -> do
      mcontent <- optional pugCode
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [PugCode content]
        Nothing -> do
          scn
          content <- pugCode
          pure $ L.IndentNone $ header [PugCode content]
    NoSym -> do
      mcontent <- optional pugText
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [PugText Normal content]
        Nothing -> pure $ L.IndentMany Nothing (pure . header) (pugNode what)

-- | Parse lines of text, indented more than `ref`.
-- E.g.:
--     a
--       b
--     c
-- will return ["a", "  b", "c"].
textBlock :: Pos -> Parser Text -> Parser [Text]
textBlock ref p = go
 where
  go = do
    scn
    pos <- L.indentLevel
    done <- isJust <$> optional eof
    if done
      then return []
      else
        if pos <= ref
          then return []
          else ((:) . (T.replicate (unPos pos - unPos ref) " " <>)) <$> p <*> go

-- | Considering all the given lign as a block, strip the leading whitespace of
-- each ligne so that the left-most character of the block is in the first
-- column.
realign :: [Text] -> [Text]
realign xs = map (T.drop n) xs
 where
  n = minimum $ map (T.length . T.takeWhile (== ' ')) xs

pugPipe :: Parser (L.IndentOpt Parser PugNode PugNode)
pugPipe = do
  _ <- lexeme $ string "|"
  mcontent <- optional pugText
  pure $ L.IndentNone $ PugText Pipe $ maybe "" id mcontent

pugCode' :: Parser (L.IndentOpt Parser PugNode PugNode)
pugCode' = do
  _ <- lexeme $ string "="
  content <- pugCode
  pure $ L.IndentNone $ PugCode content

pugCode :: Parser Code
pugCode = do
  (SingleQuoteString <$> pugSingleQuoteString) -- TODO Escape HTML, e.g. < to &lt;.
  <|> (Int <$> pugInt)
  <|> (Variable <$> pugName)

--------------------------------------------------------------------------------
pugDoctype :: Parser (L.IndentOpt Parser PugNode PugNode)
pugDoctype = do
  _ <- lexeme (string "doctype")
  _ <- lexeme (string "html")
  pure $ L.IndentNone PugDoctype

--------------------------------------------------------------------------------
-- E.g. div, div.a, .a
pugDiv :: Parser ([PugNode] -> PugNode)
pugDiv =
  pugElemWithAttrs <|> pugAttrs

-- E.g. div, div.a, div()
pugElemWithAttrs :: Parser ([PugNode] -> PugNode)
pugElemWithAttrs = do
  (name, attrs, mdot) <-
    lexeme
      ( do
          a <- pugElem
          -- `try` because we want to backtrack if there is a dot
          -- not followed by a class name, for mdot to succeed.
          b <- many (pugId <|> try pugClass <|> pugAttrList)
          mtrailing <- optional $ choice
            [ string "." >> pure HasDot
            , string "=" >> pure HasEqual
            ]
          pure (a, b, maybe NoSym id mtrailing)
      )
      <?> "div tag"
  pure $ PugElem name mdot attrs

pugElem :: Parser Elem
pugElem =
  ( try $ do
      name <- T.pack <$> (some (alphaNumChar <|> oneOf ("-_" :: String))) <?> "identifier"
      case name of
        "html" -> pure Html
        "body" -> pure Body
        "div" -> pure Div
        "span" -> pure Span
        "hr" -> pure Hr
        "h1" -> pure H1
        "h2" -> pure H2
        "h3" -> pure H3
        "h4" -> pure H4
        "h5" -> pure H5
        "h6" -> pure H6
        "header" -> pure Header
        "head" -> pure Head
        "meta" -> pure Meta
        "main" -> pure Main
        "audio" -> pure Audio
        "a" -> pure A
        "code" -> pure Code
        "img" -> pure Img
        "iframe" -> pure IFrame
        "input" -> pure Input
        "i" -> pure I
        "pre" -> pure Pre
        "p" -> pure P
        "ul" -> pure Ul
        "link" -> pure Link
        "li" -> pure Li
        "title" -> pure Title
        "table" -> pure Table
        "thead" -> pure Thead
        "tbody" -> pure Tbody
        "tr" -> pure Tr
        "td" -> pure Td
        "dl" -> pure Dl
        "dt" -> pure Dt
        "dd" -> pure Dd
        "footer" -> pure Footer
        "figure" -> pure Figure
        "form" -> pure Form
        "label" -> pure Label
        "blockquote" -> pure Blockquote
        "button" -> pure Button
        "figcaption" -> pure Figcaption
        "script" -> pure Script
        "style" -> pure Style
        "small" -> pure Small
        "source" -> pure Source
        "svg" -> pure Svg
        _ -> fail "invalid element name"
  )
  <?> "element name"

-- E.g. .a, ()
pugAttrs :: Parser ([PugNode] -> PugNode)
pugAttrs = do
  (attrs, mdot) <-
    lexeme
      ( do
          attrs <- some (pugId <|> pugClass <|> pugAttrList)
          mdot <- optional (string ".")
          pure (attrs, maybe NoSym (const HasDot) mdot)
      )
      <?> "attributes"
  pure $ PugElem Div mdot attrs

-- E.g. #a
pugId :: Parser Attr
pugId =
  Id . T.pack
    <$> (char '#' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "id"

-- E.g. .a
pugClass :: Parser Attr
pugClass =
  Class . T.pack
    <$> (char '.' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "class name"

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
    b <- lexeme (pugSingleQuoteString <|> pugDoubleQuoteString <|> pugNumber)
    pure b
  _ <- optional (lexeme $ string ",")
  pure (a, mb)

pugSingleQuoteString :: Parser Text
pugSingleQuoteString = do
  _ <- string "'"
  s <- T.pack <$> (some (noneOf ("'\n" :: String))) <?> "string"
  _ <- string "'"
  pure s

pugDoubleQuoteString :: Parser Text
pugDoubleQuoteString = do
  _ <- string "\""
  s <- T.pack <$> (some (noneOf ("\"\n" :: String))) <?> "string"
  _ <- string "\""
  pure s

-- TODO Proper data type.
pugNumber :: Parser Text
pugNumber = do
  s <- T.pack <$> (some digitChar) <?> "string"
  pure s

pugText :: Parser Text
pugText = T.pack <$> lexeme (some (noneOf ['\n'])) <?> "text content"

pugIdentifier :: Parser Text
pugIdentifier = T.pack <$> lexeme (some (noneOf (" \n" :: String))) <?> "identifier"

--------------------------------------------------------------------------------
pugInclude :: Parser (L.IndentOpt Parser PugNode PugNode)
pugInclude = do
  _ <- lexeme (string "include")
  path <- pugPath
  pure $ L.IndentNone $ PugInclude path Nothing

pugPath :: Parser FilePath
pugPath = lexeme (some (noneOf ['\n'])) <?> "path"

--------------------------------------------------------------------------------
pugMixinDef :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugMixinDef what = do
  _ <- lexeme (string "mixin")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugMixinDef name) (pugNode what)

--------------------------------------------------------------------------------
pugMixinCall :: Parser (L.IndentOpt Parser PugNode PugNode)
pugMixinCall = do
  name <- lexeme (char '+') *> pugText
  pure $ L.IndentNone $ PugMixinCall name Nothing

--------------------------------------------------------------------------------
pugFragmentDef :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugFragmentDef _ = do
  _ <- lexeme (string "fragment" <|> string "frag")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugFragmentDef name) (pugNode WithinDef)

--------------------------------------------------------------------------------
pugFragmentCall :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugFragmentCall _ = do
  name <- pugIdentifier
  pure $ L.IndentMany Nothing (pure . PugFragmentCall name) (pugNode WithinCall)

--------------------------------------------------------------------------------
pugEach :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugEach what = do
  _ <- lexeme (string "each")
  name <- lexeme pugName
  mindex <- optional $ do
    _ <- lexeme $ string ","
    lexeme pugName
  _ <- lexeme (string "in")
  collection <- (CollectionList <$> pugList) <|> (CollectionObject <$> pugObject)
  pure $ L.IndentMany Nothing (pure . PugEach name mindex collection) (pugNode what)

pugList :: Parser [Code]
pugList = do
  _ <- lexeme "["
  mx <- optional $ lexeme pugName
  xs <- case mx of
    Nothing -> pure []
    Just x -> do
      xs <- many $ do
        _ <- lexeme (string ",")
        lexeme pugName
      pure $ x : xs
  _ <- lexeme "]"
  pure $ map SingleQuoteString xs

pugObject :: Parser [(Code, Code)]
pugObject = do
  _ <- lexeme (string "{")
  mkv <- optional $ do
    key <- lexeme pugCode
    _ <- lexeme (string ":")
    val <- lexeme pugCode
    pure (key, val)
  kvs <- case mkv of
    Nothing -> pure []
    Just kv -> do
      kvs <- many $ do
        _ <- lexeme $ string ","
        key <- lexeme pugCode
        _ <- lexeme (string ":")
        val <- lexeme pugCode
        pure (key, val)
      pure $ kv : kvs
  _ <- lexeme (string "}")
  pure kvs

--------------------------------------------------------------------------------
pugComment :: Parser (L.IndentOpt Parser PugNode PugNode)
pugComment = do
  ref <- L.indentLevel
  b <- lexeme $
    choice
      [ string "//-" *> pure False
      , string "//" *> pure True
      ]
  mcontent <- optional pugText
  case mcontent of
    Just content -> pure $ L.IndentNone $ PugComment b content
    Nothing -> do
      scn
      items <- textBlock ref pugText
      let items' = realign items
      pure $ L.IndentNone $ PugComment b $ T.intercalate "\n" items'

--------------------------------------------------------------------------------
pugFilter :: Parser (L.IndentOpt Parser PugNode PugNode)
pugFilter = do
  ref <- L.indentLevel
  name <-
    lexeme (
        string ":" *>
        pugName
      ) <?> "filter name"
  mcontent <- optional pugText
  case mcontent of
    Just content -> pure $ L.IndentNone $ PugFilter name content
    Nothing -> do
      scn
      items <- textBlock ref pugText
      let items' = realign items
      pure $ L.IndentNone $ PugFilter name $ T.intercalate "\n" items'

pugInt :: Parser Int
pugInt = L.decimal

pugName :: Parser Text
pugName = T.pack <$> some (alphaNumChar <|> oneOf ("-_" :: String)) <?> "name"

--------------------------------------------------------------------------------
pugRawElement :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugRawElement what = do
  header <- pugAngleBracket
  pure $ L.IndentMany Nothing (pure . header) (pugNode what)

pugAngleBracket :: Parser ([PugNode] -> PugNode)
pugAngleBracket = do
  _ <- char '<'
  content <- pugText
  pure $ PugRawElem $ "<" <> content

--------------------------------------------------------------------------------
pugBlock :: What -> Parser (L.IndentOpt Parser PugNode PugNode)
pugBlock what = do
  _ <- lexeme (string "block")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugBlock what name) (pugNode what)

--------------------------------------------------------------------------------
pugExtends :: Parser (L.IndentOpt Parser PugNode PugNode)
pugExtends = do
  _ <- lexeme (string "extends")
  path <- pugPath
  pure $ L.IndentNone $ PugExtends path Nothing

--------------------------------------------------------------------------------
scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

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
