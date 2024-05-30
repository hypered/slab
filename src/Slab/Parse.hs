{-# LANGUAGE RecordWildCards #-}

module Slab.Parse
  ( parsePugFile
  , parsePug
  , parseErrorPretty

    -- Inline parsing stuff

    -- * The @InterpolationContext@ type
  , InterpolationContext

    -- * Basic interface
  , parse
  , parseInlines
  ) where

import Control.Monad (void)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE (toList)
import Data.Maybe (isJust)
import Data.Set qualified as S (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import Slab.Syntax
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
parsePug fn = runParser (many pugNode <* eof) fn

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

pugNode :: Parser PugNode
pugNode = do
  node <-
    L.indentBlock scn $
      choice
        [ pugDoctype
        , try pugInclude
        , pugElement
        , pugPipe
        , pugCode'
        , pugMixinDef
        , pugMixinCall
        , pugFragmentDef
        , pugComment
        , pugFilter
        , pugRawElement
        , pugDefault
        , pugImport
        , (try pugReadJson <|> pugAssignVar)
        , pugEach
        , pugIf
        , pugFragmentCall
        ]
  case node of
    PugIf cond as _ -> do
      mbs <- optional $ L.indentBlock scn pugElse
      pure $ PugIf cond as $ maybe [] id mbs
    _ -> pure node

pugIf :: Parser (L.IndentOpt Parser PugNode PugNode)
pugIf = do
  _ <- lexeme $ string "if"
  cond <- pugCode
  pure $ L.IndentMany Nothing (pure . (\as -> PugIf cond as [])) pugNode

pugElse :: Parser (L.IndentOpt Parser [PugNode] PugNode)
pugElse = do
  _ <- lexeme $ string "else"
  pure $ L.IndentMany Nothing pure pugNode

pugElement :: Parser (L.IndentOpt Parser PugNode PugNode)
pugElement = do
  ref <- L.indentLevel
  header <- pugDiv
  case trailingSym $ header [] of
    HasDot -> do
      template <- parseInlines
      case template of
        [] -> do
          scn
          items <- textBlock ref pugText -- TODO Use parseInlines
          let items' = realign items
          pure $ L.IndentNone $ header [PugText Dot [Lit $ T.intercalate "\n" items']]
        _ -> pure $ L.IndentNone $ header [PugText Dot template]
    HasEqual -> do
      mcontent <- optional pugCode
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [PugCode content]
        Nothing -> do
          scn
          content <- pugCode
          pure $ L.IndentNone $ header [PugCode content]
    NoSym -> do
      template <- parseInlines
      case template of
        [] -> pure $ L.IndentMany Nothing (pure . header) pugNode
        _ -> pure $ L.IndentNone $ header [PugText Normal template]

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

-- | Considering all the given lines as a block, strip the leading whitespace of
-- each line so that the left-most character of the block is in the first
-- column.
realign :: [Text] -> [Text]
realign xs = map (T.drop n) xs
 where
  n = minimum $ map (T.length . T.takeWhile (== ' ')) xs

-- | Parse multiple lines starting each with a pipe prefix. Most of our parsers
-- parse a single line (and optional indented child lines) and most nodes map
-- to a single line, but here we want to be able to view such blocks as a
-- single node, and we make sur to add newlines between each when rendered.
pugPipe :: Parser (L.IndentOpt Parser PugNode PugNode)
pugPipe = do
  ref <- L.indentLevel
  template <- p
  templates <- go ref
  pure $ L.IndentNone $ PugText Pipe $ intercalate [Lit "\n"] (template : templates)
 where
  go ref = do
    scn
    pos <- L.indentLevel
    done <- isJust <$> optional eof
    if done
      then return []
      else do
        cont <- isJust <$> (lookAhead $ optional $ string "|")
        if pos /= ref || not cont
          then return []
          else (:) <$> p <*> go ref
  p = do
    _ <- lexeme $ string "|"
    template <- parseInlines
    pure template

pugCode' :: Parser (L.IndentOpt Parser PugNode PugNode)
pugCode' = do
  _ <- lexeme $ string "="
  content <- pugCode
  pure $ L.IndentNone $ PugCode content

pugCode :: Parser Code
pugCode =
  do
    try pugExpression
    <|> (SingleQuoteString <$> pugSingleQuoteString) -- TODO Escape HTML, e.g. < to &lt;.
    <|> (Int <$> pugInt)
    <|> ( do
            name <- pugName
            mkey <- optional $ do
              _ <- string "["
              key <- pugSingleQuoteString
              _ <- string "]"
              pure key
            case mkey of
              Nothing -> pure $ Variable name
              Just key -> pure $ Lookup name (SingleQuoteString key)
        )
    <|> (Object <$> pugObject)

pugVariable :: Parser Text
pugVariable = pugName

-- Hard-coded for now to parse something like
--   p= index + 1 + '.'
pugExpression :: Parser Code
pugExpression = do
  name <- lexeme pugVariable
  _ <- lexeme $ string "+"
  n <- lexeme $ pugNumber
  _ <- lexeme $ string "+"
  s <- lexeme $ pugSingleQuoteString
  pure $ Add (Add (Variable name) (Int n)) (SingleQuoteString s)

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
          mtrailing <-
            optional $
              choice
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
      name <-
        T.pack
          <$> ( do
                  a <- letterChar
                  as <- many (alphaNumChar <|> oneOf ("-_" :: String))
                  pure (a : as)
              )
          <?> "identifier"
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
        "textarea" -> pure Textarea
        "canvas" -> pure Canvas
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
  Id
    . T.pack
    <$> (char '#' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "id"

-- E.g. .a
pugClass :: Parser Attr
pugClass =
  Class
    . T.pack
    <$> (char '.' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "class name"

-- E.g. (), (class='a')
pugAttrList :: Parser Attr
pugAttrList = (<?> "attribute") $ do
  _ <- string "("
  pairs <- many pugPair
  _ <- string ")"
  pure $ AttrList pairs

pugPair :: Parser (Text, Maybe Code)
pugPair = do
  a <- T.pack <$> (some (noneOf (",()= \n" :: String))) <?> "key"
  mb <- optional $ do
    _ <- string "="
    b <- lexeme (SingleQuoteString <$> pugSingleQuoteString <|> SingleQuoteString <$> pugDoubleQuoteString <|> Int <$> pugNumber)
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
pugNumber :: Parser Int
pugNumber = L.decimal

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
pugMixinDef :: Parser (L.IndentOpt Parser PugNode PugNode)
pugMixinDef = do
  _ <- lexeme (string "mixin")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugMixinDef name) pugNode

--------------------------------------------------------------------------------
pugMixinCall :: Parser (L.IndentOpt Parser PugNode PugNode)
pugMixinCall = do
  name <- lexeme (char '+') *> pugText
  pure $ L.IndentNone $ PugMixinCall name Nothing

--------------------------------------------------------------------------------
pugFragmentDef :: Parser (L.IndentOpt Parser PugNode PugNode)
pugFragmentDef = do
  _ <- lexeme (string "fragment" <|> string "frag")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugFragmentDef name) pugNode

--------------------------------------------------------------------------------
pugFragmentCall :: Parser (L.IndentOpt Parser PugNode PugNode)
pugFragmentCall = do
  name <- pugIdentifier
  pure $ L.IndentMany Nothing (pure . PugFragmentCall name) pugNode

--------------------------------------------------------------------------------
pugEach :: Parser (L.IndentOpt Parser PugNode PugNode)
pugEach = do
  _ <- lexeme (string "each")
  name <- lexeme pugName
  mindex <- optional $ do
    _ <- lexeme $ string ","
    lexeme pugName
  _ <- lexeme (string "in")
  collection <-
    (List <$> pugList) <|> (Object <$> pugObject) <|> (Variable <$> pugVariable)
  pure $ L.IndentMany Nothing (pure . PugEach name mindex collection) pugNode

pugList :: Parser [Code]
pugList = do
  _ <- lexeme "["
  mx <- optional $ lexeme pugCode
  xs <- case mx of
    Nothing -> pure []
    Just x -> do
      xs <- many $ do
        _ <- lexeme (string ",")
        lexeme pugCode
      pure $ x : xs
  _ <- lexeme "]"
  pure xs

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
  b <-
    lexeme $
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
    lexeme
      ( string ":"
          *> pugName
      )
      <?> "filter name"
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
pugName =
  T.pack
    <$> ( do
            a <- letterChar
            as <- many (alphaNumChar <|> oneOf ("-_" :: String))
            pure (a : as)
        )
    <?> "name"

--------------------------------------------------------------------------------
pugRawElement :: Parser (L.IndentOpt Parser PugNode PugNode)
pugRawElement = do
  header <- pugAngleBracket
  pure $ L.IndentMany Nothing (pure . header) pugNode

pugAngleBracket :: Parser ([PugNode] -> PugNode)
pugAngleBracket = do
  _ <- char '<'
  content <- pugText
  pure $ PugRawElem $ "<" <> content

--------------------------------------------------------------------------------
pugDefault :: Parser (L.IndentOpt Parser PugNode PugNode)
pugDefault = do
  _ <- lexeme (string "default")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . PugDefault name) pugNode

--------------------------------------------------------------------------------
pugImport :: Parser (L.IndentOpt Parser PugNode PugNode)
pugImport = do
  _ <- lexeme (string "import")
  path <- pugPath
  pure $ L.IndentMany Nothing (pure . PugImport path Nothing) pugNode

--------------------------------------------------------------------------------
pugReadJson :: Parser (L.IndentOpt Parser PugNode PugNode)
pugReadJson = do
  _ <- lexeme (string "-")
  _ <- lexeme (string "var")
  name <- lexeme pugName
  _ <- lexeme (string "=")
  _ <- lexeme (string "require")
  _ <- lexeme (string "(")
  path <- T.unpack <$> lexeme pugDoubleQuoteString
  _ <- lexeme (string ")")
  pure $ L.IndentNone $ PugReadJson name path Nothing

pugAssignVar :: Parser (L.IndentOpt Parser PugNode PugNode)
pugAssignVar = do
  _ <- lexeme (string "-")
  _ <- lexeme (string "var")
  name <- lexeme pugName
  _ <- lexeme (string "=")
  val <- lexeme pugDoubleQuoteString
  pure $ L.IndentNone $ PugAssignVar name val

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

--------------------------------------------------------------------------------

-- Text interpolation stuff

-- Text interpolation modeled on the @template@ library by Johan Tibell.
-- - This uses Megaparsec instead of an internal State monad parser.
-- - This uses @#@ instead of @$@.
-- - Only the safe parsers are provided.
-- - Only the applicative interface is provided.
-- - We don't support #name, but only #{name}, because # can appear
--   in character entities, URLs, ...
-- TODO Mention the BSD-3 license and Johan.

--------------------------------------------------------------------------------

-- | A mapping from placeholders in the template to values with an applicative
-- lookup function. For instance the lookup function can fail, returning
-- 'Nothing' or 'Left'.
type InterpolationContext f = Text -> f Text

--------------------------------------------------------------------------------
-- Basic interface

-- | Create a template from a template string. A malformed template
-- string will cause 'parse' to return a parse error.
parse :: Text -> Either (M.ParseErrorBundle Text Void) [Inline]
parse = M.parse (parseInlines <* M.eof) "-"

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

--------------------------------------------------------------------------------
-- Template parser

parseInlines :: Parser [Inline]
parseInlines = combineLits <$> M.many parseInline

parseInline :: Parser Inline
parseInline =
  M.choice
    [ parseLit
    , parsePlace
    , parseEscape
    , parseSharpLit
    ]

-- TODO The \n condition could be optional if we want this module to be useful
-- outside Slab.
parseLit :: Parser Inline
parseLit = do
  s <- M.takeWhile1P (Just "literal") (\c -> c /= '#' && c /= '\n')
  pure $ Lit s

parsePlace :: Parser Inline
parsePlace = do
  _ <- string $ T.pack "#{"
  e <- pugCode
  _ <- string $ T.pack "}"
  pure $ Place e

parseEscape :: Parser Inline
parseEscape = do
  _ <- string $ T.pack "##"
  pure $ Lit $ T.pack "#"

parseSharpLit :: Parser Inline
parseSharpLit = do
  _ <- string $ T.pack "#"
  s <- M.takeWhile1P Nothing (\c -> c /= '#' && c /= '\n')
  pure $ Lit $ "#" <> s
