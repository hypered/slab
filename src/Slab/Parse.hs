{-# LANGUAGE RecordWildCards #-}

module Slab.Parse
  ( parseFile
  , parseFileE
  , parse
  , parseExpr
  , parseErrorPretty
  , pugTextInclude
  -- Inline parsing stuff

    -- * The @InterpolationContext@ type
  , InterpolationContext

    -- * Basic interface
  , parse'
  , parseInlines
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Char (isSpace)
import Data.Functor (($>))
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
parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [Block])
parseFile path = do
  pugContent <- T.readFile path
  pure $ parse path pugContent

parseFileE :: FilePath -> ExceptT (ParseErrorBundle Text Void) IO [Block]
parseFileE path = do
  pugContent <- liftIO $ T.readFile path
  except $ parse path pugContent

parse :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Block]
parse fn = runParser (many pugNode <* eof) fn

-- | We expose the expression parser for development:
--
-- @
--     print $ Parse.parseExpr "1 + 2 * a"
-- @
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Code
parseExpr = runParser (sc *> pugCode <* eof) ""

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

pugNode :: Parser Block
pugNode = do
  node <-
    L.indentBlock scn $
      choice
        [ pugDoctype
        , try pugInclude
        , pugElement
        , pugPipe
        , pugCode'
        , pugFragmentDef
        , pugComment
        , pugFilter
        , pugRawElement
        , pugDefault
        , pugImport
        , pugRun
        , pugLet
        , try pugEach
        , pugIf
        , pugFragmentCall
        ]
  case node of
    BlockIf cond as _ -> do
      mbs <- optional $ L.indentBlock scn pugElse
      pure $ BlockIf cond as $ maybe [] id mbs
    _ -> pure node

pugIf :: Parser (L.IndentOpt Parser Block Block)
pugIf = do
  _ <- lexeme $ string "if"
  cond <- pugCode
  pure $ L.IndentMany Nothing (pure . (\as -> BlockIf cond as [])) pugNode

pugElse :: Parser (L.IndentOpt Parser [Block] Block)
pugElse = do
  _ <- lexeme $ string "else"
  pure $ L.IndentMany Nothing pure pugNode

pugElement :: Parser (L.IndentOpt Parser Block Block)
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
          pure $ L.IndentNone $ header [BlockText Dot [Lit $ T.intercalate "\n" items']]
        _ -> pure $ L.IndentNone $ header [BlockText Dot template]
    HasEqual -> do
      mcontent <- optional pugCode
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [BlockCode content]
        Nothing -> do
          scn
          content <- pugCode
          pure $ L.IndentNone $ header [BlockCode content]
    NoSym -> do
      template <- parseInlines
      case template of
        [] -> pure $ L.IndentMany Nothing (pure . header) pugNode
        _ -> pure $ L.IndentNone $ header [BlockText Normal template]

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
    n <- space'
    pos <- L.indentLevel
    done <- isJust <$> optional eof
    if done
      then return []
      else
        if pos <= ref
          then return []
          else do
            l <- p
            ls <- go
            let prefix = T.replicate (unPos pos - unPos ref) " "
                n' = replicate (n - 1) prefix
                l' = prefix <> l
            pure $ n' <> (l' : ls)

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
pugPipe :: Parser (L.IndentOpt Parser Block Block)
pugPipe = do
  ref <- L.indentLevel
  template <- p
  templates <- go ref
  pure $ L.IndentNone $ BlockText Pipe $ intercalate [Lit "\n"] (template : templates)
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

-- | A parser to convert the content of an @include@d to 'Syntax'. The
-- behavior w.r.t. to newlines should be the same as having each line
-- directly preceded by a @|@ in the including file.
pugTextInclude :: Text -> Block
pugTextInclude content =
  BlockText Include [Lit $ T.intercalate "\n" $ T.lines content]

pugCode' :: Parser (L.IndentOpt Parser Block Block)
pugCode' = do
  _ <- lexeme $ string "="
  content <- pugCode
  pure $ L.IndentNone $ BlockCode content

pugCode :: Parser Code
pugCode =
  pugExpression
    <|> (Object <$> pugObject)

pugVariable :: Parser Text
pugVariable = pugName

pugExpression :: Parser Code
pugExpression = makeExprParser pTerm operatorTable
 where
  pTerm =
    lexeme (Int <$> pugNumber)
      <|> lexeme (SingleQuoteString <$> pugSingleQuoteString)
      <|> lexeme pugVariable'
      <|> parens pugExpression
  parens = between (char '(') (char ')')

-- An operator table to define precedence and associativity
operatorTable :: [[Operator Parser Code]]
operatorTable =
  [ [InfixL (symbol "*" $> Times), InfixL (symbol "/" $> Divide)]
  , [InfixL (symbol "+" $> Add), InfixL (symbol "-" $> Sub)]
  ]

pugVariable' :: Parser Code
pugVariable' = do
  name <- pugName
  mkey <- optional $ do
    _ <- string "["
    key <- pugSingleQuoteString
    _ <- string "]"
    pure key
  case mkey of
    Nothing -> pure $ Variable name
    Just key -> pure $ Lookup name (SingleQuoteString key)

--------------------------------------------------------------------------------
pugDoctype :: Parser (L.IndentOpt Parser Block Block)
pugDoctype = do
  _ <- lexeme (string "doctype")
  _ <- lexeme (string "html")
  pure $ L.IndentNone BlockDoctype

--------------------------------------------------------------------------------
-- E.g. div, div.a, .a
pugDiv :: Parser ([Block] -> Block)
pugDiv =
  pugElemWithAttrs <|> pugAttrs

-- E.g. div, div.a, div()
pugElemWithAttrs :: Parser ([Block] -> Block)
pugElemWithAttrs = do
  (name, attrs, mdot) <-
    lexeme
      ( do
          a <- pugElem
          -- `try` because we want to backtrack if there is a dot
          -- not followed by a class name, for mdot to succeed.
          b <- many pugAttrs'
          mtrailing <-
            optional $
              choice
                [ string "." >> pure HasDot
                , string "=" >> pure HasEqual
                ]
          pure (a, concat b, maybe NoSym id mtrailing)
      )
      <?> "div tag"
  pure $ BlockElem name mdot attrs

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
        "br" -> pure Br
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
pugAttrs :: Parser ([Block] -> Block)
pugAttrs = do
  (attrs, mdot) <-
    lexeme
      ( do
          attrs <- some pugAttrs'
          mdot <- optional (string ".")
          pure (concat attrs, maybe NoSym (const HasDot) mdot)
      )
      <?> "attributes"
  pure $ BlockElem Div mdot attrs

pugAttrs' :: Parser [Attr]
pugAttrs' =
  -- `try` because we want to backtrack if there is a dot
  -- not followed by a class name, for mdot to succeed.
  ((: []) <$> pugId) <|> try ((: []) <$> pugClass) <|> pugAttrList

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
pugAttrList :: Parser [Attr]
pugAttrList = (<?> "attribute") $ do
  _ <- string "("
  pairs <- many pugPair
  _ <- string ")"
  pure $ map (uncurry Attr) pairs

pugPair :: Parser (Text, Maybe Code)
pugPair = do
  a <- T.pack <$> (some (noneOf (",()= \n" :: String))) <?> "key"
  mb <- optional $ do
    _ <- string "="
    b <- lexeme pugValue
    pure b
  _ <- optional (lexeme $ string ",")
  pure (a, mb)

pugValue :: Parser Code
pugValue =
  SingleQuoteString <$> pugSingleQuoteString
    <|> SingleQuoteString <$> pugDoubleQuoteString
    <|> Int <$> pugNumber

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
pugIdentifier = T.pack <$> lexeme (some (noneOf (" {}\n" :: String))) <?> "identifier"

--------------------------------------------------------------------------------
pugInclude :: Parser (L.IndentOpt Parser Block Block)
pugInclude = do
  mname <- lexeme $ do
    _ <- string "include"
    optional $ do
      _ <- string ":"
      pugIdentifier
  path <- pugPath
  pure $ L.IndentNone $ BlockInclude mname path Nothing

pugPath :: Parser FilePath
pugPath = lexeme (some (noneOf ['\n'])) <?> "path"

--------------------------------------------------------------------------------
pugFragmentDef :: Parser (L.IndentOpt Parser Block Block)
pugFragmentDef = do
  _ <- lexeme (string "fragment" <|> string "frag")
  name <- pugIdentifier
  params <- maybe [] id <$> optional pugParameters
  pure $ L.IndentMany Nothing (pure . BlockFragmentDef name params) pugNode

-- E.g. {}, {a, b}
pugParameters :: Parser [Text]
pugParameters = pugList' "{" "}" pugIdentifier <?> "arguments"

--------------------------------------------------------------------------------
pugFragmentCall :: Parser (L.IndentOpt Parser Block Block)
pugFragmentCall = do
  name <- pugIdentifier
  args <- maybe [] id <$> optional pugArguments
  pure $ L.IndentMany Nothing (pure . BlockFragmentCall name args) pugNode

-- E.g. {}, {1, 'a'}
pugArguments :: Parser [Code]
pugArguments = pugList' "{" "}" pugCode <?> "arguments"

--------------------------------------------------------------------------------
pugEach :: Parser (L.IndentOpt Parser Block Block)
pugEach = do
  _ <- lexeme (string "for")
  name <- lexeme pugName
  mindex <- optional $ do
    _ <- lexeme $ string ","
    lexeme pugName
  _ <- lexeme (string "in")
  collection <-
    (List <$> pugList) <|> (Object <$> pugObject) <|> (Variable <$> pugVariable)
  pure $ L.IndentMany Nothing (pure . BlockFor name mindex collection) pugNode

pugList :: Parser [Code]
pugList = pugList' "[" "]" pugCode

pugList' :: Text -> Text -> Parser a -> Parser [a]
pugList' before after p = do
  _ <- lexeme $ string before
  mx <- optional $ lexeme p
  xs <- case mx of
    Nothing -> pure []
    Just x -> do
      xs <- many $ do
        _ <- lexeme (string ",")
        lexeme p
      pure $ x : xs
  _ <- lexeme $ string after
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
pugComment :: Parser (L.IndentOpt Parser Block Block)
pugComment = do
  ref <- L.indentLevel
  b <-
    lexeme $
      choice
        [ string "---" *> pure PassthroughComment
        , string "--" *> pure NormalComment
        ]
  mcontent <- optional pugText
  case mcontent of
    Just content -> pure $ L.IndentNone $ BlockComment b content
    Nothing -> do
      scn
      items <- textBlock ref pugText
      let items' = realign items
      pure $ L.IndentNone $ BlockComment b $ T.intercalate "\n" items'

--------------------------------------------------------------------------------
pugFilter :: Parser (L.IndentOpt Parser Block Block)
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
    Just content -> pure $ L.IndentNone $ BlockFilter name content
    Nothing -> do
      scn
      items <- textBlock ref pugText
      let items' = realign items
      pure $ L.IndentNone $ BlockFilter name $ T.intercalate "\n" items'

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
pugRawElement :: Parser (L.IndentOpt Parser Block Block)
pugRawElement = do
  header <- pugAngleBracket
  pure $ L.IndentMany Nothing (pure . header) pugNode

pugAngleBracket :: Parser ([Block] -> Block)
pugAngleBracket = do
  _ <- char '<'
  content <- pugText
  pure $ BlockRawElem $ "<" <> content

--------------------------------------------------------------------------------
pugDefault :: Parser (L.IndentOpt Parser Block Block)
pugDefault = do
  _ <- lexeme (string "default")
  name <- pugText
  pure $ L.IndentMany Nothing (pure . BlockDefault name) pugNode

--------------------------------------------------------------------------------
pugImport :: Parser (L.IndentOpt Parser Block Block)
pugImport = do
  _ <- lexeme (string "import")
  path <- pugPath
  pure $ L.IndentMany Nothing (pure . BlockImport path Nothing) pugNode

--------------------------------------------------------------------------------
pugRun :: Parser (L.IndentOpt Parser Block Block)
pugRun = do
  _ <- lexeme (string "run")
  cmd <- pugText
  pure $ L.IndentNone $ BlockRun cmd Nothing

--------------------------------------------------------------------------------
pugLet :: Parser (L.IndentOpt Parser Block Block)
pugLet = do
  _ <- lexeme (string "let")
  name <- lexeme pugName
  _ <- lexeme (string "=")
  choice
    [ pugAssignVar name
    , pugReadJson name
    ]

pugAssignVar :: Text -> Parser (L.IndentOpt Parser Block Block)
pugAssignVar name = do
  val <- lexeme pugValue
  pure $ L.IndentNone $ BlockAssignVar name val

pugReadJson :: Text -> Parser (L.IndentOpt Parser Block Block)
pugReadJson name = do
  path <- pugPath
  pure $ L.IndentNone $ BlockReadJson name path Nothing

--------------------------------------------------------------------------------
scn :: Parser ()
scn = L.space space1 empty empty

-- Similar to space, but counts newlines
space' :: Parser Int
space' = do
  s <- takeWhileP (Just "white space") isSpace
  pure . length $ filter (== '\n') $ T.unpack s

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

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
parse' :: Text -> Either (M.ParseErrorBundle Text Void) [Inline]
parse' = M.parse (parseInlines <* M.eof) "-"

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
