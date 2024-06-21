{-# LANGUAGE RecordWildCards #-}

module Slab.Parse
  ( parseFile
  , parseFileE
  , parse
  , parseExpr
  , parserTextInclude
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
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, withExceptT)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Slab.Error qualified as Error
import Slab.Syntax
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
parseFile :: FilePath -> IO (Either Error.Error [Block])
parseFile = runExceptT . parseFileE

parseFileE :: FilePath -> ExceptT Error.Error IO [Block]
parseFileE path = do
  content <- liftIO $ T.readFile path
  withExceptT Error.ParseError . except $ parse path content

--------------------------------------------------------------------------------

parse :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Block]
parse fn = runParser (many parserBlock <* eof) fn

-- | We expose the expression parser for development:
--
-- @
--     Parse.parseExpr "1 + 2 * a"
-- @
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> parserExpr <* eof) ""

--------------------------------------------------------------------------------
type Parser = Parsec Void Text

parserBlock :: Parser Block
parserBlock = do
  node <-
    L.indentBlock scn $
      choice
        [ parserDoctype
        , try parserInclude
        , parserElement
        , parserPipe
        , parserExpr'
        , parserFragmentDef
        , parserComment
        , parserFilter
        , parserRawElement
        , parserDefault
        , parserImport
        , parserRun
        , parserLet
        , try parserEach
        , try parserIf
        , parserFragmentCall
        ]
  case node of
    BlockIf cond as _ -> do
      mbs <- optional $ L.indentBlock scn parserElse
      pure $ BlockIf cond as $ maybe [] id mbs
    _ -> pure node

parserIf :: Parser (L.IndentOpt Parser Block Block)
parserIf = do
  _ <- string "if"
  _ <- some (char ' ' <|> char '\t')
  cond <- parserExpr
  pure $ L.IndentMany Nothing (pure . (\as -> BlockIf cond as [])) parserBlock

parserElse :: Parser (L.IndentOpt Parser [Block] Block)
parserElse = do
  _ <- lexeme $ string "else"
  pure $ L.IndentMany Nothing pure parserBlock

parserElement :: Parser (L.IndentOpt Parser Block Block)
parserElement = do
  ref <- L.indentLevel
  header <- parserDiv
  parserElemBody ref header

-- | Parse the indented content of an HTML element or a fragment call.
parserElemBody :: Pos -> ([Block] -> Block) -> Parser (L.IndentOpt Parser Block Block)
parserElemBody ref header =
  case trailingSym $ header [] of
    HasDot -> do
      template <- parseInlines
      case template of
        [] -> do
          scn
          items <- textBlock ref parserText -- TODO Use parseInlines
          let items' = realign items
          pure $ L.IndentNone $ header [BlockText Dot [Lit $ T.intercalate "\n" items']]
        _ -> pure $ L.IndentNone $ header [BlockText Dot template]
    HasEqual -> do
      mcontent <- optional parserExpr
      case mcontent of
        Just content -> pure $ L.IndentNone $ header [BlockCode content]
        Nothing -> do
          scn
          content <- parserExpr
          pure $ L.IndentNone $ header [BlockCode content]
    NoSym -> do
      template <- parseInlines
      case template of
        [] -> pure $ L.IndentMany Nothing (pure . header) parserBlock
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
                n' = replicate (n - 1) ""
                l' = prefix <> l
            pure $ n' <> (l' : ls)

-- | Considering all the given lines as a block, strip the leading whitespace of
-- each line so that the left-most character of the block is in the first
-- column.
realign :: [Text] -> [Text]
realign xs = map (T.drop n) xs
 where
  n = minimum $ map (T.length . T.takeWhile (== ' ')) $ filter (not . T.null) xs

-- | Parse multiple lines starting each with a pipe prefix. Most of our parsers
-- parse a single line (and optional indented child lines) and most nodes map
-- to a single line, but here we want to be able to view such blocks as a
-- single node, and we make sur to add newlines between each when rendered.
parserPipe :: Parser (L.IndentOpt Parser Block Block)
parserPipe = do
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
parserTextInclude :: Text -> Block
parserTextInclude content =
  BlockText Include [Lit $ T.intercalate "\n" $ T.lines content]

parserExpr' :: Parser (L.IndentOpt Parser Block Block)
parserExpr' = do
  _ <- lexeme $ string "="
  content <- parserExpr
  pure $ L.IndentNone $ BlockCode content

parserExpr :: Parser Expr
parserExpr = makeExprParser pApp operatorTable
 where
  pApp = do
    a <- pTerm
    mb <- optional $ pTerm
    case mb of
      Nothing -> pure a
      Just b -> pure $ Application a b
  pTerm =
    lexeme (Int <$> parserNumber)
      <|> lexeme (SingleQuoteString <$> parserSingleQuoteString)
      <|> lexeme (SingleQuoteString <$> parserDoubleQuoteString) -- TODO Double
      <|> lexeme parserVariable'
      <|> lexeme (Object <$> parserObject)
      <|> parens parserExpr
  parens = between (lexeme $ char '(') (lexeme $ char ')')

parserVariable :: Parser Text
parserVariable = parserName

-- An operator table to define precedence and associativity
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [InfixL (symbol "*" $> Times), InfixL (symbol "/" $> Divide)]
  , [InfixL (symbol "+" $> Add), InfixL (symbol "-" $> Sub)]
  , [InfixL (symbol ">" $> GreaterThan), InfixL (symbol "<" $> LesserThan)]
  , [InfixL (symbol "==" $> Equal)]
  , -- I'd like to use : instead, but it is already used for objects...
    [InfixR (symbol "|" $> Cons)]
  ]

parserVariable' :: Parser Expr
parserVariable' = do
  name <- parserName
  mkey <- optional $ do
    _ <- string "["
    key <- parserSingleQuoteString
    _ <- string "]"
    pure key
  case mkey of
    Nothing -> pure $ Variable name
    Just key -> pure $ Lookup name (SingleQuoteString key)

--------------------------------------------------------------------------------
parserDoctype :: Parser (L.IndentOpt Parser Block Block)
parserDoctype = do
  _ <- lexeme (string "doctype")
  _ <- lexeme (string "html")
  pure $ L.IndentNone BlockDoctype

--------------------------------------------------------------------------------
-- E.g. div, div.a, .a
parserDiv :: Parser ([Block] -> Block)
parserDiv =
  parserElemWithAttrs <|> parserAttrs

-- E.g. div, div.a, div()
parserElemWithAttrs :: Parser ([Block] -> Block)
parserElemWithAttrs = do
  (name, attrs, mdot) <- parserNameWithAttrs
  pure $ BlockElem name mdot attrs

parserNameWithAttrs :: Parser (Elem, [Attr], TrailingSym)
parserNameWithAttrs =
  lexeme
    ( do
        el <- parserElem
        attrs <- concat <$> many parserAttrs'
        trailing <- parserTrailingSym
        pure (el, attrs, trailing)
    )
    <?> "element"

parserElem :: Parser Elem
parserElem =
  (lexeme (string "el") *> (Elem <$> lexeme parserName))
    <?> "element name"

-- E.g. .a, ()
parserAttrs :: Parser ([Block] -> Block)
parserAttrs = do
  (attrs, mdot) <-
    lexeme
      ( do
          attrs <- concat <$> some parserAttrs'
          mdot <- optional (string ".")
          pure (attrs, maybe NoSym (const HasDot) mdot)
      )
      <?> "attributes"
  pure $ BlockElem Div mdot attrs

parserAttrs' :: Parser [Attr]
parserAttrs' =
  -- `try` because we want to backtrack if there is a dot
  -- not followed by a class name, for mdot to succeed.
  ((: []) <$> parserId) <|> try ((: []) <$> parserClass) <|> parserAttrList

parserTrailingSym :: Parser TrailingSym
parserTrailingSym = do
  ms <-
    optional $
      choice
        [ string "." >> pure HasDot
        , string "=" >> pure HasEqual
        ]
  pure $ maybe NoSym id ms

-- E.g. #a
parserId :: Parser Attr
parserId =
  Id
    . T.pack
    <$> (char '#' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "id"

-- E.g. .a
parserClass :: Parser Attr
parserClass =
  Class
    . T.pack
    <$> (char '.' *> some (alphaNumChar <|> oneOf ("-_" :: String)))
    <?> "class name"

-- E.g. (), (class='a')
parserAttrList :: Parser [Attr]
parserAttrList = (<?> "attribute") $ do
  _ <- string "("
  pairs <- many parserPair
  _ <- string ")"
  pure $ map (uncurry Attr) pairs

parserPair :: Parser (Text, Maybe Expr)
parserPair = do
  a <- T.pack <$> (some (noneOf (",()= \n" :: String))) <?> "key"
  mb <- optional $ do
    _ <- string "="
    b <- lexeme parserValue
    pure b
  _ <- optional (lexeme $ string ",")
  pure (a, mb)

parserValue :: Parser Expr
parserValue =
  SingleQuoteString <$> parserSingleQuoteString
    <|> SingleQuoteString <$> parserDoubleQuoteString
    <|> Int <$> parserNumber

parserSingleQuoteString :: Parser Text
parserSingleQuoteString = do
  _ <- string "'"
  s <- T.pack <$> (many (noneOf ("'\n" :: String))) <?> "string"
  _ <- string "'"
  pure s

parserDoubleQuoteString :: Parser Text
parserDoubleQuoteString = do
  _ <- string "\""
  s <- T.pack <$> (many (noneOf ("\"\n" :: String))) <?> "string"
  _ <- string "\""
  pure s

-- TODO Proper data type.
parserNumber :: Parser Int
parserNumber = L.decimal

parserText :: Parser Text
parserText = T.pack <$> lexeme (some (noneOf ['\n'])) <?> "text content"

parserIdentifier :: Parser Text
parserIdentifier = T.pack <$> (some (noneOf (" .=#(){}\n" :: String))) <?> "identifier"

--------------------------------------------------------------------------------
parserInclude :: Parser (L.IndentOpt Parser Block Block)
parserInclude = do
  mname <- lexeme $ do
    _ <- string "include"
    optional $ do
      _ <- string ":"
      parserIdentifier
  path <- parserPath
  pure $ L.IndentNone $ BlockInclude mname path Nothing

parserPath :: Parser FilePath
parserPath = lexeme (some (noneOf ("'\"\n" :: String))) <?> "path"

--------------------------------------------------------------------------------
parserFragmentDef :: Parser (L.IndentOpt Parser Block Block)
parserFragmentDef = do
  _ <- lexeme (string "fragment" <|> string "frag")
  name <- lexeme parserIdentifier
  params <- maybe [] id <$> optional parserParameters
  pure $ L.IndentMany Nothing (pure . BlockFragmentDef name params) parserBlock

-- E.g. {}, {a, b}
parserParameters :: Parser [Text]
parserParameters = parserList' "{" "}" (lexeme parserIdentifier) <?> "arguments"

--------------------------------------------------------------------------------
parserFragmentCall :: Parser (L.IndentOpt Parser Block Block)
parserFragmentCall = do
  ref <- L.indentLevel
  header <- parserCall
  parserElemBody ref header

parserCall :: Parser ([Block] -> Block)
parserCall = do
  -- TODO Use parserNameWithAttrs.
  (name, attrs, trailing, args) <- lexeme $ do
    name <- parserIdentifier
    attrs <- concat <$> many parserAttrs'
    trailing <- parserTrailingSym
    args <- maybe [] id <$> optional parserArguments
    pure (name, attrs, trailing, args)
  pure $ BlockFragmentCall name trailing attrs args

-- E.g. {}, {1, 'a'}
parserArguments :: Parser [Expr]
parserArguments = parserList' "{" "}" parserExpr <?> "arguments"

--------------------------------------------------------------------------------
parserEach :: Parser (L.IndentOpt Parser Block Block)
parserEach = do
  _ <- string "for"
  _ <- some (char ' ' <|> char '\t')
  name <- lexeme parserName
  mindex <- optional $ do
    _ <- lexeme $ string ","
    lexeme parserName
  _ <- lexeme (string "in")
  collection <-
    (List <$> parserList) <|> (Object <$> parserObject) <|> (Variable <$> parserVariable)
  pure $ L.IndentMany Nothing (pure . BlockFor name mindex collection) parserBlock

parserList :: Parser [Expr]
parserList = parserList' "[" "]" parserExpr

parserList' :: Text -> Text -> Parser a -> Parser [a]
parserList' before after p = do
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

parserObject :: Parser [(Expr, Expr)]
parserObject = do
  _ <- lexeme (string "{")
  mkv <- optional $ do
    key <- lexeme parserExpr
    _ <- lexeme (string ":")
    val <- lexeme parserExpr
    pure (key, val)
  kvs <- case mkv of
    Nothing -> pure []
    Just kv -> do
      kvs <- many $ do
        _ <- lexeme $ string ","
        key <- lexeme parserExpr
        _ <- lexeme (string ":")
        val <- lexeme parserExpr
        pure (key, val)
      pure $ kv : kvs
  _ <- lexeme (string "}")
  pure kvs

--------------------------------------------------------------------------------
parserComment :: Parser (L.IndentOpt Parser Block Block)
parserComment = do
  ref <- L.indentLevel
  b <-
    lexeme $
      choice
        [ string "---" *> pure PassthroughComment
        , string "--" *> pure NormalComment
        ]
  mcontent <- optional parserText
  case mcontent of
    Just content -> pure $ L.IndentNone $ BlockComment b content
    Nothing -> do
      scn
      items <- textBlock ref parserText
      let items' = realign items
      pure $ L.IndentNone $ BlockComment b $ T.intercalate "\n" items'

--------------------------------------------------------------------------------
parserFilter :: Parser (L.IndentOpt Parser Block Block)
parserFilter = do
  ref <- L.indentLevel
  name <-
    lexeme
      ( string ":"
          *> parserName
      )
      <?> "filter name"
  mcontent <- optional parserText
  case mcontent of
    Just content -> pure $ L.IndentNone $ BlockFilter name content
    Nothing -> do
      scn
      items <- textBlock ref parserText
      let items' = realign items
      pure $ L.IndentNone $ BlockFilter name $ T.intercalate "\n" items'

parserName :: Parser Text
parserName =
  T.pack
    <$> ( do
            a <- letterChar
            as <- many (alphaNumChar <|> oneOf ("-_" :: String))
            pure (a : as)
        )
    <?> "name"

--------------------------------------------------------------------------------
parserRawElement :: Parser (L.IndentOpt Parser Block Block)
parserRawElement = do
  header <- parserAngleBracket
  pure $ L.IndentMany Nothing (pure . header) parserBlock

parserAngleBracket :: Parser ([Block] -> Block)
parserAngleBracket = do
  _ <- char '<'
  content <- parserText
  pure $ BlockRawElem $ "<" <> content

--------------------------------------------------------------------------------
parserDefault :: Parser (L.IndentOpt Parser Block Block)
parserDefault = do
  _ <- lexeme (string "default")
  name <- parserText
  pure $ L.IndentMany Nothing (pure . BlockDefault name) parserBlock

--------------------------------------------------------------------------------
parserImport :: Parser (L.IndentOpt Parser Block Block)
parserImport = do
  _ <- lexeme (string "import")
  path <- parserPath
  pure $ L.IndentMany Nothing (pure . BlockImport path Nothing) parserBlock

--------------------------------------------------------------------------------
parserRun :: Parser (L.IndentOpt Parser Block Block)
parserRun = do
  _ <- lexeme (string "run")
  cmd <- parserText
  pure $ L.IndentNone $ BlockRun cmd Nothing

--------------------------------------------------------------------------------
parserLet :: Parser (L.IndentOpt Parser Block Block)
parserLet = do
  _ <- lexeme (string "let")
  name <- lexeme parserName
  _ <- lexeme (string "=")
  choice
    [ parserAssignVar name
    , parserReadJson name
    ]

parserAssignVar :: Text -> Parser (L.IndentOpt Parser Block Block)
parserAssignVar name = do
  val <- parserExpr
  pure $ L.IndentNone $ BlockAssignVar name val

parserReadJson :: Text -> Parser (L.IndentOpt Parser Block Block)
parserReadJson name = do
  path <- parserPath
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

-- Text interpolation stuff

-- Text interpolation modeled on the @template@ library by Johan Tibell.
-- - This uses Megaparsec instead of an internal State monad parser.
-- - This uses @#@ instead of @$@, and both @()@ and @{}@.
--   - @()@ uses the expression syntax.
--   - @{}@ uses the block syntax (although limited).
-- - Only the safe parsers are provided.
-- - Only the applicative interface is provided.
-- - We don't support #name, but only #(name), because # can appear
--   in character entities, URLs, ...
-- TODO Mention the BSD-3 license and Johan.
-- TODO Actually support #name.

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

-- | Record whether we are parsing a template within the normal block syntax,
-- or within an inline block syntax (introduced by #{...}). This is allow a
-- closing curly bracket in the normal case without requiring to escape it, and
-- disallowing it in the inline case (since it is used to end the inline case).
data InlineContext = NormalBlock | InlineBlock

parseInlines :: Parser [Inline]
parseInlines = parseInlines' NormalBlock

parseInlines' :: InlineContext -> Parser [Inline]
parseInlines' ctx = combineLits <$> M.many (parseInline ctx)

parseInline :: InlineContext -> Parser Inline
parseInline ctx =
  M.choice
    [ parseLit ctx
    , parsePlaceExpr
    , parsePlaceBlock
    , parseEscape
    , parseSharpLit
    ]

-- TODO The \n condition could be optional if we want this module to be useful
-- outside Slab.
parseLit :: InlineContext -> Parser Inline
parseLit ctx = do
  s <- case ctx of
    NormalBlock -> M.takeWhile1P (Just "literal") (\c -> c /= '#' && c /= '\n')
    InlineBlock -> M.takeWhile1P (Just "literal") (\c -> c /= '#' && c /= '\n' && c /= '}')
  pure $ Lit s

parsePlaceExpr :: Parser Inline
parsePlaceExpr = do
  _ <- string $ T.pack "#("
  e <- parserExpr
  _ <- string $ T.pack ")"
  pure $ Place e

parsePlaceBlock :: Parser Inline
parsePlaceBlock = do
  _ <- string $ T.pack "#{"
  e <- Block <$> parseInlineBlock
  _ <- string $ T.pack "}"
  pure $ Place e

-- | Equivalent to `parserBlock` but in an inline context.
parseInlineBlock :: Parser Block
parseInlineBlock = do
  header <- parserDiv <|> parserCall
  template <- parseInlines' InlineBlock
  -- Don't return anything of the template is empty. to avoid a newline when
  -- rendering.
  if null template
    then pure $ header []
    else pure $ header [BlockText Dot template]

parseEscape :: Parser Inline
parseEscape = do
  _ <- string $ T.pack "##"
  pure $ Lit $ T.pack "#"

parseSharpLit :: Parser Inline
parseSharpLit = do
  _ <- string $ T.pack "#"
  s <- M.takeWhile1P Nothing (\c -> c /= '#' && c /= '\n')
  pure $ Lit $ "#" <> s
