module Pughs.Parse where

import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE (toList)
import Data.Set qualified as S (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec hiding (label, parse, unexpected)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
data PugNode
  = PugDoctype -- ^ Only @doctype html@ for now.
  | PugElem Elem TrailingDot [Attr] [PugNode]
  | PugText TextSyntax Text
  | PugInclude FilePath (Maybe [PugNode])
    -- ^ @Nothing@ when the template is parsed, then @Just nodes@ after
    -- preprocessing (i.e. actually running the include statement).
  | PugComment Text
  deriving (Show, Eq)

hasTrailingDot :: PugNode -> Bool
hasTrailingDot (PugElem _ HasDot _ _) = True
hasTrailingDot _ = False

data Elem
  = Html
  | Body
  | Div
  | Span
  | Hr
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Header
  | Head
  | Meta
  | Main
  | Link
  | A
  | P
  | Ul
  | Li
  | Footer
  | Figure
  | Blockquote
  | Figcaption
  | Audio
  | Script
  | Small
  | Source
  | Pre
  | Code
  | Img
  | I
  deriving (Show, Eq)

data TrailingDot = HasDot | NoDot
  deriving (Show, Eq)

data Attr = AttrList [(Text, Maybe Text)] | Class Text
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data TextSyntax
  = Normal -- ^ The text follows an element on the same line.
  | Pipe -- ^ The text follows a pipe character.
  | Dot -- ^ The text is part of a text block following a trailing dot.
  | Include -- ^ The text is the content of an include statement without a .pug extension.
  deriving (Show, Eq)

extractClasses :: [PugNode] -> [Text]
extractClasses = nub . sort . concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ attrs children) = concatMap g attrs <> extractClasses children
  f (PugText _ _) = []
  f (PugInclude _ children) = maybe [] extractClasses children
  f (PugComment _) = []
  g (AttrList xs) = concatMap h xs
  g (Class c) = [c]
  h ("class", Just c) = [c]
  h _ = []

--------------------------------------------------------------------------------
data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

-- Similarly to `parsePugFile` but pre-process the include statements.
preProcessPugFile :: FilePath -> IO (Either PreProcessError [PugNode])
preProcessPugFile = runExceptT . preProcessPugFileE

preProcessPugFileE :: FilePath -> ExceptT PreProcessError IO [PugNode]
preProcessPugFileE path = do
  pugContent <- liftIO $ T.readFile path
  let mnodes = first PreProcessParseError $ parsePug path pugContent
  nodes <- except mnodes
  preProcessNodesE path nodes

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preProcessNodesE :: FilePath -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
preProcessNodesE startPath nodes = mapM (preProcessNodeE startPath) nodes

preProcessNodeE :: FilePath -> PugNode -> ExceptT PreProcessError IO PugNode
preProcessNodeE startPath = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- preProcessNodesE startPath nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  PugInclude path _ -> do
    let includedPath = takeDirectory startPath </> path
    exists <- liftIO $ doesFileExist includedPath
    if exists
      then do
        content <- liftIO $ T.readFile includedPath
        let nodes' = map (PugText Include) $ T.lines content
        pure $ PugInclude path (Just nodes')
      else do
        nodes' <- preProcessPugFileE $ includedPath <> ".pug"
        pure $ PugInclude path (Just nodes')
  node@(PugComment _) -> pure node

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
pugNode = L.indentBlock scn $
  choice
    [ pugDoctype
    , try pugInclude
    , pugElement
    , pugPipe
    , pugComment
    ]

pugElement :: Parser (L.IndentOpt Parser PugNode PugNode)
pugElement = do
  header <- pugDiv
  mcontent <- optional pugText
  case mcontent of
    Nothing ->
      if hasTrailingDot $ header []
      then
        pure $ L.IndentMany Nothing (pure . header) pugTexts
      else
        pure $ L.IndentMany Nothing (pure . header) pugNode
    Just content ->
      pure $ L.IndentNone $ header [PugText Normal content]

pugPipe :: Parser (L.IndentOpt Parser PugNode PugNode)
pugPipe = do
  _ <- lexeme $ string "|"
  mcontent <- optional pugText
  pure $ L.IndentNone $ PugText Pipe $ maybe "" id mcontent

--------------------------------------------------------------------------------
pugDoctype :: Parser (L.IndentOpt Parser PugNode PugNode)
pugDoctype = do
  _ <- lexeme (string "doctype")
  _ <- lexeme (string "html")
  pure $ L.IndentNone PugDoctype

--------------------------------------------------------------------------------
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
  , string "hr" *> pure Hr
  , string "h1" *> pure H1
  , string "h2" *> pure H2
  , string "h3" *> pure H3
  , string "h4" *> pure H4
  , string "h5" *> pure H5
  , string "h6" *> pure H6
  , string "header" *> pure Header
  , string "head" *> pure Head
  , string "meta" *> pure Meta
  , string "main" *> pure Main
  , string "audio" *> pure Audio
  , string "a" *> pure A
  , string "code" *> pure Code
  , string "img" *> pure Img
  , string "i" *> pure I
  , string "pre" *> pure Pre
  , string "p" *> pure P
  , string "ul" *> pure Ul
  , string "link" *> pure Link
  , string "li" *> pure Li
  , string "footer" *> pure Footer
  , string "figure" *> pure Figure
  , string "blockquote" *> pure Blockquote
  , string "figcaption" *> pure Figcaption
  , string "script" *> pure Script
  , string "small" *> pure Small
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
  (char '.' *> some (alphaNumChar <|> oneOf ("-_" :: String))) <?> "class name"

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
    b <- lexeme (pugSingleQuoteString <|> pugDoubleQuoteString)
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

pugText :: Parser Text
pugText = T.pack <$> lexeme (some (noneOf ['\n'])) <?> "text content"

--------------------------------------------------------------------------------
pugInclude :: Parser (L.IndentOpt Parser PugNode PugNode)
pugInclude = do
  _ <- lexeme (string "include")
  path <- pugPath
  pure $ L.IndentNone $ PugInclude path Nothing

pugPath :: Parser FilePath
pugPath = lexeme (some (noneOf ['\n'])) <?> "path"

--------------------------------------------------------------------------------
pugComment :: Parser (L.IndentOpt Parser PugNode PugNode)
pugComment = do
  _ <- lexeme (string "//")
  content <- pugText
  pure $ L.IndentNone $ PugComment content

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
    (e:_) -> case e of
      TrivialError offset unexpected expected ->
        let
          pos = pstateSourcePos $ reachOffsetNoLine offset posState
          errorPos =
            T.pack (show (unPos (sourceLine pos)))
              <> ":" <> T.pack (show (unPos (sourceColumn pos)))
          unexpectedMsg = maybe "Unexpected end of input."
                          (\u -> "Unexpected " <> errorItemPretty u <> ".")
                          unexpected
          expectedMsg = if null expected
                        then ""
                        else "Expected " <> (T.intercalate ", " . map errorItemPretty . S.toList $ expected) <> "."
        in
          T.unwords
            [ "Error at", errorPos, "-", unexpectedMsg
            , if T.null expectedMsg then "." else expectedMsg
            ]
      FancyError offset _ -> "Complex error at position " <> T.pack (show offset)

errorItemPretty :: ErrorItem Char -> Text
errorItemPretty = \case
  Tokens ts -> "character '" <> T.pack (NE.toList ts) <> "'"
  Label label -> T.pack (NE.toList label)
  EndOfInput -> "end of input"
