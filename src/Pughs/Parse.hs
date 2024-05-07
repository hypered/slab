{-# LANGUAGE RecordWildCards #-}
module Pughs.Parse
  ( PugNode (..)
  , Elem (..)
  , TrailingDot (..)
  , Attr (..)
  , extractClasses
  , extractMixins
  , findMixin
  , PreProcessError (..)
  , parsePugFile
  , preProcessPugFile
  , parseErrorPretty
  ) where

import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except, throwE)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE (toList)
import Data.Maybe (isJust)
import Data.Set qualified as S (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void (Void)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import Text.Megaparsec hiding (label, parse, parseErrorPretty, unexpected, Label)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
data PugNode
  = PugDoctype -- ^ Only @doctype html@ for now.
  | PugElem Elem TrailingDot [Attr] [PugNode]
  | PugText TextSyntax Text
  | PugInclude FilePath (Maybe [PugNode])
    -- ^ @Nothing@ when the template is parsed, then @Just nodes@ after
    -- preprocessing (i.e. actually running the include statement).
  | PugMixinDef Text [PugNode]
  | PugMixinCall Text (Maybe [PugNode])
    -- ^ The Maybe works similarly to `PugInclude`.
  | PugComment Text
  | PugRawElem Text [PugNode]
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
  | Table
  | Thead
  | Tbody
  | Tr
  | Td
  | Dl
  | Dt
  | Dd
  | Footer
  | Figure
  | Form
  | Label
  | Blockquote
  | Button
  | Figcaption
  | Audio
  | Script
  | Style
  | Small
  | Source
  | Pre
  | Code
  | Img
  | I
  | Svg
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
  f (PugMixinDef _ _) = [] -- We extract them in PugMixinCall instead.
  f (PugMixinCall _ children) = maybe [] extractClasses children
  f (PugComment _) = []
  -- TODO Would be nice to extract classes from verbatim HTML too.
  f (PugRawElem _ _) = []
  g (AttrList xs) = concatMap h xs
  g (Class c) = [c]
  h ("class", Just c) = [c]
  h _ = []

-- Return type used for `extractMixins`.
data PugMixin
  = PugMixinDef' Text [PugNode]
  | PugMixinCall' Text
  deriving (Show, Eq)

extractMixins :: [PugNode] -> [PugMixin]
extractMixins = concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ _ children) = extractMixins children
  f (PugText _ _) = []
  f (PugInclude _ children) = maybe [] extractMixins children
  f (PugMixinDef name children) = [PugMixinDef' name children]
  f (PugMixinCall name children) = [PugMixinCall' name] <> maybe [] extractMixins children
  f (PugComment _) = []
  f (PugRawElem _ _) = []

findMixin :: Text -> [PugMixin] -> Maybe [PugNode]
findMixin name ms = case filter f ms of
  [PugMixinDef' _ nodes] -> Just nodes
  _ -> Nothing
 where
  f (PugMixinDef' name' _) = name == name'
  f _ = False

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  , ctxNodes :: [PugNode] -- ^ Nodes before pre-processing.
  }

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
  let ctx = Context
        { ctxStartPath = path
        , ctxNodes = nodes
        }
  nodes' <- preProcessNodesE ctx nodes
  preProcessMixinsE ctx { ctxNodes = nodes' } nodes'

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preProcessNodesE :: Context -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
preProcessNodesE ctx nodes = mapM (preProcessNodeE ctx) nodes

-- Process mixin calls. This should be done after processing the include statement
-- since mixins may be defined in included files.
preProcessMixinsE :: Context -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
preProcessMixinsE ctx nodes = mapM (preProcessMixinE ctx) nodes

preProcessNodeE :: Context -> PugNode -> ExceptT PreProcessError IO PugNode
preProcessNodeE ctx@Context {..} = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  PugInclude path _ -> do
    let includedPath = takeDirectory ctxStartPath </> path
        pugExt = takeExtension includedPath == ".pug"
    exists <- liftIO $ doesFileExist includedPath
    if exists && not pugExt
      then do
        -- Include the file content as-is.
        content <- liftIO $ T.readFile includedPath
        let nodes' = map (PugText Include) $ T.lines content
        pure $ PugInclude path (Just nodes')
      else do
        -- Parse and process the .pug file.
        let includedPath' = if pugExt then includedPath else includedPath <.> ".pug"
        nodes' <- preProcessPugFileE includedPath'
        pure $ PugInclude path (Just nodes')
  PugMixinDef name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugMixinDef name nodes'
  node@(PugMixinCall _ _) -> pure node
  node@(PugComment _) -> pure node
  node@(PugRawElem _ _) -> pure node

preProcessMixinE :: Context -> PugNode -> ExceptT PreProcessError IO PugNode
preProcessMixinE ctx@Context {..} = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- preProcessMixinsE ctx nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  PugInclude path mnodes -> do
    case mnodes of
      Just nodes -> do
        nodes' <- preProcessMixinsE ctx nodes
        pure $ PugInclude path (Just nodes')
      Nothing ->
        pure $ PugInclude path Nothing
  PugMixinDef name nodes -> do
    nodes' <- preProcessMixinsE ctx nodes
    pure $ PugMixinDef name nodes'
  PugMixinCall name _ -> do
    case findMixin name $ extractMixins ctxNodes of
      Just body ->
        pure $ PugMixinCall name (Just body)
      Nothing -> throwE $ PreProcessError $ "Can't find mixin \"" <> name <> "\""
  node@(PugComment _) -> pure node
  node@(PugRawElem _ _) -> pure node

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
    , pugMixinDef
    , pugMixinCall
    , pugComment
    , pugRawElement
    ]

pugElement :: Parser (L.IndentOpt Parser PugNode PugNode)
pugElement = do
  ref <- L.indentLevel
  header <- pugDiv
  mcontent <- optional pugText
  case mcontent of
    Nothing ->
      if hasTrailingDot $ header []
      then do
        scn
        items <- textBlock ref pugText
        let items' = realign items
        pure $ L.IndentNone $ header [PugText Dot $ T.intercalate "\n" items']
      else
        pure $ L.IndentMany Nothing (pure . header) pugNode
    Just content ->
      pure $ L.IndentNone $ header [PugText Normal content]

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
            else
              ((:) . (T.replicate (unPos pos - unPos ref) " " <>)) <$> p <*> go

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
  , string "table" *> pure Table
  , string "thead" *> pure Thead
  , string "tbody" *> pure Tbody
  , string "tr" *> pure Tr
  , string "td" *> pure Td
  , string "dl" *> pure Dl
  , string "dt" *> pure Dt
  , string "dd" *> pure Dd
  , string "footer" *> pure Footer
  , string "figure" *> pure Figure
  , string "form" *> pure Form
  , string "label" *> pure Label
  , string "blockquote" *> pure Blockquote
  , string "button" *> pure Button
  , string "figcaption" *> pure Figcaption
  , string "script" *> pure Script
  , string "style" *> pure Style
  , string "small" *> pure Small
  , string "source" *> pure Source
  , string "svg" *> pure Svg
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
pugMixinDef:: Parser (L.IndentOpt Parser PugNode PugNode)
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
pugComment :: Parser (L.IndentOpt Parser PugNode PugNode)
pugComment = do
  _ <- lexeme (string "//")
  content <- pugText
  pure $ L.IndentNone $ PugComment content

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
      FancyError offset err -> "Complex error at position " <> T.pack (show offset) <> ": " <> TL.toStrict (pShowNoColor err)

errorItemPretty :: ErrorItem Char -> Text
errorItemPretty = \case
  Tokens ts -> "character '" <> T.pack (NE.toList ts) <> "'"
  M.Label label -> T.pack (NE.toList label)
  EndOfInput -> "end of input"
