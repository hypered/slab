{-# LANGUAGE RecordWildCards #-}

module Slab.Syntax
  ( Block (..)
  , isDoctype
  , Elem (..)
  , TrailingSym (..)
  , Attr (..)
  , TextSyntax (..)
  , Code (..)
  , Inline (..)
  , Env (..)
  , emptyEnv
  , trailingSym
  , extractClasses
  , extractFragments
  , findFragment
  , idNamesFromAttrs
  , classNamesFromAttrs
  , namesFromAttrs
  , groupAttrs
  ) where

import Data.Aeson qualified as Aeson
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
data Block
  = -- | Only @doctype html@ for now.
    BlockDoctype
  | PugElem Elem TrailingSym [Attr] [Block]
  | PugText TextSyntax [Inline]
  | -- | @Nothing@ when the template is parsed, then @Just nodes@ after
    -- preprocessing (i.e. actually running the include statement).
    PugInclude FilePath (Maybe [Block])
  | -- | This doesn't exit in Pug. This is like a mixin than receive block arguments.
    -- Or like a parent template that can be @extended@ by a child template.
    PugFragmentDef Text [Text] [Block]
  | PugFragmentCall Text [Code] [Block]
  | PugEach Text (Maybe Text) Code [Block]
  | -- | Whether or not the comment must appear in the output.
    PugComment Bool Text
  | PugFilter Text Text
  | PugRawElem Text [Block]
  | -- | @default@ defines an optional formal parameter with a default content.
    -- Its content is used when the argument is not given.
    PugDefault Text [Block]
  | -- | Similar to an anonymous fragment call, where the fragment body is the
    -- content of the referenced file.
    PugImport FilePath (Maybe [Block]) [Block]
  | -- | Allow to assign the content of a JSON file to a variable. The syntax
    -- is specific to how Struct has a @require@ function in scope.
    PugReadJson Text FilePath (Maybe Aeson.Value)
  | -- | Only support assigning a string for now.
    PugAssignVar Text Text
  | PugIf Code [Block] [Block]
  | PugList [Block]
  | BlockCode Code
  deriving (Show, Eq)

isDoctype :: Block -> Bool
isDoctype BlockDoctype = True
isDoctype _ = False

trailingSym :: Block -> TrailingSym
trailingSym (PugElem _ sym _ _) = sym
trailingSym _ = NoSym

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
  | Title
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
  | IFrame
  | Input
  | I
  | Svg
  | Textarea
  | Canvas
  deriving (Show, Eq)

data TrailingSym = HasDot | HasEqual | NoSym
  deriving (Show, Eq)

-- The Code must already be evaluated.
data Attr = Id Text | Class Text | Attr Text (Maybe Code)
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data TextSyntax
  = -- | The text follows an element on the same line.
    Normal
  | -- | The text follows a pipe character. Multiple lines each introduced by a
    -- pipe symbol are grouped as a single 'PugText' node.
    Pipe
  | -- | The text is part of a text block following a trailing dot.
    Dot
  | -- | The text is the content of an include statement without a .slab extension.
    Include
  deriving (Show, Eq)

-- Minimal support for some JS expressions.
data Code
  = Variable Text
  | Int Int
  | SingleQuoteString Text
  | List [Code]
  | Object [(Code, Code)]
  | -- The object[key] lookup. This is quite restrive as a start.
    Lookup Text Code
  | Add Code Code
  | -- Code can be a fragment, so we can manipulate them with code later.
    -- We also capture the current environment.
    Frag [Text] Env [Block]
  deriving (Show, Eq)

-- | A representation of a 'Data.Text' template is a list of Inline, supporting
-- efficient rendering. Use 'parse' to create a template from a text containing
-- placeholders. 'Lit' is a literal Text value. 'Place' is a placeholder created
-- with @#{...}@.
data Inline = Lit {-# UNPACK #-} !Text | Place !Code
  deriving (Eq, Show)

data Env = Env
  { envVariables :: [(Text, Code)]
  }
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env []

--------------------------------------------------------------------------------

extractClasses :: [Block] -> [Text]
extractClasses = nub . sort . concatMap f
 where
  f BlockDoctype = []
  f (PugElem _ _ attrs children) = concatMap g attrs <> extractClasses children
  f (PugText _ _) = []
  f (PugInclude _ children) = maybe [] extractClasses children
  f (PugFragmentDef _ _ _) = [] -- We extract them in PugFragmentCall instead.
  f (PugFragmentCall _ _ children) = extractClasses children
  f (PugEach _ _ _ children) = extractClasses children
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  -- TODO Would be nice to extract classes from verbatim HTML too.
  f (PugRawElem _ _) = []
  f (PugDefault _ children) = extractClasses children
  f (PugImport _ children blocks) = maybe [] extractClasses children <> extractClasses blocks
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ as bs) = extractClasses as <> extractClasses bs
  f (PugList children) = extractClasses children
  f (BlockCode _) = []

  g (Id _) = []
  g (Class c) = [c]
  g (Attr a b) = h a b
  h "class" (Just (SingleQuoteString c)) = [c]
  h "class" _ = error "The class is not a string"
  h _ _ = []

-- Return type used for `extractFragments`.
data PugMixin
  = PugFragmentDef' Text [Block]
  | PugFragmentCall' Text
  deriving (Show, Eq)

extractFragments :: [Block] -> [PugMixin]
extractFragments = concatMap f
 where
  f BlockDoctype = []
  f (PugElem _ _ _ children) = extractFragments children
  f (PugText _ _) = []
  f (PugInclude _ children) = maybe [] extractFragments children
  f (PugFragmentDef name _ children) = [PugFragmentDef' name children]
  f (PugFragmentCall name _ children) = [PugFragmentCall' name] <> extractFragments children
  f (PugEach _ _ _ children) = extractFragments children
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugDefault _ children) = extractFragments children
  f (PugImport _ children args) = maybe [] extractFragments children <> extractFragments args
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ as bs) = extractFragments as <> extractFragments bs
  f (PugList children) = extractFragments children
  f (BlockCode _) = []

findFragment :: Text -> [PugMixin] -> Maybe [Block]
findFragment name ms = case filter f ms of
  [PugFragmentDef' _ nodes] -> Just nodes
  _ -> Nothing
 where
  f (PugFragmentDef' name' _) = name == name'
  f _ = False

--------------------------------------------------------------------------------
idNamesFromAttrs :: [Attr] -> [Text]
idNamesFromAttrs =
  concatMap
    ( \case
        Id i -> [i]
        Class _ -> []
        Attr a b -> f a b
    )
 where
  f "id" (Just (SingleQuoteString x)) = [x]
  f "id" (Just _) = error "The id is not a string"
  f _ _ = []

classNamesFromAttrs :: [Attr] -> [Text]
classNamesFromAttrs =
  concatMap
    ( \case
        Id _ -> []
        Class c -> [c]
        Attr a b -> f a b
    )
 where
  f "class" (Just (SingleQuoteString x)) = [x]
  f "class" (Just _) = error "The class is not a string"
  f _ _ = []

namesFromAttrs :: [Attr] -> [(Text, Text)]
namesFromAttrs =
  concatMap
    ( \case
        Id _ -> []
        Class _ -> []
        Attr a b -> f a b
    )
 where
  f "id" _ = []
  f "class" _ = []
  f a (Just (SingleQuoteString b)) = [(a, b)]
  f a (Just (Int b)) = [(a, T.pack $ show b)]
  f _ (Just _) = error "The attribute is not a string"
  f a Nothing = [(a, a)]

-- | Group multiple classes or IDs in a single class or ID, and transform the
-- other attributes in 'SingleQuoteString's.
groupAttrs :: [Attr] -> [Attr]
groupAttrs attrs = elemId <> elemClass <> elemAttrs
 where
  idNames = idNamesFromAttrs attrs
  idNames' :: Text
  idNames' = T.intercalate " " idNames
  elemId =
    if idNames == []
    then []
    else [Id idNames']

  classNames = classNamesFromAttrs attrs
  classNames' :: Text
  classNames' = T.intercalate " " classNames
  elemClass =
    if classNames == []
    then []
    else [Class classNames']

  attrs' = namesFromAttrs attrs
  elemAttrs = map (\(a, b) -> Attr a (Just $ SingleQuoteString b)) attrs'
