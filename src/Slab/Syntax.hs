{-# LANGUAGE RecordWildCards #-}

module Slab.Syntax
  ( PugNode (..)
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
  , extractMixins
  , findMixin
  ) where

import Data.Aeson qualified as Aeson
import Data.List (nub, sort)
import Data.Text (Text)

--------------------------------------------------------------------------------
data PugNode
  = -- | Only @doctype html@ for now.
    PugDoctype
  | PugElem Elem TrailingSym [Attr] [PugNode]
  | PugText TextSyntax [Inline]
  | -- | Recognize only strings for now.
    PugCode Code
  | -- | @Nothing@ when the template is parsed, then @Just nodes@ after
    -- preprocessing (i.e. actually running the include statement).
    PugInclude FilePath (Maybe [PugNode])
  | PugMixinDef Text [PugNode]
  | -- | The Maybe works similarly to `PugInclude`.
    PugMixinCall Text (Maybe [PugNode])
  | -- | This doesn't exit in Pug. This is like a mixin than receive block arguments.
    -- Or like a parent template that can be @extended@ by a child template.
    PugFragmentDef Text [PugNode]
  | PugFragmentCall Text [PugNode]
  | PugEach Text (Maybe Text) Code [PugNode]
  | -- | Whether or not the comment must appear in the output.
    PugComment Bool Text
  | PugFilter Text Text
  | PugRawElem Text [PugNode]
  | -- | @default@ defines an optional formal parameter with a default content.
    -- Its content is used when the argument is not given.
    PugDefault Text [PugNode]
  | -- | Similar to an anonymous fragment call, where the fragment body is the
    -- content of the referenced file.
    PugImport FilePath (Maybe [PugNode]) [PugNode]
  | -- | Allow to assign the content of a JSON file to a variable. The syntax
    -- is specific to how Struct has a @require@ function in scope.
    PugReadJson Text FilePath (Maybe Aeson.Value)
  | -- | Only support assigning a string for now.
    PugAssignVar Text Text
  | PugIf Code [PugNode] [PugNode]
  | PugList [PugNode]
  deriving (Show, Eq)

trailingSym :: PugNode -> TrailingSym
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
data Attr = AttrList [(Text, Maybe Code)] | Id Text | Class Text
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
    Frag Env [PugNode]
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

extractClasses :: [PugNode] -> [Text]
extractClasses = nub . sort . concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ attrs children) = concatMap g attrs <> extractClasses children
  f (PugText _ _) = []
  f (PugCode _) = []
  f (PugInclude _ children) = maybe [] extractClasses children
  f (PugMixinDef _ _) = [] -- We extract them in PugMixinCall instead.
  f (PugMixinCall _ children) = maybe [] extractClasses children
  f (PugFragmentDef _ _) = [] -- We extract them in PugFragmentCall instead.
  f (PugFragmentCall _ children) = extractClasses children
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

  g (AttrList xs) = concatMap h xs
  g (Id _) = []
  g (Class c) = [c]
  h ("class", Just (SingleQuoteString c)) = [c]
  h ("class", _) = error "The class is not a string"
  h _ = []

-- Return type used for `extractMixins`.
data PugMixin
  = PugMixinDef' Text [PugNode]
  | PugMixinCall' Text
  | PugFragmentDef' Text [PugNode]
  | PugFragmentCall' Text
  deriving (Show, Eq)

extractMixins :: [PugNode] -> [PugMixin]
extractMixins = concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ _ children) = extractMixins children
  f (PugText _ _) = []
  f (PugCode _) = []
  f (PugInclude _ children) = maybe [] extractMixins children
  f (PugMixinDef name children) = [PugMixinDef' name children]
  f (PugMixinCall name children) = [PugMixinCall' name] <> maybe [] extractMixins children
  f (PugFragmentDef name children) = [PugFragmentDef' name children]
  f (PugFragmentCall name children) = [PugFragmentCall' name] <> extractMixins children
  f (PugEach _ _ _ children) = extractMixins children
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugDefault _ children) = extractMixins children
  f (PugImport _ children args) = maybe [] extractMixins children <> extractMixins args
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ as bs) = extractMixins as <> extractMixins bs
  f (PugList children) = extractMixins children

findMixin :: Text -> [PugMixin] -> Maybe [PugNode]
findMixin name ms = case filter f ms of
  [PugMixinDef' _ nodes] -> Just nodes
  [PugFragmentDef' _ nodes] -> Just nodes
  _ -> Nothing
 where
  f (PugMixinDef' name' _) = name == name'
  f (PugFragmentDef' name' _) = name == name'
  f _ = False
