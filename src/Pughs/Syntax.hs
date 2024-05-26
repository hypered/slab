{-# LANGUAGE RecordWildCards #-}

module Pughs.Syntax
  ( PugNode (..)
  , Elem (..)
  , TrailingSym (..)
  , Attr (..)
  , TextSyntax (..)
  , What (..)
  , trailingSym
  , extractClasses
  , extractMixins
  , findMixin
  , extractCombinators
  ) where

import Data.List (nub, sort)
import Data.Text (Text)

--------------------------------------------------------------------------------
data PugNode
  = -- | Only @doctype html@ for now.
    PugDoctype
  | PugElem Elem TrailingSym [Attr] [PugNode]
  | PugText TextSyntax Text
  | -- | Recognize only strings for now.
    PugCode Text
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
  | -- | Whether or not the comment must appear in the output.
    PugComment Bool Text
  | PugFilter Text Text
  | PugRawElem Text [PugNode]
  | PugBlock What Text [PugNode]
  | -- | Similar to PugInclude. The named block arguments are the following nodes.
    -- This is not enforced by the parser.
    PugExtends FilePath (Maybe [PugNode])
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
  | I
  | Svg
  deriving (Show, Eq)

data TrailingSym = HasDot | HasEqual | NoSym
  deriving (Show, Eq)

data Attr = AttrList [(Text, Maybe Text)] | Id Text | Class Text
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data TextSyntax
  = -- | The text follows an element on the same line.
    Normal
  | -- | The text follows a pipe character.
    Pipe
  | -- | The text is part of a text block following a trailing dot.
    Dot
  | -- | The text is the content of an include statement without a .pug extension.
    Include
  deriving (Show, Eq)

-- | Because we use the same syntax to define named blocks, and to pass them as
-- arguments, we want to keep track of whether we're parsing a fragment
-- definition (where we "use" blocks), or fragment calls (where we "pass"
-- block arguments).
data What = WithinDef | WithinCall
  deriving (Show, Eq)

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
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  -- TODO Would be nice to extract classes from verbatim HTML too.
  f (PugRawElem _ _) = []
  f (PugBlock _ _ children) = extractClasses children
  f (PugExtends _ children) = maybe [] extractClasses children

  g (AttrList xs) = concatMap h xs
  g (Id _) = []
  g (Class c) = [c]
  h ("class", Just c) = [c]
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
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugBlock _ _ children) = extractMixins children
  f (PugExtends _ children) = maybe [] extractMixins children

findMixin :: Text -> [PugMixin] -> Maybe [PugNode]
findMixin name ms = case filter f ms of
  [PugMixinDef' _ nodes] -> Just nodes
  [PugFragmentDef' _ nodes] -> Just nodes
  _ -> Nothing
 where
  f (PugMixinDef' name' _) = name == name'
  f (PugFragmentDef' name' _) = name == name'
  f _ = False

-- Extract mixin and fragment definitions, in a single namespace. We don't
-- extract them recursively.
extractCombinators :: [PugNode] -> [(Text, [PugNode])]
extractCombinators = concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ _ _) = []
  f (PugText _ _) = []
  f (PugCode _) = []
  f (PugInclude _ children) = maybe [] extractCombinators children
  f (PugMixinDef name children) = [(name, children)]
  f (PugMixinCall _ _) = []
  f (PugFragmentDef name children) = [(name, children)]
  f (PugFragmentCall _ _) = []
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugBlock _ _ _) = []
  f (PugExtends _ _) = []
