{-# LANGUAGE RecordWildCards #-}

module Pughs.Syntax
  ( PugNode (..)
  , Elem (..)
  , TrailingSym (..)
  , Attr (..)
  , TextSyntax (..)
  , What (..)
  , Code (..)
  , Inline (..)
  , trailingSym
  , extractClasses
  , extractMixins
  , findMixin
  , extractCombinators
  , extractAssignments
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Vector qualified as V

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
  | PugBlock What Text [PugNode]
  | -- | Similar to PugInclude. The named block arguments are the following nodes.
    -- This is not enforced by the parser.
    PugExtends FilePath (Maybe [PugNode])
  | -- | Allow to assign the content of a JSON file to a variable. The syntax
    -- is specific to how Struct has a @require@ function in scope.
    PugReadJson Text FilePath (Maybe Aeson.Value)
  | -- | Only support assigning a string for now.
    PugAssignVar Text Text
  | PugIf Code [PugNode] [PugNode]
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

-- Minimal support for some JS expressions.
data Code
  = Variable Text
  | Int Int
  | SingleQuoteString Text
  | List [Code]
  | Object [(Code, Code)]
  | -- The object[key] lookup. This is quite restrive as a start.
    Lookup Text Code
  deriving (Show, Eq)

-- | A representation of a 'Data.Text' template is a list of Inline, supporting
-- efficient rendering. Use 'parse' to create a template from a text containing
-- placeholders.
data Inline = Lit {-# UNPACK #-} !Text | Var {-# UNPACK #-} !Text
  deriving (Eq, Show)

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
  f (PugBlock _ _ children) = extractClasses children
  f (PugExtends _ children) = maybe [] extractClasses children
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ as bs) = extractClasses as <> extractClasses bs

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
  f (PugBlock _ _ children) = extractMixins children
  f (PugExtends _ children) = maybe [] extractMixins children
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ as bs) = extractMixins as <> extractMixins bs

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
  f (PugEach _ _ _ _) = []
  f (PugFragmentDef name children) = [(name, children)]
  f (PugFragmentCall _ _) = []
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugBlock _ _ _) = []
  f (PugExtends _ _) = []
  f (PugReadJson _ _ _) = []
  f (PugAssignVar _ _) = []
  f (PugIf _ _ _) = []

-- Extract variable assignments. We don't extract them recursively.
-- This doens't extract @each@ loops.
extractAssignments :: [PugNode] -> [(Text, Code)]
extractAssignments = concatMap f
 where
  f PugDoctype = []
  f (PugElem _ _ _ _) = []
  f (PugText _ _) = []
  f (PugCode _) = []
  f (PugInclude _ _) = []
  f (PugMixinDef _ _) = []
  f (PugMixinCall _ _) = []
  f (PugEach _ _ _ _) = []
  f (PugFragmentDef _ _) = []
  f (PugFragmentCall _ _) = []
  f (PugComment _ _) = []
  f (PugFilter _ _) = []
  f (PugRawElem _ _) = []
  f (PugBlock _ _ _) = []
  f (PugExtends _ _) = []
  f (PugReadJson name _ (Just val)) = [(name, jsonToCode val)]
  f (PugReadJson _ _ Nothing) = []
  f (PugAssignVar name s) = [(name, SingleQuoteString s)]
  f (PugIf _ _ _) = []

jsonToCode :: Aeson.Value -> Code
jsonToCode = \case
  Aeson.String s -> SingleQuoteString s
  Aeson.Array xs ->
    List $ map jsonToCode (V.toList xs)
  Aeson.Object kvs ->
    let f (k, v) = (SingleQuoteString $ Aeson.Key.toText k, jsonToCode v)
     in Object $ map f (Aeson.KeyMap.toList kvs)
  x -> error $ "jsonToCode: " <> show x
