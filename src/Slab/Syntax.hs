{-# LANGUAGE RecordWildCards #-}

module Slab.Syntax
  ( Block (..)
  , isDoctype
  , pasteBlocks
  , setAttrs
  , CommentType (..)
  , Elem (..)
  , TrailingSym (..)
  , Attr (..)
  , TextSyntax (..)
  , Expr (..)
  , Inline (..)
  , Env (..)
  , emptyEnv
  , trailingSym
  , freeVariables
  , thunk
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
  | BlockElem Elem TrailingSym [Attr] [Block]
  | BlockText TextSyntax [Inline]
  | -- | @Nothing@ when the template is parsed, then @Just nodes@ after
    -- preprocessing (i.e. actually running the include statement).
    -- The filter name follows the same behavior as BlockFilter.
    BlockInclude (Maybe Text) FilePath (Maybe [Block])
  | -- | This doesn't exist in Pug. This is like a mixin than receive block arguments.
    -- Or like a parent template that can be @extended@ by a child template.
    BlockFragmentDef Text [Text] [Block]
  | BlockFragmentCall Text [Attr] [Expr] [Block]
  | BlockFor Text (Maybe Text) Expr [Block]
  | -- TODO Should we allow string interpolation here ?
    BlockComment CommentType Text
  | BlockFilter Text Text
  | BlockRawElem Text [Block]
  | -- | @default@ defines an optional formal parameter with a default content.
    -- Its content is used when the argument is not given.
    BlockDefault Text [Block]
  | -- | Similar to an anonymous fragment call, where the fragment body is the
    -- content of the referenced file.
    BlockImport FilePath (Maybe [Block]) [Block]
  | BlockRun Text (Maybe [Block])
  | -- | Allow to assign the content of a JSON file to a variable. The syntax
    -- is specific to how Struct has a @require@ function in scope.
    BlockReadJson Text FilePath (Maybe Aeson.Value)
  | BlockAssignVar Text Expr
  | BlockIf Expr [Block] [Block]
  | BlockList [Block]
  | BlockCode Expr
  deriving (Show, Eq)

isDoctype :: Block -> Bool
isDoctype BlockDoctype = True
isDoctype _ = False

trailingSym :: Block -> TrailingSym
trailingSym (BlockElem _ sym _ _) = sym
trailingSym _ = NoSym

-- | Takes two blocks and returns a BlockList containing both, but peel the
-- outer list of a and b if they are themselves BlockList.
pasteBlocks :: Block -> Block -> Block
pasteBlocks a b = BlockList $ peel a <> peel b
 where
  peel (BlockList xs) = xs
  peel x = [x]

-- | Set attrs on a the block, if it is a BlockElem.
setAttrs attrs (BlockElem name mdot attrs' nodes : bs) =
  BlockElem name mdot (attrs' <> attrs) nodes : bs
setAttrs _ bs = bs

-- | A "passthrough" comment will be included in the generated HTML.
data CommentType = NormalComment | PassthroughComment
  deriving (Show, Eq)

data Elem
  = Html
  | Body
  | Div
  | Span
  | Br
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
  | -- | Arbitrary element name, using the @el@ keyword.
    Elem Text
  deriving (Show, Eq)

data TrailingSym = HasDot | HasEqual | NoSym
  deriving (Show, Eq)

-- The Code must already be evaluated.
data Attr = Id Text | Class Text | Attr Text (Maybe Expr)
  deriving (Show, Eq)

-- Tracks the syntax used to enter the text.
data TextSyntax
  = -- | The text follows an element on the same line.
    Normal
  | -- | The text follows a pipe character. Multiple lines each introduced by a
    -- pipe symbol are grouped as a single 'BlockText' node.
    Pipe
  | -- | The text is part of a text block following a trailing dot.
    Dot
  | -- | The text is the content of an include statement without a .slab extension.
    Include
  | -- | The text is the output of command.
    RunOutput
  deriving (Show, Eq)

-- | Simple expression language.
data Expr
  = Variable Text
  | Bool Bool
  | Int Int
  | SingleQuoteString Text
  | List [Expr]
  | Object [(Expr, Expr)]
  | -- The object[key] lookup. This is quite restrive as a start.
    Lookup Text Expr
  | Application Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Times Expr Expr
  | Divide Expr Expr
  | GreaterThan Expr Expr
  | LesserThan Expr Expr
  | Equal Expr Expr
  | Block Block
  | -- Expr can be a fragment, so we can manipulate them with code later.
    -- We also capture the current environment.
    Frag [Text] Env [Block]
  | -- Same for Expr instead of Block.
    Thunk Env Expr
  | BuiltIn Text
  deriving (Show, Eq)

-- | A representation of a 'Data.Text' template is a list of Inline, supporting
-- efficient rendering. Use 'parse' to create a template from a text containing
-- placeholders. 'Lit' is a literal Text value. 'Place' is a placeholder created
-- with @#{...}@.
data Inline = Lit {-# UNPACK #-} !Text | Place !Expr
  deriving (Eq, Show)

data Env = Env
  { envVariables :: [(Text, Expr)]
  }
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env []

--------------------------------------------------------------------------------
freeVariables :: Expr -> [Text]
freeVariables =
  nub . \case
    Variable a -> [a]
    Bool _ -> []
    Int _ -> []
    SingleQuoteString _ -> []
    List as -> concatMap freeVariables as
    Object _ -> [] -- TODO I guess some of those can contain variables.
    Lookup a b -> a : freeVariables b
    Add a b -> freeVariables a <> freeVariables b
    Sub a b -> freeVariables a <> freeVariables b
    Times a b -> freeVariables a <> freeVariables b
    Divide a b -> freeVariables a <> freeVariables b
    Frag _ _ _ -> []
    Thunk _ _ -> []

-- Capture an environment, but limit its content to only the free variables of
-- the expression.
thunk :: Env -> Expr -> Expr
thunk Env {..} code = Thunk env code
 where
  env = Env $ filter ((`elem` frees) . fst) envVariables
  frees = freeVariables code

--------------------------------------------------------------------------------

extractClasses :: [Block] -> [Text]
extractClasses = nub . sort . concatMap f
 where
  f BlockDoctype = []
  f (BlockElem _ _ attrs children) = concatMap g attrs <> extractClasses children
  f (BlockText _ _) = []
  f (BlockInclude _ _ children) = maybe [] extractClasses children
  f (BlockFragmentDef _ _ _) = [] -- We extract them in BlockFragmentCall instead.
  f (BlockFragmentCall _ attrs _ children) = concatMap g attrs <> extractClasses children
  f (BlockFor _ _ _ children) = extractClasses children
  f (BlockComment _ _) = []
  f (BlockFilter _ _) = []
  -- TODO Would be nice to extract classes from verbatim HTML too.
  f (BlockRawElem _ _) = []
  f (BlockDefault _ children) = extractClasses children
  f (BlockImport _ children blocks) = maybe [] extractClasses children <> extractClasses blocks
  f (BlockRun _ _) = []
  f (BlockReadJson _ _ _) = []
  f (BlockAssignVar _ _) = []
  f (BlockIf _ as bs) = extractClasses as <> extractClasses bs
  f (BlockList children) = extractClasses children
  f (BlockCode _) = []

  g (Id _) = []
  g (Class c) = [c]
  g (Attr a b) = h a b
  h "class" (Just (SingleQuoteString c)) = [c]
  h "class" _ = error "The class is not a string"
  h _ _ = []

-- Return type used for `extractFragments`.
data BlockFragment
  = BlockFragmentDef' Text [Block]
  | BlockFragmentCall' Text
  deriving (Show, Eq)

extractFragments :: [Block] -> [BlockFragment]
extractFragments = concatMap f
 where
  f BlockDoctype = []
  f (BlockElem _ _ _ children) = extractFragments children
  f (BlockText _ _) = []
  f (BlockInclude _ _ children) = maybe [] extractFragments children
  f (BlockFragmentDef name _ children) = [BlockFragmentDef' name children]
  f (BlockFragmentCall name _ _ children) = [BlockFragmentCall' name] <> extractFragments children
  f (BlockFor _ _ _ children) = extractFragments children
  f (BlockComment _ _) = []
  f (BlockFilter _ _) = []
  f (BlockRawElem _ _) = []
  f (BlockDefault _ children) = extractFragments children
  f (BlockImport _ children args) = maybe [] extractFragments children <> extractFragments args
  f (BlockRun _ _) = []
  f (BlockReadJson _ _ _) = []
  f (BlockAssignVar _ _) = []
  f (BlockIf _ as bs) = extractFragments as <> extractFragments bs
  f (BlockList children) = extractFragments children
  f (BlockCode _) = []

findFragment :: Text -> [BlockFragment] -> Maybe [Block]
findFragment name ms = case filter f ms of
  [BlockFragmentDef' _ nodes] -> Just nodes
  _ -> Nothing
 where
  f (BlockFragmentDef' name' _) = name == name'
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
