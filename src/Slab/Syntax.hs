{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Slab.Syntax
-- Description : The abstract syntax used by Slab
--
-- @Slab.Syntax@ provides data types to represent the syntax used by the Slab
-- language. It also provides small helpers functions to operate on the syntax.
module Slab.Syntax
  ( Block (..)
  , isDoctype
  , pasteBlocks
  , setAttrs
  , setContent
  , addScript
  , CommentType (..)
  , Elem (..)
  , DefinitionUse (..)
  , TrailingSym (..)
  , Attr (..)
  , splitAttrsAndArgs
  , TextSyntax (..)
  , Expr (..)
  , Inline (..)
  , Env (..)
  , emptyEnv
  , displayEnv
  , trailingSym
  , freeVariables
  , thunk
  , extractClasses
  , extractFragments
  , findFragment
  , idNamesFromAttrs
  , idNamesFromAttrs'
  , classNamesFromAttrs
  , namesFromAttrs
  , groupAttrs
  ) where

import Data.List (nub, partition, sort)
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
  | BlockFragmentDef DefinitionUse Text [Text] [Block]
  | BlockFragmentCall Text TrailingSym [Attr] [Expr] [Block]
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
  | -- | Run an external command, with maybe some stdin input.
    BlockRun Text (Maybe Text) (Maybe [Block])
  | BlockAssignVars [(Text, Expr)]
  | BlockIf Expr [Block] [Block]
  | BlockList [Block]
  | BlockCode Expr
  deriving (Show, Eq)

isDoctype :: Block -> Bool
isDoctype BlockDoctype = True
isDoctype _ = False

trailingSym :: Block -> TrailingSym
trailingSym (BlockElem _ sym _ _) = sym
trailingSym (BlockFragmentCall _ sym _ _ _) = sym
trailingSym _ = NoSym

-- | Takes two blocks and returns a BlockList containing both, but peel the
-- outer list of a and b if they are themselves BlockList.
pasteBlocks :: Block -> Block -> Block
pasteBlocks a b = BlockList $ peel a <> peel b
 where
  peel (BlockList xs) = xs
  peel x = [x]

-- | Set attrs on a the first block, if it is a BlockElem.
setAttrs :: [Attr] -> [Block] -> [Block]
setAttrs attrs (BlockElem name mdot attrs' nodes : bs) =
  BlockElem name mdot (attrs' <> attrs) nodes : bs
setAttrs _ bs = bs

-- | Set the content on a block, if it is a BlockElem.
setContent :: [Block] -> Block -> Block
setContent nodes (BlockElem name mdot attrs _) =
  BlockElem name mdot attrs nodes
setContent _ b = b

-- | Find the head element and add a script element at its end.  TODO This
-- doesn't go through all children to find the head. It's best to use
-- "Evaluate.simplify" before using this function.
addScript :: Text -> [Block] -> [Block]
addScript t = map f
 where
  f (BlockElem Head mdot attrs children) = BlockElem Head mdot attrs (children <> [s])
  f (BlockElem name mdot attrs children) = BlockElem name mdot attrs (map f children)
  f block = block
  s = BlockElem Script NoSym [] [BlockText Dot [Lit t]]

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
  | Nav
  | Link
  | A
  | P
  | Em
  | Strong
  | Ul
  | Li
  | Title
  | Table
  | Thead
  | Tbody
  | Tfoot
  | Th
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
  | Circle
  | Line
  | Path
  | Polyline
  | Rect
  | Select
  | Option
  | Textarea
  | Canvas
  | -- | Arbitrary element name, using the @el@ keyword.
    Elem Text
  deriving (Show, Eq)

-- | Specifies if a fragment definition is a normal definition, or one meant to
-- be an argument of a fragment call.
data DefinitionUse = DefinitionNormal | DefinitionArg
  deriving (Show, Eq)

data TrailingSym = HasDot | HasEqual | NoSym
  deriving (Show, Eq)

-- | Represent an attribute or an argument of an element. Attributes can be
-- IDs, classes, or arbitrary keys. Arguments are expressions with no key.
-- The Code must already be evaluated.
data Attr = Id Text | Class Text | Attr Text Expr | Arg Expr
  deriving (Show, Eq)

splitAttrsAndArgs :: [Attr] -> ([Attr], [Expr])
splitAttrsAndArgs = g . partition f
 where
  f = \case
    Id _ -> True
    Class _ -> True
    Attr _ _ -> True
    Arg _ -> False
  g (a, b) = (a, map h b)
  h (Arg e) = e
  h _ = error "Can't happen"

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
  | -- Not really a cons for lists, but instead to add content to an element.
    -- E.g. p : "Hello."
    Cons Expr Expr
  | Block Block
  | -- Expr can be a fragment, so we can manipulate them with code later.
    -- We also capture the current environment.
    Frag [Text] Env [Block]
  | -- Same for Expr instead of Block.
    Thunk Env Expr
  | -- | Allow to assign the content of a JSON file to a variable.
    JsonPath FilePath
  | BuiltIn Text
  | -- | This should probably be separate from "Expr".
    Route [(Text, Expr)]
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

-- Similar to `show`, but makes the environment capture by "Frag" and "Thunk"
-- empty to avoid an infinite data structure.
displayEnv :: Env -> Text
displayEnv = T.pack . show . map (\(a, b) -> (a, f b)) . envVariables
 where
  f = \case
    Frag names _ children -> Frag names emptyEnv children
    Thunk _ expr -> Thunk emptyEnv expr
    expr -> expr

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
  f (BlockFragmentDef _ _ _ _) = [] -- We extract them in BlockFragmentCall instead.
  f (BlockFragmentCall _ _ attrs _ children) = concatMap g attrs <> extractClasses children
  f (BlockFor _ _ _ children) = extractClasses children
  f (BlockComment _ _) = []
  f (BlockFilter _ _) = []
  -- TODO Would be nice to extract classes from verbatim HTML too.
  f (BlockRawElem _ _) = []
  f (BlockDefault _ children) = extractClasses children
  f (BlockImport _ children blocks) = maybe [] extractClasses children <> extractClasses blocks
  f (BlockRun _ _ _) = []
  f (BlockAssignVars _) = []
  f (BlockIf _ as bs) = extractClasses as <> extractClasses bs
  f (BlockList children) = extractClasses children
  f (BlockCode _) = []

  g (Id _) = []
  g (Class c) = [c]
  g (Attr a b) = h a b
  h "class" (SingleQuoteString c) = [c]
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
  f (BlockFragmentDef DefinitionNormal name _ children) = [BlockFragmentDef' name children]
  f (BlockFragmentDef DefinitionArg _ _ _) = []
  f (BlockFragmentCall name _ _ _ children) =
    [BlockFragmentCall' name] <> extractFragments children
  f (BlockFor _ _ _ children) = extractFragments children
  f (BlockComment _ _) = []
  f (BlockFilter _ _) = []
  f (BlockRawElem _ _) = []
  f (BlockDefault _ children) = extractFragments children
  f (BlockImport _ children args) = maybe [] extractFragments children <> extractFragments args
  f (BlockRun _ _ _) = []
  f (BlockAssignVars _) = []
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
  f "id" (SingleQuoteString x) = [x]
  f "id" _ = error "The id is not a string"
  f _ _ = []

idNamesFromAttrs' :: [Attr] -> Maybe Text
idNamesFromAttrs' attrs =
  if idNames == []
    then Nothing
    else Just idNames'
 where
  idNames = idNamesFromAttrs attrs
  -- TODO Refuse multiple Ids in some kind of validation step after parsing ?
  idNames' = T.intercalate "-" idNames

classNamesFromAttrs :: [Attr] -> [Text]
classNamesFromAttrs =
  concatMap
    ( \case
        Id _ -> []
        Class c -> [c]
        Attr a b -> f a b
    )
 where
  f "class" (SingleQuoteString x) = [x]
  f "class" _ = error "The class is not a string"
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
  f a (SingleQuoteString b) = [(a, b)]
  f a (Int b) = [(a, T.pack $ show b)]
  f a (Bool True) = [(a, a)]
  f a (Bool False) = []
  f a (Variable _) = error "The attribute is not evaluated"
  f _ _ = error "The attribute is not a string"

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
  elemAttrs = map (\(a, b) -> Attr a (SingleQuoteString b)) attrs'
