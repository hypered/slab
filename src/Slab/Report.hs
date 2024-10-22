-- |
-- Module      : Slab.Report
-- Description : Report information about Slab templates (mostly empty for now)
--
-- This module serves as a way to explore new Slab features, e.g. creating a
-- module system, or analyzing a growing HTML code base to help refactor it.
module Slab.Report
  ( reportPages
  , reportHeadings
  , reportElement
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..), drawForest)
import Slab.Build qualified as Build
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Render qualified as Render
import Slab.Syntax qualified as Syntax
import System.Exit (exitFailure, exitSuccess)
import Text.Pretty.Simple (pPrintNoColor)

--------------------------------------------------------------------------------
reportPages :: FilePath -> IO ()
reportPages srcDir = do
  modules <- buildDir srcDir
  putStrLn $ "Read " <> show (length modules) <> " modules."
  let pages = filter isPage modules
  putStrLn $ show (length pages) <> " pages."
  mapM_ (putStrLn . modulePath) pages

data Module = Module
  { modulePath :: FilePath
  , moduleNodes :: [Syntax.Block]
  }
  deriving (Show)

isPage :: Module -> Bool
isPage Module {moduleNodes = (x : _)} = Syntax.isDoctype x
isPage _ = False

--------------------------------------------------------------------------------
reportHeadings :: Maybe FilePath -> IO ()
reportHeadings mpath = do
  modl <- buildFile False mpath
  let headings = extractHeadings . Evaluate.simplify $ moduleNodes modl
      f (Heading level _ t) = show level <> " " <> T.unpack t
  putStrLn . drawForest . map (fmap f) $ buildTrees headings

--------------------------------------------------------------------------------
reportElement :: Text -> Maybe FilePath -> IO ()
reportElement i mpath = do
  modl <- buildFile True mpath
  let me = extractElement i . Evaluate.simplify $ moduleNodes modl
  case me of
    Just e -> pPrintNoColor e >> exitSuccess
    Nothing -> exitFailure

--------------------------------------------------------------------------------
-- Similar to Build.buildDir and buildFile, but don't render HTML to disk.
-- TODO Move this code to (and combine it with) with @Slab.Build@.

buildDir :: FilePath -> IO [Module]
buildDir srcDir = do
  templates <- Build.listTemplates srcDir
  mapM (buildFile False . Just) templates

buildFile :: Bool -> Maybe FilePath -> IO Module
buildFile quiet mpath = do
  when (not quiet) $ putStrLn $ "Reading " <> maybe "stdin" id mpath <> "..."
  nodes <- Evaluate.evaluateFile mpath >>= Error.unwrap
  pure
    Module
      { modulePath = maybe "." id mpath
      , moduleNodes = nodes
      }

--------------------------------------------------------------------------------

buildTrees :: [Heading] -> [Tree Heading]
buildTrees [] = []
buildTrees (h : hs) =
  let (children, rest) = span ((> headingLevel h) . headingLevel) hs
      childTrees = buildTrees children
      tree = Node h childTrees
   in tree : buildTrees rest

data Heading = Heading
  { headingLevel :: Int
  , headingId :: Maybe Text
  , headingText :: Text
  }
  deriving (Show, Eq)

extractHeadings :: [Syntax.Block] -> [Heading]
extractHeadings = concatMap f
 where
  f Syntax.BlockDoctype = []
  f (Syntax.BlockElem el _ attrs children) =
    let i = Syntax.idNamesFromAttrs' attrs
        t = Render.extractTexts children
     in case el of
          Syntax.H1 -> [Heading 1 i t]
          Syntax.H2 -> [Heading 2 i t]
          Syntax.H3 -> [Heading 3 i t]
          Syntax.H4 -> [Heading 4 i t]
          Syntax.H5 -> [Heading 5 i t]
          Syntax.H6 -> [Heading 6 i t]
          _ -> extractHeadings children
  f (Syntax.BlockText _ _) = []
  f (Syntax.BlockInclude _ _ children) = maybe [] extractHeadings children
  f (Syntax.BlockFragmentDef _ _ _ _) = []
  f (Syntax.BlockFragmentCall _ _ _ _ children) =
    extractHeadings children
  f (Syntax.BlockFor _ _ _ children) = extractHeadings children
  f (Syntax.BlockComment _ _) = []
  f (Syntax.BlockFilter _ _) = []
  f (Syntax.BlockRawElem _ _) = []
  f (Syntax.BlockDefault _ children) = extractHeadings children
  f (Syntax.BlockImport _ children args) = maybe [] extractHeadings children <> extractHeadings args
  f (Syntax.BlockRun _ _ _) = []
  f (Syntax.BlockAssignVars _) = []
  f (Syntax.BlockIf _ as bs) = extractHeadings as <> extractHeadings bs
  f (Syntax.BlockList children) = extractHeadings children
  f (Syntax.BlockCode _) = []

extractElement :: Text -> [Syntax.Block] -> Maybe Syntax.Block
extractElement i blocks = case go blocks of
  [e] -> Just e
  _ -> Nothing
 where
  go :: [Syntax.Block] -> [Syntax.Block]
  go = concatMap f
  f Syntax.BlockDoctype = []
  f e@(Syntax.BlockElem el _ attrs children)
    | Syntax.idNamesFromAttrs' attrs == Just i = [e]
  f (Syntax.BlockElem el _ attrs children) = go children
  f (Syntax.BlockText _ _) = []
  f (Syntax.BlockInclude _ _ children) = maybe [] go children
  f (Syntax.BlockFragmentDef _ _ _ _) = []
  f (Syntax.BlockFragmentCall _ _ _ _ children) =
    go children
  f (Syntax.BlockFor _ _ _ children) = go children
  f (Syntax.BlockComment _ _) = []
  f (Syntax.BlockFilter _ _) = []
  f (Syntax.BlockRawElem _ _) = []
  f (Syntax.BlockDefault _ children) = go children
  f (Syntax.BlockImport _ children args) = maybe [] go children <> go args
  f (Syntax.BlockRun _ _ _) = []
  f (Syntax.BlockAssignVars _) = []
  f (Syntax.BlockIf _ as bs) = go as <> go bs
  f (Syntax.BlockList children) = go children
  f (Syntax.BlockCode _) = []
