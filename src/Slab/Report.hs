module Slab.Report
  ( run
  ) where

import Slab.Build qualified as Build
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Syntax qualified as Syntax

--------------------------------------------------------------------------------
run :: FilePath -> IO ()
run srcDir = do
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
-- Similar to Build.buildDir and buildFile, but don't render HTML to disk.

buildDir :: FilePath -> IO [Module]
buildDir srcDir = do
  templates <- Build.listTemplates srcDir
  mapM buildFile templates

buildFile :: FilePath -> IO Module
buildFile path = do
  putStrLn $ "Reading " <> path <> "..."
  nodes <- Evaluate.evaluateFile path >>= Error.unwrap
  pure
    Module
      { modulePath = path
      , moduleNodes = nodes
      }
