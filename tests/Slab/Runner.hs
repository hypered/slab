module Slab.Runner
  ( runExamples
  ) where

import Data.List (sort)
import Data.Text (Text)
import Slab.Evaluate qualified as Evaluate
import Slab.Render qualified as Render
import System.FilePath
import System.FilePath.Glob qualified as Glob
import Test.Tasty
import Test.Tasty.Silver qualified as Silver

--------------------------------------------------------------------------------
runExamples :: IO ()
runExamples = do
  -- List all examples, comparing them to their corresponding golden files.
  goldens <- listExamples "examples/" >>= mapM mkGoldenTest
  -- Examples coming from
  -- https://github.com/pugjs/pug/tree/master/packages/pug/test/cases.
  cases <- listExamples "examples/cases/" >>= mapM mkGoldenTest
  -- Examples coming from the documentation.
  docs <- listExamples "examples/docs/" >>= mapM mkGoldenTest
  defaultMain $
    testGroup
      "Test suite"
      [ testGroup "Examples" goldens
      , testGroup "Cases" cases
      , testGroup "Documentation" docs
      ]

--------------------------------------------------------------------------------
listExamples :: FilePath -> IO [FilePath]
listExamples examplesDir = sort <$> Glob.globDir1 pat examplesDir
 where
  pat = Glob.compile "*.slab"

--------------------------------------------------------------------------------
mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  -- `path` looks like @examples/a.slab@.
  -- `testName` looks like @a@.
  -- `goldenPath` looks like @examples/a.html@.
  let testName = takeBaseName path
      goldenPath = replaceExtension path ".html"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO Text
  action = do
    evaluated <- Evaluate.evaluateFile path
    case evaluated of
      Left _ -> pure "TODO"
      Right nodes -> do
        let output = Render.prettyHtmls . Render.renderBlocks $ nodes
        pure output

  convert = id
