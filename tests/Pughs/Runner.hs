module Pughs.Runner
  ( runExamples
  ) where

import Data.List (sort)
import Data.Text (Text)
import Pughs.Evaluate qualified as Evaluate
import Pughs.Render qualified as Render
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
  defaultMain $
    testGroup
      "Test suite"
      [ testGroup "Examples" goldens
      , testGroup "Cases" cases
      ]

--------------------------------------------------------------------------------
listExamples :: FilePath -> IO [FilePath]
listExamples examplesDir = sort <$> Glob.globDir1 pat examplesDir
 where
  pat = Glob.compile "*.pug"

--------------------------------------------------------------------------------
mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  -- `path` looks like @examples/a.pug@.
  -- `testName` looks like @a@.
  -- `goldenPath` looks like @examples/a.html@.
  let testName = takeBaseName path
      goldenPath = replaceExtension path ".html"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO Text
  action = do
    evaluated <- Evaluate.evaluatePugFile path
    case evaluated of
      Left _ -> pure "TODO"
      Right nodes -> do
        let output = Render.prettyHtmls . Render.pugNodesToHtml $ nodes
        pure output

  convert = id
