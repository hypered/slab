module Pughs.Runner
  ( runExamples
  ) where

import Data.List (sort)
import Data.Text (Text)
import Pughs.Run qualified as Run
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
    testGroup "Test suite"
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
  -- `goldenPath` looks like @examples/a.golden@.
  let testName = takeBaseName path
      goldenPath = replaceExtension path ".golden"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO Text
  action = do
    mrenderedHtml <- Run.renderPretty path
    either pure pure mrenderedHtml

  convert = id
