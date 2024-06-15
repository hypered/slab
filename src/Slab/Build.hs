module Slab.Build
  ( buildDir
  , buildFile
  , listTemplates
  ) where

import Data.List (sort)
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Slab.Command qualified as Command
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Execute qualified as Execute
import Slab.Render qualified as Render
import System.Directory (createDirectoryIfMissing)
import System.FilePath (makeRelative, replaceExtension, takeDirectory, (</>))
import System.FilePath.Glob qualified as Glob

--------------------------------------------------------------------------------
buildDir :: FilePath -> Command.RenderMode -> FilePath -> IO ()
buildDir srcDir mode distDir = do
  templates <- listTemplates srcDir
  mapM_ (buildFile srcDir mode distDir) templates

buildFile :: FilePath -> Command.RenderMode -> FilePath -> FilePath -> IO ()
buildFile srcDir mode distDir path = do
  let path' = distDir </> replaceExtension (makeRelative srcDir path) ".html"
      dir' = takeDirectory path'
  putStrLn $ "Building " <> path' <> "..."
  createDirectoryIfMissing True dir'

  nodes <- Execute.executeFile path >>= Error.unwrap
  if Evaluate.simplify nodes == []
    then putStrLn $ "No generated content for " <> path
    else case mode of
      Command.RenderNormal ->
        TL.writeFile path' . Render.renderHtmls $ Render.renderBlocks nodes
      Command.RenderPretty ->
        T.writeFile path' . Render.prettyHtmls $ Render.renderBlocks nodes

--------------------------------------------------------------------------------
listTemplates :: FilePath -> IO [FilePath]
listTemplates templatesDir = sort <$> Glob.globDir1 pat templatesDir
 where
  pat = Glob.compile "**/*.slab"
