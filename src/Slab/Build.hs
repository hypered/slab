module Slab.Build
  ( buildDir
  , buildFile
  , listTemplates
  ) where

import Data.List (sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Slab.Command qualified as Command
import Slab.Evaluate qualified as Evaluate
import Slab.Execute qualified as Execute
import Slab.Render qualified as Render
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, replaceExtension, takeDirectory, (</>))
import System.FilePath.Glob qualified as Glob
import Text.Megaparsec hiding (parse)
import Text.Pretty.Simple (pShowNoColor)

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

  evaluated <- Execute.executeFile path
  case evaluated of
    Left (Evaluate.PreProcessParseError err) -> do
      T.putStrLn . T.pack $ errorBundlePretty err
      exitFailure
    Left err -> do
      TL.putStrLn $ pShowNoColor err
      exitFailure
    Right nodes
      | Evaluate.simplify nodes == [] ->
          putStrLn $ "No generated content for " <> path
    Right nodes ->
      case mode of
        Command.RenderNormal ->
          TL.writeFile path' . Render.renderHtmls $ Render.renderBlocks nodes
        Command.RenderPretty ->
          T.writeFile path' . Render.prettyHtmls $ Render.renderBlocks nodes

--------------------------------------------------------------------------------
listTemplates :: FilePath -> IO [FilePath]
listTemplates templatesDir = sort <$> Glob.globDir1 pat templatesDir
 where
  pat = Glob.compile "**/*.slab"
