module Pughs.Build
  ( buildDir
  ) where

import Data.List (sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Pughs.Command qualified as Command
import Pughs.Evaluate qualified as Evaluate
import Pughs.Render qualified as Render
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, replaceExtension, takeDirectory, (</>))
import System.FilePath.Glob qualified as Glob
import Text.Megaparsec hiding (parse)
import Text.Pretty.Simple (pShowNoColor)

--------------------------------------------------------------------------------
buildDir :: FilePath -> Command.RenderMode -> IO ()
buildDir dir mode = do
  templates <- listTemplates dir

  let build path = do
        let path' = "_site" </> replaceExtension (makeRelative dir path) ".html"
            dir' = takeDirectory path'
        putStrLn $ "Building " <> path' <> "..."
        createDirectoryIfMissing True dir'

        evaluated <- Evaluate.evaluatePugFile path
        case evaluated of
          Left (Evaluate.PreProcessParseError err) -> do
            T.putStrLn . T.pack $ errorBundlePretty err
            exitFailure
          Left err -> do
            TL.putStrLn $ pShowNoColor err
            exitFailure
          Right nodes ->
            case mode of
              Command.RenderNormal ->
                TL.writeFile path' . Render.renderHtmls $ Render.pugNodesToHtml nodes
              Command.RenderPretty ->
                T.writeFile path' . Render.prettyHtmls $ Render.pugNodesToHtml nodes

  mapM_ build templates

--------------------------------------------------------------------------------
listTemplates :: FilePath -> IO [FilePath]
listTemplates templatesDir = sort <$> Glob.globDir1 pat templatesDir
 where
  pat = Glob.compile "**/*.pug"
