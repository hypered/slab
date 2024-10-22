-- |
-- Module      : Slab.Build
-- Description : Build Slab templates to HTML
--
-- @Slab.Build@ provides types and functions to easily build Slab templates.
-- There are mostly two ways to build templates: by writing the resulting HTML
-- to files, or by writing them to an @STM@-based store.
--
-- Writing to disk is used by the @slab watch@ command. Writing to the @STM@
-- store is used by the @slab serve@ command.
module Slab.Build
  ( buildDir
  , buildFile
  , StmStore
  , buildDirInMemory
  , buildFileInMemory
  , listTemplates
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM qualified as STM
import Data.List (sort)
import Data.Map qualified as M
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Slab.Command qualified as Command
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Execute qualified as Execute
import Slab.Render qualified as Render
import Slab.Syntax qualified as Syntax
import System.Directory (createDirectoryIfMissing)
import System.FilePath (makeRelative, replaceExtension, takeDirectory, (</>))
import System.FilePath.Glob qualified as Glob

--------------------------------------------------------------------------------
buildDir :: FilePath -> Command.RenderMode -> Command.RunMode -> FilePath -> IO ()
buildDir srcDir mode passthrough distDir = do
  templates <- listTemplates srcDir
  mapM_ (buildFile srcDir mode passthrough distDir) templates

buildFile :: FilePath -> Command.RenderMode -> Command.RunMode -> FilePath -> FilePath -> IO ()
buildFile srcDir mode passthrough distDir path = do
  let path' = distDir </> replaceExtension (makeRelative srcDir path) ".html"
      dir' = takeDirectory path'
      ctx = Execute.Context (Just path) passthrough
  putStrLn $ "Building " <> path' <> "..."
  createDirectoryIfMissing True dir'

  nodes <- Execute.executeFile ctx >>= Error.unwrap
  if Evaluate.simplify nodes == []
    then putStrLn $ "No generated content for " <> path
    else case mode of
      Command.RenderNormal ->
        TL.writeFile path' . Render.renderHtmls $ Render.renderBlocks nodes
      Command.RenderPretty ->
        T.writeFile path' . Render.prettyHtmls $ Render.renderBlocks nodes

--------------------------------------------------------------------------------

type Store = M.Map FilePath (Either Error.Error [Syntax.Block])

type StmStore = STM.TVar Store

-- | A version of `buildDir` that doesn't write files to disk, but instead
-- record the generated `Syntax.Block`s in STM.
buildDirInMemory :: FilePath -> Command.RenderMode -> Command.RunMode -> StmStore -> IO ()
buildDirInMemory srcDir mode passthrough store = do
  templates <- listTemplates srcDir
  mapM_ (buildFileInMemory srcDir mode passthrough store) templates

buildFileInMemory :: FilePath -> Command.RenderMode -> Command.RunMode -> StmStore -> FilePath -> IO ()
buildFileInMemory srcDir mode passthrough store path = do
  let path' = replaceExtension (makeRelative srcDir path) ".html"
      ctx = Execute.Context (Just path) passthrough
  putStrLn $ "Building " <> path' <> "..."

  mnodes <- Execute.executeFile ctx
  case mnodes of
    Right nodes ->
      if Evaluate.simplify nodes == []
        then putStrLn $ "No generated content for " <> path
        else case mode of
          Command.RenderNormal ->
            atomically $ STM.modifyTVar store (writeStore path' $ Right nodes)
          Command.RenderPretty ->
            atomically $ STM.modifyTVar store (writeStore path' $ Right nodes)
    Left err -> do
      Error.display err
      atomically $ STM.modifyTVar store (writeStore path' $ Left err)

writeStore :: FilePath -> Either Error.Error [Syntax.Block] -> Store -> Store
writeStore path mblocks = M.insert path mblocks

--------------------------------------------------------------------------------
listTemplates :: FilePath -> IO [FilePath]
listTemplates templatesDir = sort <$> Glob.globDir1 pat templatesDir
 where
  pat = Glob.compile "**/*.slab"
