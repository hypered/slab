-- |
-- Module      : Slab.Watch
-- Description : Continuously build a set of Slab templates
--
-- @Slab.Watch@ watches a set of Slab templates, continuously rebuilding them
-- as they change.
module Slab.Watch
  ( run
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Directory (canonicalizePath)
import System.FSNotify
import System.FilePath (makeRelative, takeExtension, (</>))

--------------------------------------------------------------------------------
run :: FilePath -> (FilePath -> IO ()) -> IO ()
run srcDir update = do
  putStrLn "Watching..."
  withManager $ \mgr -> do
    -- Start a watching job in the background.
    -- TODO We should debounce multiple Modified events occurring in a short
    -- amount of time on the same file. Typically saving a file with Vim will
    -- trigger two Modified events on a .slab file.
    _ <-
      watchTree
        mgr
        srcDir
        ( \e -> do
            case e of
              Modified path _ _ | takeExtension path `elem` [".slab", ".pikchr"] -> True
              _ -> False
        )
        ( \e -> do
            srcDir' <- canonicalizePath srcDir
            let path = srcDir </> makeRelative srcDir' (eventPath e)
            update path
        )

    -- Sleep forever (until interrupted).
    forever $ threadDelay 1000000
