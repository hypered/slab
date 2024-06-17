{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module is loaded only in the GHCi session. It exposes all the Slab
-- modules qualified (similarly to how they are imported throughout the Slab
-- code base).
module Slab.GHCi
  (
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Slab.Build qualified as Build
import Slab.Command qualified as Command
import Slab.Error qualified as Error
import Slab.Evaluate qualified as Evaluate
import Slab.Execute qualified as Execute
import Slab.Generate.Haskell qualified as Generate
import Slab.Parse qualified as Parse
import Slab.Render qualified as Render
import Slab.Report qualified as Report
import Slab.Run qualified as Run
import Slab.Serve qualified as Serve
import Slab.Syntax qualified as Syntax
import Slab.Watch qualified as Watch
import System.Process (callCommand)

-- This import is present to set -interactive-print in ghci.conf.
import Text.Pretty.Simple qualified

--------------------------------------------------------------------------------
-- This is wired to :about.
about :: String -> IO String
about _ = do
  callCommand "clear"
  catFile "scripts/ghci.about.txt"
  pure ""

catFile :: FilePath -> IO ()
catFile filename = do
  content <- readFile filename
  putStrLn content
