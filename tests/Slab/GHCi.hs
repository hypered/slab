{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module is loaded only in the GHCi session.
module Slab.GHCi
  (
  ) where

import Data.Text qualified as T
import Slab.Command qualified as Command
import Slab.Parse qualified as Parse
import Slab.Render qualified as Render
import Slab.Run qualified as Run
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
