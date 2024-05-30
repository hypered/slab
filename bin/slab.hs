module Main
  ( main
  ) where

import Options.Applicative qualified as A
import Slab.Command qualified as Command
import Slab.Run qualified as Run

--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser Command.parserInfo >>= Run.run
