module Main
  ( main
  ) where

import Options.Applicative qualified as A
import Pughs.Command qualified as Command
import Pughs.Run qualified as Run

--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser Command.parserInfo >>= Run.run
