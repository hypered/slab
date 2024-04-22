module Pughs.Command where

import Options.Applicative ((<**>))
import Options.Applicative qualified as A

--------------------------------------------------------------------------------
data Command
  = Render

--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper) $
    A.fullDesc
      <> A.header "pughs - parses the Pug syntax"
      <> A.progDesc
        "pughs tries to implement the Pug syntax."

--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
    ( A.command
        "render"
        ( A.info (parserRender <**> A.helper) $
            A.progDesc
              "Render a Pug template to HTML"
        )
    )

--------------------------------------------------------------------------------
parserRender :: A.Parser Command
parserRender = pure Render
