{-# LANGUAGE ApplicativeDo #-}

module Pughs.Command where

import Options.Applicative ((<**>))
import Options.Applicative qualified as A

--------------------------------------------------------------------------------
data Command
  = CommandWithPath FilePath CommandWithPath

-- | Commands operating on a path.
data CommandWithPath
  = Render RenderMode
  | Parse
  | Classes -- ^ List the classes used in a template. TODO Later, we want to list (or create a tree) of extends/includes/mixins.

data RenderMode = RenderNormal | RenderPretty

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
    <>  A.command
        "parse"
        ( A.info (parserParse <**> A.helper) $
            A.progDesc
              "Parse a Pug template to AST"
        )
    <>  A.command
        "classes"
        ( A.info (parserClasses <**> A.helper) $
            A.progDesc
              "Parse a Pug template and report its CSS classes"
        )
    )

--------------------------------------------------------------------------------
parserRender :: A.Parser Command
parserRender = do
  mode <- A.flag RenderNormal RenderPretty
    ( A.long "pretty" <> A.help "Use pretty-printing"
    )
  path <- parserTemplatePath
  pure $ CommandWithPath path $ Render mode

parserParse :: A.Parser Command
parserParse = do
  path <- parserTemplatePath
  pure $ CommandWithPath path Parse

parserClasses :: A.Parser Command
parserClasses = do
  path <- parserTemplatePath
  pure $ CommandWithPath path Classes

--------------------------------------------------------------------------------
parserTemplatePath :: A.Parser FilePath
parserTemplatePath =
  A.argument
    A.str
    (A.metavar "FILE" <> A.action "file" <> A.help "Pug template to parse.")
