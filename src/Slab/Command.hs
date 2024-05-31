{-# LANGUAGE ApplicativeDo #-}

module Slab.Command
  ( Command (..)
  , CommandWithPath (..)
  , RenderMode (..)
  , ParseMode (..)
  , parserInfo
  ) where

import Data.Text (Text)
import Options.Applicative ((<**>))
import Options.Applicative qualified as A

--------------------------------------------------------------------------------
data Command
  = Build FilePath RenderMode FilePath
  | CommandWithPath FilePath ParseMode CommandWithPath

-- | Commands operating on a path.
data CommandWithPath
  = Render RenderMode
  | Evaluate
  | Parse
  | -- | List the classes used in a template. TODO Later, we want to list (or create a tree) of extends/includes.
    Classes
  | -- | List the fragments used in a template. If a name is given, extract that definition.
    Fragments (Maybe Text)

data RenderMode = RenderNormal | RenderPretty

data ParseMode
  = -- | Don't process include statements.
    ParseShallow
  | -- | Process the include statements, creating a complete template.
    ParseDeep

--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper) $
    A.fullDesc
      <> A.header "slab - A programmable markup language to generate HTML"
      <> A.progDesc
        "Slab is a programmable markup language to generate HTML."

--------------------------------------------------------------------------------
parser :: A.Parser Command
parser =
  A.subparser
    ( A.command
        "build"
        ( A.info (parserBuild <**> A.helper) $
            A.progDesc
              "Build a library of Slab templates"
        )
        <> A.command
          "render"
          ( A.info (parserRender <**> A.helper) $
              A.progDesc
                "Render a Slab template to HTML"
          )
        <> A.command
          "eval"
          ( A.info (parserEvaluate <**> A.helper) $
              A.progDesc
                "Parse a Slab template to AST and evaluate it"
          )
        <> A.command
          "parse"
          ( A.info (parserParse <**> A.helper) $
              A.progDesc
                "Parse a Slab template to AST"
          )
        <> A.command
          "classes"
          ( A.info (parserClasses <**> A.helper) $
              A.progDesc
                "Parse a Slab template and report its CSS classes"
          )
        <> A.command
          "fragments"
          ( A.info (parserFragments <**> A.helper) $
              A.progDesc
                "Parse a Slab template and report its fragments"
          )
    )

--------------------------------------------------------------------------------
parserBuild :: A.Parser Command
parserBuild = do
  srcDir <-
    A.argument
      A.str
      (A.metavar "DIR" <> A.action "file" <> A.help "Directory of Slab templates to build.")
  mode <-
    A.flag
      RenderNormal
      RenderPretty
      ( A.long "pretty" <> A.help "Use pretty-printing"
      )
  distDir <-
    A.strOption
      ( A.long "dist"
          <> A.value "./_site"
          <> A.metavar "DIR"
          <> A.help
            "A destination directory for the generated HTML files."
      )
  pure $ Build srcDir mode distDir

parserRender :: A.Parser Command
parserRender = do
  mode <-
    A.flag
      RenderNormal
      RenderPretty
      ( A.long "pretty" <> A.help "Use pretty-printing"
      )
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode $ Render mode

parserEvaluate :: A.Parser Command
parserEvaluate = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode Evaluate

parserParse :: A.Parser Command
parserParse = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode Parse

parserClasses :: A.Parser Command
parserClasses = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode Classes

parserFragments :: A.Parser Command
parserFragments = do
  pathAndmode <- parserWithPath
  mname <-
    A.optional $
      A.argument
        A.str
        (A.metavar "NAME" <> A.help "Fragment name to extract.")
  pure $ uncurry CommandWithPath pathAndmode $ Fragments mname

--------------------------------------------------------------------------------
parserWithPath :: A.Parser (FilePath, ParseMode)
parserWithPath = (,) <$> parserTemplatePath <*> parserShallowFlag

parserTemplatePath :: A.Parser FilePath
parserTemplatePath =
  A.argument
    A.str
    (A.metavar "FILE" <> A.action "file" <> A.help "Slab template to parse.")

parserShallowFlag :: A.Parser ParseMode
parserShallowFlag =
  A.flag
    ParseDeep
    ParseShallow
    ( A.long "shallow" <> A.help "Don't parse recursively the included Slab files"
    )
