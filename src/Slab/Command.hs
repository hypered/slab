{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Slab.Command
-- Description : Command-line interface to Slab
--
-- @Slab.Command@ provides a command line interface for the Slab program.
--
-- Commands and options are defined by parsers written using the
-- @optparse-applicative@ library.
--
-- The implementation of each command can be found in the "Slab.Run" module.
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
  | Watch FilePath RenderMode FilePath
  | Serve FilePath
  | Report FilePath
  | -- | Generate code. Only Haskell for now.
    Generate FilePath
  | CommandWithPath FilePath ParseMode CommandWithPath

-- | Commands operating on a path.
data CommandWithPath
  = Render RenderMode
  | Execute
  | -- | If True, simplify the evaluated AST.
    Evaluate Bool
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
              "Build a library of Slab templates to HTML"
        )
        <> A.command
          "watch"
          ( A.info (parserWatch <**> A.helper) $
              A.progDesc
                "Watch and build a library of Slab templates to HTML"
          )
        <> A.command
          "serve"
          ( A.info (parserServe <**> A.helper) $
              A.progDesc
                "Watch and serve a library of Slab templates to HTML"
          )
        <> A.command
          "report"
          ( A.info (parserReport <**> A.helper) $
              A.progDesc
                "Analyse a library of Slab templates"
          )
        <> A.command
          "render"
          ( A.info (parserRender <**> A.helper) $
              A.progDesc
                "Render a Slab template to HTML"
          )
        <> A.command
          "run"
          ( A.info (parserExectue <**> A.helper) $
              A.progDesc
                "Execute a Slab template"
          )
        <> A.command
          "evaluate"
          ( A.info (parserEvaluate <**> A.helper) $
              A.progDesc
                "Evaluate a Slab template"
          )
        <> A.command
          "parse"
          ( A.info (parserParse <**> A.helper) $
              A.progDesc
                "Parse a Slab template to AST"
          )
        <> A.command
          "generate"
          ( A.info (parserGenerate <**> A.helper) $
              A.progDesc
                "Generate code corresponding to a Slab template"
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

parserServe :: A.Parser Command
parserServe = do
  distDir <-
    A.strOption
      ( A.long "dist"
          <> A.value "./_site"
          <> A.metavar "DIR"
          <> A.help
            "A destination directory for the generated HTML files."
      )
  pure $ Serve distDir

parserReport :: A.Parser Command
parserReport = do
  srcDir <-
    A.argument
      A.str
      (A.metavar "DIR" <> A.action "file" <> A.help "Directory of Slab templates to analyse.")
  pure $ Report srcDir

parserWatch :: A.Parser Command
parserWatch = do
  srcDir <-
    A.argument
      A.str
      (A.metavar "DIR" <> A.action "file" <> A.help "Directory of Slab templates to watch.")
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
  pure $ Watch srcDir mode distDir

parserExectue :: A.Parser Command
parserExectue = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode $ Execute

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
  simpl <-
    A.switch
      ( A.long "simplify" <> A.help "Simplify the AST"
      )
  pure $ uncurry CommandWithPath pathAndmode $ Evaluate simpl

parserParse :: A.Parser Command
parserParse = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode Parse

parserGenerate :: A.Parser Command
parserGenerate = do
  path <- parserTemplatePath
  pure $ Generate path

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
