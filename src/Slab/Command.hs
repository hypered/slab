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
  , RunMode (..)
  , parserInfo
  ) where

import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative ((<**>))
import Options.Applicative qualified as A
import Paths_slab (version)

--------------------------------------------------------------------------------
data Command
  = Build FilePath RenderMode RunMode FilePath
  | Watch FilePath RenderMode RunMode FilePath
  | Serve FilePath FilePath
  | ReportPages FilePath
  | ReportHeadings (Maybe FilePath)
  | -- | Return the content of element matching the provided ID.
    ReportElement Text (Maybe FilePath)
  | -- | Generate code. Only Haskell for now.
    Generate (Maybe FilePath)
  | -- | When the filepath is Nothing, use stdin.
    CommandWithPath (Maybe FilePath) ParseMode CommandWithPath

-- | Commands operating on a path.
data CommandWithPath
  = Render RenderMode RunMode
  | -- | If True, simplify the evaluated AST.
    Execute Bool RunMode
  | -- | If True, simplify the evaluated AST. If a variable name is provided, only output that variable value.
    Evaluate Bool (Maybe Text)
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

data RunMode
  = -- | A failing external command fails the template.
    RunNormal
  | -- | A failing external command doesn't fail the template and its output is
    -- rendered in the template.
    RunPassthrough

--------------------------------------------------------------------------------
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> simpleVersioner ("slab " <> version') <**> A.helper) $
    A.fullDesc
      <> A.header
        ("slab " <> version' <> " - A programmable markup language to generate HTML")
      <> A.progDesc
        "Slab is a programmable markup language to generate HTML."
 where
  version' = showVersion version

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
                "Watch and serve a library of Slab templates"
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
  passthrough <- parserPassthroughFlag
  distDir <-
    A.strOption
      ( A.long "dist"
          <> A.value "./_site"
          <> A.metavar "DIR"
          <> A.help
            "A destination directory for the generated HTML files."
      )
  pure $ Build srcDir mode passthrough distDir

parserServe :: A.Parser Command
parserServe = do
  srcDir <-
    A.argument
      A.str
      (A.metavar "DIR" <> A.action "file" <> A.help "Directory of Slab templates to build.")
  distDir <-
    A.strOption
      ( A.long "dist"
          <> A.value "./_site"
          <> A.metavar "DIR"
          <> A.help
            "A directory with existing static files."
      )
  pure $ Serve srcDir distDir

parserReport :: A.Parser Command
parserReport =
  A.subparser
    ( A.command
        "pages"
        ( A.info (parserReportPages <**> A.helper) $
            A.progDesc
              "Report pages found in a directory"
        )
        <> A.command
          "headings"
          ( A.info (parserReportHeadings <**> A.helper) $
              A.progDesc
                "Report the headings of a page"
          )
        <> A.command
          "element"
          ( A.info (parserReportElement <**> A.helper) $
              A.progDesc
                "Return the content of element matching the provided ID"
          )
    )

parserReportPages :: A.Parser Command
parserReportPages = do
  srcDir <-
    A.argument
      A.str
      (A.metavar "DIR" <> A.action "file" <> A.help "Directory of Slab templates to analyse.")
  pure $ ReportPages srcDir

parserReportHeadings :: A.Parser Command
parserReportHeadings = do
  mpath <- parserTemplatePath
  pure $ ReportHeadings mpath

parserReportElement :: A.Parser Command
parserReportElement = do
  id_ <-
    A.argument
      A.str
      (A.metavar "ID" <> A.help "Element ID to match.")
  mpath <- parserTemplatePath
  pure $ ReportElement id_ mpath

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
  passthrough <- parserPassthroughFlag
  distDir <-
    A.strOption
      ( A.long "dist"
          <> A.value "./_site"
          <> A.metavar "DIR"
          <> A.help
            "A destination directory for the generated HTML files."
      )
  pure $ Watch srcDir mode passthrough distDir

parserExectue :: A.Parser Command
parserExectue = do
  pathAndmode <- parserWithPath
  simpl <-
    A.switch
      ( A.long "simplify" <> A.help "Simplify the AST"
      )
  passthrough <- parserPassthroughFlag
  pure $ uncurry CommandWithPath pathAndmode $ Execute simpl passthrough

parserRender :: A.Parser Command
parserRender = do
  mode <-
    A.flag
      RenderNormal
      RenderPretty
      ( A.long "pretty" <> A.help "Use pretty-printing"
      )
  pathAndmode <- parserWithPath
  passthrough <- parserPassthroughFlag
  pure $ uncurry CommandWithPath pathAndmode $ Render mode passthrough

parserEvaluate :: A.Parser Command
parserEvaluate = do
  pathAndmode <- parserWithPath
  simpl <-
    A.switch
      ( A.long "simplify" <> A.help "Simplify the AST"
      )
  mname <-
    A.optional $
      A.argument
        A.str
        (A.metavar "NAME" <> A.help "Variable name to evaluate.")
  pure $ uncurry CommandWithPath pathAndmode $ Evaluate simpl mname

parserParse :: A.Parser Command
parserParse = do
  pathAndmode <- parserWithPath
  pure $ uncurry CommandWithPath pathAndmode Parse

parserGenerate :: A.Parser Command
parserGenerate = do
  mpath <- parserTemplatePath
  pure $ Generate mpath

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
parserWithPath :: A.Parser (Maybe FilePath, ParseMode)
parserWithPath = (,) <$> parserTemplatePath <*> parserShallowFlag

parserTemplatePath :: A.Parser (Maybe FilePath)
parserTemplatePath = do
  path <- A.argument
    A.str
    (A.metavar "FILE" <> A.action "file" <> A.help "Slab template to parse.")
  pure (
    if path == "-"
    then Nothing
    else Just path
    )

parserShallowFlag :: A.Parser ParseMode
parserShallowFlag =
  A.flag
    ParseDeep
    ParseShallow
    ( A.long "shallow" <> A.help "Don't parse recursively the included Slab files"
    )

--------------------------------------------------------------------------------
parserPassthroughFlag :: A.Parser RunMode
parserPassthroughFlag =
  A.flag
    RunNormal
    RunPassthrough
    ( A.long "passthrough" <> A.help "Allow external command failures"
    )

-- From optparse-applicative ~ 0.18 (we're on 0.17), although we add also `-v`.

-- | A hidden \"--version\" option that displays the version.
--
-- > opts :: ParserInfo Sample
-- > opts = info (sample <**> simpleVersioner "v1.2.3") mempty
simpleVersioner
  :: String
  -- ^ Version string to be shown
  -> A.Parser (a -> a)
simpleVersioner version_ =
  A.infoOption version_ $
    mconcat
      [ A.long "version"
      , A.short 'v'
      , A.help "Show version information"
      , A.hidden
      ]
