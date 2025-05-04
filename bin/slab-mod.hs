{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Main
-- Description : Prototype implementation of a module system for Slab
--
-- This is a standalone prototype of the Slab module system.
-- It parses packages and modules, and provides commands for
-- exploring and validating the module graph.
module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative qualified as A
import Protolude hiding (many, moduleName, packageName, some, try)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Pretty.Simple (pPrintNoColor)

--------------------------------------------------------------------------------
-- Main function

-- | Main function
main :: IO ()
main = do
  cmd <- A.execParser parserInfo
  result <- runExceptT $ run cmd
  case result of
    Left err -> do
      putText "Error: "
      print err
      exitFailure
    Right () -> pure ()

--------------------------------------------------------------------------------
-- Command Line Interface

data Command
  = Tree FilePath
  | Validate FilePath
  | ListModules FilePath
  | Resolve FilePath Text Text -- package file, package name, module name
  deriving (Show, Eq)

-- | Parse command line arguments
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info
    (parser <**> A.helper)
    ( A.fullDesc
        <> A.progDesc "Slab module system prototype"
        <> A.header "slab-mod - Prototype implementation of the Slab module system"
    )

parser :: A.Parser Command
parser =
  A.subparser
    ( A.command
        "tree"
        ( A.info
            (Tree <$> parserPackagePath)
            (A.progDesc "Display the package dependency tree")
        )
        <> A.command
          "validate"
          ( A.info
              (Validate <$> parserPackagePath)
              (A.progDesc "Validate all module imports can be resolved")
          )
        <> A.command
          "list-modules"
          ( A.info
              (ListModules <$> parserPackagePath)
              (A.progDesc "List all modules in all packages")
          )
        <> A.command
          "resolve"
          ( A.info
              (Resolve <$> parserPackagePath <*> parserPackageName <*> parserModuleName)
              (A.progDesc "Resolve a module reference")
          )
    )
 where
  parserPackagePath =
    A.strOption
      ( A.long "package"
          <> A.short 'p'
          <> A.metavar "SLAB_JSON"
          <> A.help "Path to slab.json file"
      )
  parserPackageName =
    A.argument
      A.str
      ( A.metavar "PACKAGE"
          <> A.help "Package name"
      )
  parserModuleName =
    A.argument
      A.str
      ( A.metavar "MODULE"
          <> A.help "Module name"
      )

--------------------------------------------------------------------------------

-- | Handle a command
run :: Command -> ExceptT ModuleError IO ()
run (Tree packagePath) = do
  graph <- buildPackageGraph packagePath
  liftIO $ putText $ displayPackageTree graph
run (Validate packagePath) = do
  graph <- buildPackageGraph packagePath
  let issues = validateImports graph
  if null issues
    then liftIO $ putText "All imports can be resolved."
    else do
      liftIO $ putText $ "Found " <> show (length issues) <> " issues:"
      forM_ issues $ \(mod, imp, err) -> liftIO $ do
        putText $ "Error in module " <> modulePackage mod <> ":" <> moduleName mod
        putText $ "  Import: " <> importPackage imp <> " " <> importModule imp
        print err
run (ListModules packagePath) = do
  graph <- buildPackageGraph packagePath
  liftIO $ putText "All modules:"
  forM_ (Map.toList $ modules graph) $ \((pkg, mod), module_) -> do
    liftIO $ putText $ pkg <> ":" <> mod <> " (" <> T.pack (modulePath module_) <> ")"
    liftIO $ putText $ "  Fragments: " <> T.intercalate ", " (map fragmentName $ moduleFragments module_)
    liftIO $
      putText $
        "  Imports: "
          <> T.intercalate
            ", "
            [importPackage i <> " " <> importModule i | i <- moduleImports module_]
    liftIO $ putText ""
run (Resolve packagePath packageName moduleName) = do
  graph <- buildPackageGraph packagePath
  case lookupModule graph packageName moduleName of
    Nothing -> throwE $ ModuleNotFound packageName moduleName
    Just mod -> liftIO $ do
      putText $ "Module resolved: " <> packageName <> ":" <> moduleName
      putText $ "Path: " <> T.pack (modulePath mod)
      putText $ "Fragments: " <> T.intercalate ", " (map fragmentName $ moduleFragments mod)
      putText "Module details:"
      pPrintNoColor mod

--------------------------------------------------------------------------------
-- Types

-- | Represents an error in the module system
data ModuleError
  = PackageNotFound FilePath
  | PackageParseError FilePath Text
  | ModuleNotFound Text Text -- Package name, module name
  | CyclicDependency [Text] -- Cycle in the form of package names
  | ParseError FilePath Text -- File path and error message
  | FileNotFound FilePath
  | OtherError Text
  deriving (Show, Eq)

-- | A package is a collection of modules with a name and dependencies
data Package = Package
  { packageName :: Text
  , packageVersion :: Text
  , packageModules :: Map Text FilePath -- Module name to file path
  , packageDependencies :: Map Text FilePath -- Dependency name to slab.json path
  , packagePath :: FilePath -- Path to the slab.json file
  }
  deriving (Show, Eq)

-- | A module contains fragments and can import other modules
data Module = Module
  { moduleName :: Text
  , modulePackage :: Text
  , modulePath :: FilePath
  , moduleFragments :: [Fragment]
  , moduleImports :: [Import]
  }
  deriving (Show, Eq)

-- | A fragment is a reusable component within a module
data Fragment = Fragment
  { fragmentName :: Text
  , fragmentParams :: [Text]
  , fragmentBody :: Text -- Simplified for prototype
  }
  deriving (Show, Eq)

-- | An import statement referencing another module
data Import = Import
  { importPackage :: Text
  , importModule :: Text
  , importPath :: Text
  , importParams :: Map Text Text -- Parameters passed to the import
  }
  deriving (Show, Eq)

-- | The package graph contains all packages, modules, and their relationships
data PackageGraph = PackageGraph
  { packages :: Map Text Package
  , modules :: Map (Text, Text) Module -- (Package name, Module name) -> Module
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Instances

instance FromJSON Package where
  parseJSON = Aeson.withObject "Package" $ \v -> do
    name <- v .: "name"
    version <- v .: "version"
    modules <- v .: "modules"
    dependencies <- v .:? "dependencies" .!= Map.empty
    return $
      Package
        { packageName = name
        , packageVersion = version
        , packageModules = modules
        , packageDependencies = dependencies
        , packagePath = "" -- Will be set when loading
        }

instance ToJSON Package where
  toJSON Package {..} =
    object
      [ "name" .= packageName
      , "version" .= packageVersion
      , "modules" .= packageModules
      , "dependencies" .= packageDependencies
      ]

--------------------------------------------------------------------------------
-- Package Loading

-- | Load a package from a slab.json file
loadPackage :: FilePath -> ExceptT ModuleError IO Package
loadPackage path = do
  exists <- liftIO $ doesFileExist path
  if not exists
    then throwE $ PackageNotFound path
    else do
      content <- liftIO $ Aeson.eitherDecodeFileStrict path
      case content of
        Left err -> throwE $ PackageParseError path (T.pack err)
        Right pkg -> do
          -- Set the package path and return
          let dir = takeDirectory path
              pkg' = pkg {packagePath = path}
              -- Resolve relative paths in modules
              resolvedModules = Map.map (dir </>) (packageModules pkg')
              -- Resolve relative paths in dependencies
              resolvedDeps = Map.map (dir </>) (packageDependencies pkg')
          return $ pkg' {packageModules = resolvedModules, packageDependencies = resolvedDeps}

-- | Load all packages recursively starting from a root package
loadPackages :: FilePath -> ExceptT ModuleError IO (Map Text Package)
loadPackages rootPackagePath = do
  rootPackage <- loadPackage rootPackagePath
  loadAllDependencies Map.empty [rootPackage]
 where
  loadAllDependencies :: Map Text Package -> [Package] -> ExceptT ModuleError IO (Map Text Package)
  loadAllDependencies acc [] = return acc
  loadAllDependencies acc (pkg : rest) = do
    -- Add current package to accumulator
    let acc' = Map.insert (packageName pkg) pkg acc

    -- Load dependencies not already loaded
    deps <- forM (Map.toList $ packageDependencies pkg) $ \(name, path) ->
      if Map.member name acc'
        then return Nothing -- Already loaded
        else do
          depPkg <- loadPackage path
          -- Check if package name matches dependency name
          when (name /= packageName depPkg) $
            throwE $
              OtherError $
                "Package name mismatch: declared as "
                  <> name
                  <> " but package calls itself "
                  <> packageName depPkg
          return $ Just depPkg

    -- Filter out Nothings and recurse
    let newDeps = catMaybes deps
    loadAllDependencies acc' (rest <> newDeps)

--------------------------------------------------------------------------------
-- Module Parsing

-- | Type for parsing Slab files
type Parser = Parsec Void Text

-- | Parse a Slab file to extract modules and imports
parseSlabFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Module
parseSlabFile filePath content = runParser moduleParser filePath content

-- | Parser for a module
moduleParser :: Parser Module
moduleParser = do
  -- For this prototype, we'll extract:
  -- 1. Module declarations
  -- 2. Fragment definitions
  -- 3. Import statements
  moduleImps <- many (try importParser)
  moduleFrgs <- many (try fragmentParser)

  -- In a real implementation, we'd parse the actual package and module name
  -- from the file or context. For now, we'll use placeholders.
  return
    Module
      { moduleName = "placeholder" -- Will be set when loading into PackageGraph
      , modulePackage = "placeholder" -- Will be set when loading into PackageGraph
      , modulePath = "" -- Will be set when loading into PackageGraph
      , moduleFragments = moduleFrgs
      , moduleImports = moduleImps
      }

-- | Parser for import statements
importParser :: Parser Import
importParser = do
  _ <- space
  _ <- string "import"
  _ <- space1

  -- Parse package name
  pkgName <- some (alphaNumChar <|> char '-')
  _ <- space1

  -- Parse module/path
  modPath <- some (alphaNumChar <|> char '/' <|> char '-' <|> char '_')

  -- Split into module and path
  let (modName, path) = case T.splitOn "/" (T.pack modPath) of
        (m : ps) -> (m, T.intercalate "/" ps)
        [] -> (T.pack modPath, "")

  -- Parse optional parameters
  params <- option Map.empty $ do
    _ <- char '('
    params <- paramParser `sepBy` (char ',' >> space)
    _ <- char ')'
    return $ Map.fromList params

  _ <- many (char ' ' <|> char '\t')
  _ <- optional (char '\n')

  return
    Import
      { importPackage = T.pack pkgName
      , importModule = modName
      , importPath = path
      , importParams = params
      }

-- | Parser for parameters in the form key=value
paramParser :: Parser (Text, Text)
paramParser = do
  _ <- space
  key <- some (alphaNumChar <|> char '_' <|> char '-')
  _ <- space
  _ <- char '='
  _ <- space
  value <- quotedString <|> unquotedString
  _ <- space
  return (T.pack key, value)
 where
  quotedString = do
    _ <- char '\''
    content <- many (noneOf ['\''])
    _ <- char '\''
    return $ T.pack content

  unquotedString = T.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

-- | Parser for fragment definitions
fragmentParser :: Parser Fragment
fragmentParser = do
  _ <- space
  _ <- string "fragment"
  _ <- space1

  -- Parse fragment name
  name <- some (alphaNumChar <|> char '_' <|> char '-')

  -- Parse parameters
  params <- option [] $ do
    _ <- char '('
    params <- some (alphaNumChar <|> char '_' <|> char '-') `sepBy` (char ',' >> space)
    _ <- char ')'
    return $ map T.pack params

  -- For this prototype, we'll just capture the body as a single block of text
  -- In a real implementation, we'd parse the full Slab syntax
  _ <- space
  _ <- many (char ' ' <|> char '\t')
  _ <- char '{'
  body <- many (noneOf ['}'])
  _ <- char '}'

  return
    Fragment
      { fragmentName = T.pack name
      , fragmentParams = params
      , fragmentBody = T.pack body
      }

-- | Load and parse a module from a file
loadModule :: Text -> Text -> FilePath -> ExceptT ModuleError IO Module
loadModule packageName moduleName filePath = do
  exists <- liftIO $ doesFileExist filePath
  if not exists
    then throwE $ FileNotFound filePath
    else do
      content <- liftIO $ T.readFile filePath
      case parseSlabFile filePath content of
        Left err -> throwE $ ParseError filePath (T.pack $ errorBundlePretty err)
        Right mod -> return mod {moduleName = moduleName, modulePackage = packageName, modulePath = filePath}

-- | Load all modules for a package
loadModulesForPackage :: Package -> ExceptT ModuleError IO [Module]
loadModulesForPackage pkg = do
  forM (Map.toList $ packageModules pkg) $ \(modName, filePath) -> do
    loadModule (packageName pkg) modName filePath

--------------------------------------------------------------------------------
-- Build Package Graph

-- | Build a complete package graph from a root package
buildPackageGraph :: FilePath -> ExceptT ModuleError IO PackageGraph
buildPackageGraph rootPackagePath = do
  allPackages <- loadPackages rootPackagePath

  -- Load all modules for all packages
  allModules <- concat <$> forM (Map.elems allPackages) loadModulesForPackage

  -- Create module map
  let moduleMap = Map.fromList [((modulePackage mod, moduleName mod), mod) | mod <- allModules]

  -- Check for cyclic dependencies
  -- (simplified for prototype - would need deeper cycle detection in practice)

  return
    PackageGraph
      { packages = allPackages
      , modules = moduleMap
      }

-- | Detect cycles in the dependency graph
detectCycles :: PackageGraph -> [Text]
detectCycles graph = [] -- TODO

--------------------------------------------------------------------------------
-- Lookup and Resolution

-- | Look up a module in the package graph
lookupModule :: PackageGraph -> Text -> Text -> Maybe Module
lookupModule graph packageName moduleName =
  Map.lookup (packageName, moduleName) (modules graph)

-- | Resolve an import to a module
resolveImport :: PackageGraph -> Import -> Maybe Module
resolveImport graph import_ =
  lookupModule graph (importPackage import_) (importModule import_)

--------------------------------------------------------------------------------
-- Display Functions

-- | Display the package graph as a tree
displayPackageTree :: PackageGraph -> Text
displayPackageTree graph =
  T.unlines $
    concatMap (displayPackage graph) (Map.keys $ packages graph)

-- | Display a package and its modules as a tree
displayPackage :: PackageGraph -> Text -> [Text]
displayPackage graph pkgName =
  case Map.lookup pkgName (packages graph) of
    Nothing -> ["Package not found: " <> pkgName]
    Just pkg ->
      [ pkgName <> " (" <> packageVersion pkg <> ")"
      ]
        <> map (("  ├── " <>) . fst) (Map.toList $ packageModules pkg)
        <> concatMap (displayDependency "  ") (Map.toList $ packageDependencies pkg)

-- | Display a dependency as a tree
displayDependency :: Text -> (Text, FilePath) -> [Text]
displayDependency indent (depName, _) =
  [indent <> "└── " <> depName <> "*"]

-- | Validate that all imports in all modules can be resolved
validateImports :: PackageGraph -> [(Module, Import, ModuleError)]
validateImports graph =
  let allModules = Map.elems (modules graph)
      allImports = [(mod, imp) | mod <- allModules, imp <- moduleImports mod]
   in [ (mod, imp, ModuleNotFound (importPackage imp) (importModule imp))
      | (mod, imp) <- allImports
      , isNothing $ resolveImport graph imp
      ]
