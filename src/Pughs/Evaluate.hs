{-# LANGUAGE RecordWildCards #-}

module Pughs.Evaluate
  ( PreProcessError (..)
  , preProcessPugFile
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Pughs.Parse qualified as Parse
import Pughs.Syntax
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  , ctxNodes :: [PugNode]
  -- ^ Nodes before pre-processing.
  }

data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

-- Similarly to `parsePugFile` but pre-process the include statements.
preProcessPugFile :: FilePath -> IO (Either PreProcessError [PugNode])
preProcessPugFile = runExceptT . preProcessPugFileE

preProcessPugFileE :: FilePath -> ExceptT PreProcessError IO [PugNode]
preProcessPugFileE path = do
  pugContent <- liftIO $ T.readFile path
  let mnodes = first PreProcessParseError $ Parse.parsePug path pugContent
  nodes <- except mnodes
  let ctx =
        Context
          { ctxStartPath = path
          , ctxNodes = nodes
          }
      env = []
  nodes' <- preProcessNodesE ctx nodes
  evaluate ctx {ctxNodes = nodes'} env nodes'

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preProcessNodesE :: Context -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
preProcessNodesE ctx nodes = mapM (preProcessNodeE ctx) nodes

type Env = [(Text, [PugNode])]

-- Process mixin calls. This should be done after processing the include statement
-- since mixins may be defined in included files.
evaluate :: Context -> Env -> [PugNode] -> ExceptT PreProcessError IO [PugNode]
evaluate ctx env nodes = do
  let env' = extractCombinators nodes ++ env
  mapM (eval ctx env') nodes

preProcessNodeE :: Context -> PugNode -> ExceptT PreProcessError IO PugNode
preProcessNodeE ctx@Context {..} = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  node@(PugCode _) -> pure node
  PugInclude path _ -> do
    let includedPath = takeDirectory ctxStartPath </> path
        pugExt = takeExtension includedPath == ".pug"
    exists <- liftIO $ doesFileExist includedPath
    if exists && not pugExt
      then do
        -- Include the file content as-is.
        content <- liftIO $ T.readFile includedPath
        let nodes' = map (PugText Include) $ T.lines content
        pure $ PugInclude path (Just nodes')
      else do
        -- Parse and process the .pug file.
        let includedPath' = if pugExt then includedPath else includedPath <.> ".pug"
        nodes' <- preProcessPugFileE includedPath'
        pure $ PugInclude path (Just nodes')
  PugMixinDef name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugMixinDef name nodes'
  node@(PugMixinCall _ _) -> pure node
  PugFragmentDef name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugFragmentDef name nodes'
  node@(PugFragmentCall _ _) -> pure node
  node@(PugComment _) -> pure node
  node@(PugRawElem _ _) -> pure node
  PugBlock what name nodes -> do
    nodes' <- preProcessNodesE ctx nodes
    pure $ PugBlock what name nodes'

eval :: Context -> Env -> PugNode -> ExceptT PreProcessError IO PugNode
eval ctx env = \case
  node@PugDoctype -> pure node
  PugElem name mdot attrs nodes -> do
    nodes' <- evaluate ctx env nodes
    pure $ PugElem name mdot attrs nodes'
  node@(PugText _ _) -> pure node
  node@(PugCode _) -> pure node
  PugInclude path mnodes -> do
    case mnodes of
      Just nodes -> do
        nodes' <- evaluate ctx env nodes
        pure $ PugInclude path (Just nodes')
      Nothing ->
        pure $ PugInclude path Nothing
  PugMixinDef name nodes -> do
    nodes' <- evaluate ctx env nodes
    pure $ PugMixinDef name nodes'
  PugMixinCall name _ -> do
    case lookup name env of
      Just body ->
        pure $ PugMixinCall name (Just body)
      Nothing -> throwE $ PreProcessError $ "Can't find mixin \"" <> name <> "\""
  PugFragmentDef name nodes -> do
    nodes' <- evaluate ctx env nodes
    pure $ PugFragmentDef name nodes'
  PugFragmentCall name args -> do
    case lookup name env of
      Just body -> do
        -- TODO Either evaluate the args before constructing the env, or capture
        -- the env in a thunk.
        env' <- mapM namedBlock args
        body' <- evaluate ctx (env' ++ env) body
        pure $ PugFragmentCall name body'
      Nothing -> throwE $ PreProcessError $ "Can't find fragment \"" <> name <> "\""
  node@(PugComment _) -> pure node
  node@(PugRawElem _ _) -> pure node
  PugBlock WithinDef name nodes -> do
    -- If the block is not given as an argument, we return the default block,
    -- but recursively trying to replace the blocks found within its own body.
    case lookup name env of
      Nothing -> do
        nodes' <- evaluate ctx env nodes
        pure $ PugBlock WithinDef name nodes'
      Just nodes' -> pure $ PugBlock WithinDef name nodes'
  PugBlock WithinCall name nodes -> do
    nodes' <- evaluate ctx env nodes
    pure $ PugBlock WithinCall name nodes'

namedBlock :: Monad m => PugNode -> ExceptT PreProcessError m (Text, [PugNode])
namedBlock (PugBlock _ name content) = pure (name, content)
namedBlock _ = throwE $ PreProcessError $ "Not a named block argument"
