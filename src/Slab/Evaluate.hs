{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Slab.Evaluate
  ( PreProcessError (..)
  , preprocessFile
  , evaluateFile
  , evaluate
  , defaultEnv
  , simplify
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import Slab.Parse qualified as Parse
import Slab.Syntax
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.Megaparsec hiding (Label, label, parse, parseErrorPretty, unexpected)

--------------------------------------------------------------------------------
data Context = Context
  { ctxStartPath :: FilePath
  }

data PreProcessError
  = PreProcessParseError (ParseErrorBundle Text Void)
  | PreProcessError Text -- TODO Add specific variants instead of using Text.
  deriving (Show, Eq)

-- | Similarly to `parseFile` but pre-process the include statements.
preprocessFile :: FilePath -> IO (Either PreProcessError [Block])
preprocessFile = runExceptT . preprocessFileE

preprocessFileE :: FilePath -> ExceptT PreProcessError IO [Block]
preprocessFileE path = do
  pugContent <- liftIO $ T.readFile path
  let mnodes = first PreProcessParseError $ Parse.parse path pugContent
  nodes <- except mnodes
  let ctx =
        Context
          { ctxStartPath = path
          }
  preprocessNodesE ctx nodes

-- Process include statements (i.e. read the given path and parse its content
-- recursively).
preprocessNodesE :: Context -> [Block] -> ExceptT PreProcessError IO [Block]
preprocessNodesE ctx nodes = mapM (preprocessNodeE ctx) nodes

evaluateFile :: FilePath -> IO (Either PreProcessError [Block])
evaluateFile path = runExceptT (preprocessFileE path >>= evaluate defaultEnv ["toplevel"])

-- Process mixin calls. This should be done after processing the include statement
-- since mixins may be defined in included files.
evaluate :: Monad m => Env -> [Text] -> [Block] -> ExceptT PreProcessError m [Block]
evaluate env stack nodes = do
  -- Note that we pass the environment that we are constructing, so that each
  -- definition sees all definitions (including later ones and itself).
  let vars = extractVariables env' nodes
      env' = augmentVariables env vars
  mapM (eval env' stack) nodes

preprocessNodeE :: Context -> Block -> ExceptT PreProcessError IO Block
preprocessNodeE ctx@Context {..} = \case
  node@BlockDoctype -> pure node
  BlockElem name mdot attrs nodes -> do
    nodes' <- preprocessNodesE ctx nodes
    pure $ BlockElem name mdot attrs nodes'
  node@(BlockText _ _) -> pure node
  BlockInclude mname path _ -> do
    let includedPath = takeDirectory ctxStartPath </> path
        slabExt = takeExtension includedPath == ".slab"
    exists <- liftIO $ doesFileExist includedPath
    if
        | exists && (not slabExt || mname == Just "escape-html") -> do
            -- Include the file content as-is.
            content <- liftIO $ T.readFile includedPath
            let node = Parse.pugTextInclude content
            pure $ BlockInclude mname path (Just [node])
        | exists -> do
            -- Parse and process the .slab file.
            nodes' <- preprocessFileE includedPath
            pure $ BlockInclude mname path (Just nodes')
        | otherwise ->
            throwE $ PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  BlockFragmentDef name params nodes -> do
    nodes' <- preprocessNodesE ctx nodes
    pure $ BlockFragmentDef name params nodes'
  BlockFragmentCall name values nodes -> do
    nodes' <- preprocessNodesE ctx nodes
    pure $ BlockFragmentCall name values nodes'
  node@(BlockFor _ _ _ _) -> pure node
  node@(BlockComment _ _) -> pure node
  node@(BlockFilter _ _) -> pure node
  node@(BlockRawElem _ _) -> pure node
  BlockDefault name nodes -> do
    nodes' <- preprocessNodesE ctx nodes
    pure $ BlockDefault name nodes'
  BlockImport path _ args -> do
    -- An import is treated like an include used to define a fragment, then
    -- directly calling that fragment.
    let includedPath = takeDirectory ctxStartPath </> path
        slabExt = takeExtension includedPath == ".slab"
    exists <- liftIO $ doesFileExist includedPath
    if
        | exists && not slabExt ->
            throwE $ PreProcessError $ "Extends requires a .slab file"
        | exists -> do
            -- Parse and process the .slab file.
            body <- preprocessFileE includedPath
            args' <- mapM (preprocessNodeE ctx) args
            pure $ BlockImport path (Just body) args'
        | otherwise ->
            throwE $ PreProcessError $ "File " <> T.pack includedPath <> " doesn't exist"
  BlockReadJson name path _ -> do
    let path' = takeDirectory ctxStartPath </> path
    content <- liftIO $ BL.readFile path'
    case Aeson.eitherDecode content of
      Right val ->
        pure $ BlockReadJson name path $ Just val
      Left err ->
        throwE $ PreProcessError $ "Can't decode JSON: " <> T.pack err
  node@(BlockAssignVar _ _) -> pure node
  BlockIf cond as bs -> do
    -- File inclusion is done right away, without checking the condition.
    as' <- preprocessNodesE ctx as
    bs' <- preprocessNodesE ctx bs
    pure $ BlockIf cond as' bs'
  BlockList nodes -> do
    nodes' <- preprocessNodesE ctx nodes
    pure $ BlockList nodes'
  node@(BlockCode _) -> pure node

eval :: Monad m => Env -> [Text] -> Block -> ExceptT PreProcessError m Block
eval env stack = \case
  node@BlockDoctype -> pure node
  BlockElem name mdot attrs nodes -> do
    nodes' <- evaluate env stack nodes
    pure $ BlockElem name mdot attrs nodes'
  BlockText syn template -> do
    template' <- evalTemplate env template
    pure $ BlockText syn template'
  BlockInclude mname path mnodes -> do
    case mnodes of
      Just nodes -> do
        nodes' <- evaluate env ("include" : stack) nodes
        pure $ BlockInclude mname path (Just nodes')
      Nothing ->
        pure $ BlockInclude mname path Nothing
  node@(BlockFragmentDef _ _ _) -> pure node
  BlockFragmentCall name values args -> do
    body <- call env stack name values args
    pure $ BlockFragmentCall name values body
  BlockFor name mindex values nodes -> do
    -- Re-use BlockFor to construct a single node to return.
    let zero :: Int
        zero = 0
    values' <- evalCode env values
    collection <- case values' of
      List xs -> pure $ zip xs $ map Int [zero ..]
      Object xs -> pure $ map (\(k, v) -> (v, k)) xs
      _ -> throwE $ PreProcessError $ "Iterating on something that is not a collection"
    nodes' <- forM collection $ \(value, index) -> do
      let env' = case mindex of
            Just idxname -> augmentVariables env [(name, value), (idxname, index)]
            Nothing -> augmentVariables env [(name, value)]
      evaluate env' ("each" : stack) nodes
    pure $ BlockFor name mindex values $ concat nodes'
  node@(BlockComment _ _) -> pure node
  node@(BlockFilter _ _) -> pure node
  node@(BlockRawElem _ _) -> pure node
  BlockDefault name nodes -> do
    -- If the fragment is not given as an argument, we return the default block,
    -- but recursively trying to replace the blocks found within its own body.
    case lookupVariable name env of
      Nothing -> do
        nodes' <- evaluate env ("?block" : stack) nodes
        pure $ BlockDefault name nodes'
      Just (Frag _ capturedEnv nodes') -> do
        nodes'' <- evaluate capturedEnv ("+block" : stack) nodes'
        pure $ BlockDefault name nodes''
      Just _ -> throwE $ PreProcessError $ "Calling something that is not a fragment \"" <> name <> "\" in " <> T.pack (show stack)
  BlockImport path _ args -> do
    body <- call env stack (T.pack path) [] args
    pure $ BlockImport path (Just body) args
  node@(BlockReadJson _ _ _) -> pure node
  node@(BlockAssignVar _ _) -> pure node
  BlockIf cond as bs -> do
    cond' <- evalCode env cond
    case cond' of
      SingleQuoteString s
        | not (T.null s) -> do
            as' <- evaluate env ("then" : stack) as
            pure $ BlockIf cond as' []
      Int n
        | n /= 0 -> do
            as' <- evaluate env ("then" : stack) as
            pure $ BlockIf cond as' []
      _ -> do
        bs' <- evaluate env ("else" : stack) bs
        pure $ BlockIf cond [] bs'
  BlockList nodes -> do
    nodes' <- evaluate env stack nodes
    pure $ BlockList nodes'
  BlockCode code -> do
    code' <- evalCode env code
    pure $ BlockCode code'

call :: Monad m => Env -> [Text] -> Text -> [Code] -> [Block] -> ExceptT PreProcessError m [Block]
call env stack name values args =
  case lookupVariable name env of
    Just (Frag names capturedEnv body) -> do
      env' <- map (\(a, (as, b)) -> (a, Frag as env b)) <$> namedBlocks args
      let env'' = augmentVariables capturedEnv env'
          arguments = zip names (map (thunk env) values)
          env''' = augmentVariables env'' arguments
      body' <- evaluate env''' ("frag" : stack) body
      pure body'
    Just _ -> throwE $ PreProcessError $ "Calling something that is not a fragment \"" <> name <> "\" in " <> T.pack (show stack)
    Nothing -> throwE $ PreProcessError $ "Can't find fragment \"" <> name <> "\""

defaultEnv :: Env
defaultEnv = Env [("true", Int 1), ("false", Int 0)]

lookupVariable :: Text -> Env -> Maybe Code
lookupVariable name Env {..} = lookup name envVariables

augmentVariables :: Env -> [(Text, Code)] -> Env
augmentVariables Env {..} xs = Env {envVariables = xs <> envVariables}

namedBlocks :: Monad m => [Block] -> ExceptT PreProcessError m [(Text, ([Text], [Block]))]
namedBlocks nodes = do
  named <- concat <$> mapM namedBlock nodes
  unnamed <- concat <$> mapM unnamedBlock nodes
  let content = if null unnamed then [] else [("content", ([], unnamed))]
  if isJust (lookup "content" named) && not (null unnamed)
    then throwE $ PreProcessError $ "A block of content and a content argument are provided"
    else pure $ named <> content

namedBlock :: Monad m => Block -> ExceptT PreProcessError m [(Text, ([Text], [Block]))]
namedBlock (BlockImport path (Just body) _) = pure [(T.pack path, ([], body))]
namedBlock (BlockFragmentDef name names content) = pure [(name, (names, content))]
namedBlock _ = pure []

unnamedBlock :: Monad m => Block -> ExceptT PreProcessError m [Block]
unnamedBlock (BlockImport path _ args) =
  pure [BlockFragmentCall (T.pack path) [] args]
unnamedBlock (BlockFragmentDef _ _ _) = pure []
unnamedBlock node = pure [node]

evalCode :: Monad m => Env -> Code -> ExceptT PreProcessError m Code
evalCode env = \case
  Variable name ->
    case lookupVariable name env of
      Just val -> evalCode env val
      Nothing -> throwE $ PreProcessError $ "Can't find variable \"" <> name <> "\""
  Lookup name key ->
    case lookupVariable name env of
      Just (Object obj) -> do
        -- key' <- evalCode env key
        case lookup key obj of
          Just val -> evalCode env val
          Nothing ->
            pure $ Variable "false"
      Just _ -> throwE $ PreProcessError $ "Variable \"" <> name <> "\" is not an object"
      Nothing -> throwE $ PreProcessError $ "Can't find variable \"" <> name <> "\""
  Add a b -> do
    a' <- evalCode env a
    b' <- evalCode env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i + j
      (Int i, SingleQuoteString s) -> pure . SingleQuoteString $ T.pack (show i) <> s
      _ -> throwE $ PreProcessError $ "Unimplemented (add): " <> T.pack (show (Add a' b'))
  Sub a b -> do
    a' <- evalCode env a
    b' <- evalCode env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i - j
      _ -> throwE $ PreProcessError $ "Unimplemented (sub): " <> T.pack (show (Add a' b'))
  Times a b -> do
    a' <- evalCode env a
    b' <- evalCode env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i * j
      _ -> throwE $ PreProcessError $ "Unimplemented (times): " <> T.pack (show (Add a' b'))
  Divide a b -> do
    a' <- evalCode env a
    b' <- evalCode env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i `div` j
      _ -> throwE $ PreProcessError $ "Unimplemented (divide): " <> T.pack (show (Add a' b'))
  Thunk capturedEnv code ->
    evalCode capturedEnv code
  code -> pure code

-- After evaluation, the template should be either empty or contain a single literal.
evalTemplate :: Monad m => Env -> [Inline] -> ExceptT PreProcessError m [Inline]
evalTemplate env inlines = do
  t <- T.concat <$> traverse (evalInline env) inlines
  pure [Lit t]

evalInline :: Monad m => Env -> Inline -> ExceptT PreProcessError m Text
evalInline env = \case
  Lit s -> pure s
  Place code -> do
    code' <- evalCode env code
    case code' of
      SingleQuoteString s -> pure s
      Int x -> pure . T.pack $ show x
      -- Variable x -> context x -- Should not happen after evalCode
      x -> error $ "evalInline: unhandled value: " <> show x

-- Extract both fragments and assignments.
-- TODO This should be merged with namedBlocks.
-- TODO We could filter the env, keeping only the free variables that appear
-- in the bodies.
extractVariables :: Env -> [Block] -> [(Text, Code)]
extractVariables env = concatMap f
 where
  f BlockDoctype = []
  f (BlockElem _ _ _ _) = []
  f (BlockText _ _) = []
  f (BlockInclude _ _ children) = maybe [] (extractVariables env) children
  f (BlockFor _ _ _ _) = []
  f (BlockFragmentDef name names children) = [(name, Frag names env children)]
  f (BlockFragmentCall _ _ _) = []
  f (BlockComment _ _) = []
  f (BlockFilter _ _) = []
  f (BlockRawElem _ _) = []
  f (BlockDefault _ _) = []
  f (BlockImport path (Just body) _) = [(T.pack path, Frag [] env body)]
  f (BlockImport _ _ _) = []
  f (BlockReadJson name _ (Just val)) = [(name, jsonToCode val)]
  f (BlockReadJson _ _ Nothing) = []
  f (BlockAssignVar name val) = [(name, val)]
  f (BlockIf _ _ _) = []
  f (BlockList _) = []
  f (BlockCode _) = []

jsonToCode :: Aeson.Value -> Code
jsonToCode = \case
  Aeson.String s -> SingleQuoteString s
  Aeson.Array xs ->
    List $ map jsonToCode (V.toList xs)
  Aeson.Object kvs ->
    let f (k, v) = (SingleQuoteString $ Aeson.Key.toText k, jsonToCode v)
     in Object $ map f (Aeson.KeyMap.toList kvs)
  x -> error $ "jsonToCode: " <> show x

--------------------------------------------------------------------------------
simplify :: [Block] -> [Block]
simplify = concatMap simplify'

simplify' :: Block -> [Block]
simplify' = \case
  node@BlockDoctype -> [node]
  BlockElem name mdot attrs nodes -> [BlockElem name mdot attrs $ simplify nodes]
  node@(BlockText _ _) -> [node]
  BlockInclude _ _ mnodes -> maybe [] simplify mnodes
  BlockFragmentDef _ _ _ -> []
  BlockFragmentCall _ _ args -> simplify args
  BlockFor _ _ _ nodes -> simplify nodes
  node@(BlockComment _ _) -> [node]
  node@(BlockFilter _ _) -> [node]
  node@(BlockRawElem _ _) -> [node]
  BlockDefault _ nodes -> simplify nodes
  BlockImport _ mbody _ -> maybe [] simplify mbody
  BlockReadJson _ _ _ -> []
  BlockAssignVar _ _ -> []
  BlockIf _ [] bs -> simplify bs
  BlockIf _ as _ -> simplify as
  BlockList nodes -> simplify nodes
  node@(BlockCode _) -> [node]
