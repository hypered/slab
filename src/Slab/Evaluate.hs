{-# LANGUAGE RecordWildCards #-}

module Slab.Evaluate
  ( evaluateFile
  , evaluate
  , evalExpr
  , defaultEnv
  , simplify
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Slab.Error qualified as Error
import Slab.PreProcess qualified as PreProcess
import Slab.Syntax

--------------------------------------------------------------------------------

-- | Similar to `preprocessFile` but evaluate the template.
evaluateFile :: FilePath -> IO (Either Error.Error [Block])
evaluateFile = runExceptT . evaluateFileE

evaluateFileE :: FilePath -> ExceptT Error.Error IO [Block]
evaluateFileE path =
  PreProcess.preprocessFileE path >>= evaluate defaultEnv [T.pack path]

--------------------------------------------------------------------------------
defaultEnv :: Env
defaultEnv =
  Env
    [ ("true", Bool True)
    , ("false", Bool False)
    , ("show", BuiltIn "show")
    , ("null", BuiltIn "null")
    , -- Proof-of-concept that @div@ can be implemented without special support
      -- in the parser but can be present in the environment as a user-defined
      -- fragment. We need to be able to define @Div@ though.
      ("div", Frag ["content"] emptyEnv [BlockElem Div NoSym [] [BlockDefault "content" []]])
    , ("br", Block (BlockElem Br NoSym [] []))
    , ("hr", Block (BlockElem Hr NoSym [] []))
    , ("meta", Block (BlockElem Meta NoSym [] []))
    , ("link", Block (BlockElem Link NoSym [] []))
    , ("source", Block (BlockElem Source NoSym [] []))
    , ("img", Block (BlockElem Img NoSym [] []))
    , ("input", Block (BlockElem Input NoSym [] []))
    ]

--------------------------------------------------------------------------------

-- Process mixin calls. This should be done after processing the include statement
-- since mixins may be defined in included files.
evaluate :: Monad m => Env -> [Text] -> [Block] -> ExceptT Error.Error m [Block]
evaluate env stack nodes = do
  -- Note that we pass the environment that we are constructing, so that each
  -- definition sees all definitions (including later ones and itself).
  let vars = extractVariables env' nodes
      env' = augmentVariables env vars
  mapM (eval env' stack) nodes

eval :: Monad m => Env -> [Text] -> Block -> ExceptT Error.Error m Block
eval _ stack _ | length stack > 100 =
  throwE $ Error.EvaluateError $ "Stack overflow. Is there an infinite loop?"
eval env stack bl = case bl of
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
  BlockFragmentCall name mdot attrs values args -> do
    body <- call env stack name values args
    let body' = setAttrs attrs body
    pure $ BlockFragmentCall name mdot attrs values body'
  BlockFor name mindex values nodes -> do
    -- Re-use BlockFor to construct a single node to return.
    let zero :: Int
        zero = 0
    values' <- evalExpr env values
    collection <- case values' of
      List xs -> pure $ zip xs $ map Int [zero ..]
      Object xs -> pure $ map (\(k, v) -> (v, k)) xs
      _ -> throwE $ Error.EvaluateError $ "Iterating on something that is not a collection"
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
        nodes'' <- evaluate capturedEnv ("default block " <> name : stack) nodes'
        pure $ BlockDefault name nodes''
      Just _ -> throwE $ Error.EvaluateError $ "Calling something that is not a fragment \"" <> name <> "\" in " <> T.pack (show stack)
  BlockImport path _ args -> do
    body <- call env stack (T.pack path) [] args
    pure $ BlockImport path (Just body) args
  node@(BlockRun _ _) -> pure node
  node@(BlockReadJson _ _ _) -> pure node
  node@(BlockAssignVar _ _) -> pure node
  BlockIf cond as bs -> do
    cond' <- evalExpr env cond
    case cond' of
      Bool True -> do
        as' <- evaluate env ("then" : stack) as
        pure $ BlockIf cond as' []
      Bool False -> do
        bs' <- evaluate env ("else" : stack) bs
        pure $ BlockIf cond [] bs'
      _ ->
        throwE . Error.EvaluateError $
          "Conditional is not a boolean: " <> T.pack (show cond')
  BlockList nodes -> do
    nodes' <- evaluate env stack nodes
    pure $ BlockList nodes'
  BlockCode code -> do
    code' <- evalExpr env code
    pure $ BlockCode code'

call :: Monad m => Env -> [Text] -> Text -> [Expr] -> [Block] -> ExceptT Error.Error m [Block]
call env stack name values args =
  case lookupVariable name env of
    Just (Frag names capturedEnv body) -> do
      env' <- extractVariables' env args
      let env'' = augmentVariables capturedEnv env'
          arguments = zip names (map (thunk env) values)
          env''' = augmentVariables env'' arguments
      body' <- evaluate env''' ("frag " <> name : stack) body
      pure body'
    Just (Block x) -> pure [x]
    Just _ -> throwE $ Error.EvaluateError $ "Calling something that is not a fragment \"" <> name <> "\" in " <> T.pack (show stack)
    Nothing -> throwE $ Error.EvaluateError $ "Can't find fragment \"" <> name <> "\" while evaluating " <> T.pack (show $ reverse stack) <> " with environment " <> displayEnv env

lookupVariable :: Text -> Env -> Maybe Expr
lookupVariable name Env {..} = lookup name envVariables

augmentVariables :: Env -> [(Text, Expr)] -> Env
augmentVariables Env {..} xs = Env {envVariables = xs <> envVariables}

evalExpr :: Monad m => Env -> Expr -> ExceptT Error.Error m Expr
evalExpr env = \case
  Variable name ->
    case lookupVariable name env of
      Just val -> evalExpr env val
      Nothing -> throwE $ Error.EvaluateError $ "Can't find variable \"" <> name <> "\""
  Lookup name key ->
    case lookupVariable name env of
      Just (Object obj) -> do
        -- key' <- evalExpr env key
        case lookup key obj of
          Just val -> evalExpr env val
          Nothing -> pure $ Bool False -- TODO Either crash, or we have to implement on option type.
      Just _ -> throwE $ Error.EvaluateError $ "Variable \"" <> name <> "\" is not an object"
      Nothing -> throwE $ Error.EvaluateError $ "Can't find variable \"" <> name <> "\""
  Add a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i + j
      (SingleQuoteString s, SingleQuoteString t) ->
        pure . SingleQuoteString $ s <> t
      (Block a, Block b) ->
        pure . Block $ pasteBlocks a b
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (add): " <> T.pack (show (Add a' b'))
  Sub a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i - j
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (sub): " <> T.pack (show (Sub a' b'))
  Times a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i * j
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (times): " <> T.pack (show (Times a' b'))
  Divide a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Int $ i `div` j
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (divide): " <> T.pack (show (Divide a' b'))
  GreaterThan a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Bool $ i > j
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (greater-than): " <> T.pack (show (GreaterThan a' b'))
  LesserThan a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Int i, Int j) -> pure . Bool $ i < j
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (lesser-than): " <> T.pack (show (LesserThan a' b'))
  Equal a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Bool i, Bool j) -> pure . Bool $ i == j
      (Int i, Int j) -> pure . Bool $ i == j
      (SingleQuoteString s, SingleQuoteString t) -> pure . Bool $ s == t
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (equal): " <> T.pack (show (Equal a' b'))
  Application a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    evalApplication env a' b'
  Thunk capturedEnv code ->
    evalExpr capturedEnv code
  code -> pure code

evalApplication :: Monad m => Env -> Expr -> Expr -> ExceptT Error.Error m Expr
evalApplication env a b =
  case a of
    BuiltIn "show" -> case b of
      Int i -> pure . SingleQuoteString . T.pack $ show i
      _ -> throwE $ Error.EvaluateError $ "Cannot apply show to: " <> T.pack (show b)
    BuiltIn "null" -> case b of
      SingleQuoteString s -> pure . Bool $ T.null s
      -- TODO Lookup returns False when the key is not present,
      -- but I have this code around:
      --   if null entry['journal']
      -- We need something like:
      --   if 'journal' in entry
      --   if elem 'journal' (keys entry)
      --   ...
      Bool False -> pure . Bool $ True
      _ -> throwE $ Error.EvaluateError $ "Cannot apply null to: " <> T.pack (show b)
    _ -> throwE $ Error.EvaluateError $ "Cannot apply: " <> T.pack (show a)

-- After evaluation, the template should be either empty or contain a single literal.
evalTemplate :: Monad m => Env -> [Inline] -> ExceptT Error.Error m [Inline]
evalTemplate env inlines = do
  t <- T.concat <$> traverse (evalInline env) inlines
  pure [Lit t]

evalInline :: Monad m => Env -> Inline -> ExceptT Error.Error m Text
evalInline env = \case
  Lit s -> pure s
  Place code -> do
    code' <- evalExpr env code
    case code' of
      SingleQuoteString s -> pure s
      Int x -> pure . T.pack $ show x
      -- Variable x -> context x -- Should not happen after evalExpr
      x -> error $ "evalInline: unhandled value: " <> show x

-- | Same as `extractVariables` plus an implicit @content@ block.
-- Note that unlike `extractVariables`, this version takes also care of
-- passing the environment being constructed to each definition.
extractVariables' :: Monad m => Env -> [Block] -> ExceptT Error.Error m [(Text, Expr)]
extractVariables' env nodes = do
  let named = extractVariables env' nodes
      unnamed = concatMap unnamedBlock nodes
      content = if null unnamed then [] else [("content", Frag [] env' unnamed)]
      vars = named <> content
      env' = augmentVariables env vars
  if isJust (lookup "content" named) && not (null unnamed)
    then
      throwE $
        Error.EvaluateError $
          "A block of content and a content argument are provided"
    else pure vars

unnamedBlock :: Block -> [Block]
unnamedBlock (BlockImport path _ args) = [BlockFragmentCall (T.pack path) NoSym [] [] args]
unnamedBlock (BlockFragmentDef _ _ _) = []
unnamedBlock node = [node]

-- Extract both fragments and assignments.
-- TODO This should be merged with extractVariables'.
-- TODO We could filter the env, keeping only the free variables that appear
-- in the bodies.
extractVariables :: Env -> [Block] -> [(Text, Expr)]
extractVariables env = concatMap (extractVariable env)

extractVariable :: Env -> Block -> [(Text, Expr)]
extractVariable env = \case
  BlockDoctype -> []
  (BlockElem _ _ _ _) -> []
  (BlockText _ _) -> []
  (BlockInclude _ _ children) -> maybe [] (extractVariables env) children
  (BlockFor _ _ _ _) -> []
  (BlockFragmentDef name names children) -> [(name, Frag names env children)]
  (BlockFragmentCall _ _ _ _ _) -> []
  (BlockComment _ _) -> []
  (BlockFilter _ _) -> []
  (BlockRawElem _ _) -> []
  (BlockDefault _ _) -> []
  (BlockImport path (Just body) _) -> [(T.pack path, Frag [] env body)]
  (BlockImport _ _ _) -> []
  (BlockRun _ _) -> []
  (BlockReadJson name _ (Just val)) -> [(name, jsonToExpr val)]
  (BlockReadJson _ _ Nothing) -> []
  (BlockAssignVar name val) -> [(name, val)]
  (BlockIf _ _ _) -> []
  (BlockList _) -> []
  (BlockCode _) -> []

jsonToExpr :: Aeson.Value -> Expr
jsonToExpr = \case
  Aeson.String s -> SingleQuoteString s
  Aeson.Array xs ->
    List $ map jsonToExpr (V.toList xs)
  Aeson.Object kvs ->
    let f (k, v) = (SingleQuoteString $ Aeson.Key.toText k, jsonToExpr v)
     in Object $ map f (Aeson.KeyMap.toList kvs)
  x -> error $ "jsonToExpr: " <> show x

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
  BlockFragmentCall _ _ _ _ args -> simplify args
  BlockFor _ _ _ nodes -> simplify nodes
  node@(BlockComment _ _) -> [node]
  node@(BlockFilter _ _) -> [node]
  node@(BlockRawElem _ _) -> [node]
  BlockDefault _ nodes -> simplify nodes
  BlockImport _ mbody _ -> maybe [] simplify mbody
  BlockRun _ mbody -> maybe [] simplify mbody
  BlockReadJson _ _ _ -> []
  BlockAssignVar _ _ -> []
  BlockIf _ [] bs -> simplify bs
  BlockIf _ as _ -> simplify as
  BlockList nodes -> simplify nodes
  node@(BlockCode _) -> [node]
