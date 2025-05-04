{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Slab.Evaluate
-- Description : Evaluate an AST (to a non-reducible AST)
--
-- @Slab.Evaluate@ implements the evaluation stage of Slab, following both the
-- parsing and pre-processing stages. This is responsible of reducing for
-- instance @1 + 2@ to @3@, or transforming a loop construct to an actual list
-- of HTML blocks.
--
-- Evaluation works on an abstract syntax tree (defined in "Slab.Syntax") and
-- currently reuses the sames types for its result.
--
-- The stage following evaluation is "Slab.Execute", responsible of running
-- external commands.
module Slab.Evaluate
  ( evaluateFile
  , evaluate
  , evaluateVar
  , evalExpr
  , defaultEnv
  , simplify
  , simplifyVal
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.List ((\\))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Slab.Error qualified as Error
import Slab.PreProcess qualified as PreProcess
import Slab.Syntax

--------------------------------------------------------------------------------

-- | Similar to `preprocessFile` but evaluate the template.
evaluateFile :: Maybe FilePath -> IO (Either Error.Error [Block])
evaluateFile = runExceptT . evaluateFileE

evaluateFileE :: Maybe FilePath -> ExceptT Error.Error IO [Block]
evaluateFileE mpath =
  PreProcess.preprocessFileE mpath >>= evaluate defaultEnv [maybe "-" T.pack mpath]

--------------------------------------------------------------------------------
defaultEnv :: Env
defaultEnv =
  Env
    [ ("true", Bool True)
    , ("false", Bool False)
    , ("show", BuiltIn "show")
    , ("null", BuiltIn "null")
    , mkElem "div" Div
    , mkElem "html" Html
    , mkElem "body" Body
    , mkElem "span" Span
    , mkElem "h1" H1
    , mkElem "h2" H2
    , mkElem "h3" H3
    , mkElem "h4" H4
    , mkElem "h5" H5
    , mkElem "h6" H6
    , mkElem "header" Header
    , mkElem "head" Head
    , mkElem "main" Main
    , mkElem "audio" Audio
    , mkElem "a" A
    , mkElem "code" Code
    , mkElem "iframe" IFrame
    , mkElem "i" I
    , mkElem "pre" Pre
    , mkElem "p" P
    , mkElem "em" Em
    , mkElem "ul" Ul
    , mkElem "li" Li
    , mkElem "title" Title
    , mkElem "table" Table
    , mkElem "thead" Thead
    , mkElem "tbody" Tbody
    , mkElem "tr" Tr
    , mkElem "td" Td
    , mkElem "dl" Dl
    , mkElem "dt" Dt
    , mkElem "dd" Dd
    , mkElem "footer" Footer
    , mkElem "figure" Figure
    , mkElem "form" Form
    , mkElem "label" Label
    , mkElem "blockquote" Blockquote
    , mkElem "button" Button
    , mkElem "figcaption" Figcaption
    , mkElem "script" Script
    , mkElem "style" Style
    , mkElem "small" Small
    , mkElem "svg" Svg
    , mkElem "textarea" Textarea
    , mkElem "canvas" Canvas
    , -- Elements with no content.
      ("br", Block (BlockElem Br NoSym [] []))
    , ("hr", Block (BlockElem Hr NoSym [] []))
    , ("meta", Block (BlockElem Meta NoSym [] []))
    , ("link", Block (BlockElem Link NoSym [] []))
    , ("source", Block (BlockElem Source NoSym [] []))
    , ("img", Block (BlockElem Img NoSym [] []))
    , ("input", Block (BlockElem Input NoSym [] []))
    ]
 where
  mkElem name el =
    (name, Frag ["content"] emptyEnv [BlockElem el NoSym [] [BlockDefault "content" []]])

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

evaluateVar :: Monad m => Env -> [Text] -> [Block] -> Text -> ExceptT Error.Error m Expr
evaluateVar env stack nodes name = do
  -- Note that we pass the environment that we are constructing, so that each
  -- definition sees all definitions (including later ones and itself).
  let vars = extractVariables env' nodes
      env' = augmentVariables env vars
  evalExpr env' (Variable name)

eval :: Monad m => Env -> [Text] -> Block -> ExceptT Error.Error m Block
eval env stack b
  | length stack > 100 =
      throwE $
        Error.EvaluateError $
          "Stack overflow. Is there an infinite loop?"
            <> " "
            <> T.pack (show $ reverse stack)
            <> " "
            <> displayEnv env
eval env stack bl = case bl of
  node@BlockDoctype -> pure node
  BlockElem name mdot attrs nodes -> do
    attrs' <- evalAttrs env stack attrs
    nodes' <- evaluate env stack nodes
    pure $ BlockElem name mdot attrs' nodes'
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
  node@(BlockFragmentDef _ _ _ _) -> pure node
  BlockFragmentCall name mdot attrs values args -> do
    attrs' <- evalAttrs env stack attrs
    body <- call env stack name values args
    let body' = setAttrs attrs' body
    pure $ BlockFragmentCall name mdot attrs' values body'
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
  node@(BlockRun _ _ _) -> pure node
  node@(BlockAssignVars _) -> pure node
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
    Just frag@(Frag _ _ _) -> evalFrag env stack name values args frag
    Just (Block x) -> pure [x]
    Just _ -> throwE $ Error.EvaluateError $ "Calling something that is not a fragment \"" <> name <> "\" in " <> T.pack (show stack)
    Nothing -> throwE $ Error.EvaluateError $ "Can't find fragment \"" <> name <> "\" while evaluating " <> T.pack (show $ reverse stack) <> " with environment " <> displayEnv env

lookupVariable :: Text -> Env -> Maybe Expr
lookupVariable name Env {..} = lookup name envVariables

augmentVariables :: Env -> [(Text, Expr)] -> Env
augmentVariables Env {..} xs = Env {envVariables = xs <> envVariables}

evalFrag :: Monad m => Env -> [Text] -> Text -> [Expr] -> [Block] -> Expr -> ExceptT Error.Error m [Block]
evalFrag env stack name values args (Frag names capturedEnv body) = do
  env' <- extractVariables' env args
  case map fst env' \\ names of
    [] -> pure ()
    ["content"] -> pure ()
    ns ->
      throwE . Error.EvaluateError $
        "Unnecessary arguments to " <> name <> ": " <> T.pack (show ns)
  let env'' = augmentVariables (removeFormalParams names capturedEnv) env'
      arguments = zip names (map (thunk env) values)
      env''' = augmentVariables env'' arguments
  body' <- evaluate env''' ("frag " <> name : stack) body
  pure body'

removeFormalParams names Env {..} = Env {envVariables = vars'}
 where
  vars' = filter (not . (`elem` names) . fst) envVariables

evalAttrs :: Monad m => Env -> [Text] -> [Attr] -> ExceptT Error.Error m [Attr]
evalAttrs env stack attrs = mapM f attrs
 where
  f (Attr a b) = do
    b' <- evalExpr env b
    pure $ Attr a b'
  f attr = pure attr

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
  Cons a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    case (a', b') of
      (Block bl, Block c) ->
        pure . Block $ setContent [c] bl
      (Block bl, SingleQuoteString s) ->
        pure . Block $ setContent [BlockText Normal [Lit s]] bl
      _ -> throwE $ Error.EvaluateError $ "Unimplemented (cons): " <> T.pack (show (Cons a' b'))
  Application a b -> do
    a' <- evalExpr env a
    b' <- evalExpr env b
    evalApplication env a' b'
  Thunk capturedEnv code ->
    evalExpr capturedEnv code
  frag@(Frag _ _ _) -> do
    blocks <- evalFrag env ["frag"] "-" [] [] frag
    case blocks of
      [bl] -> pure $ Block bl
      _ -> pure . Block $ BlockList blocks
  Block b -> do
    b' <- eval env ["block"] b
    pure $ Block b'
  Route as -> do
    as' <- mapM (\(a, b) -> (a,) <$> evalExpr env b) as
    pure $ Route as'
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

evalTemplate :: Monad m => Env -> [Inline] -> ExceptT Error.Error m [Inline]
evalTemplate env inlines =
  traverse (evalInline env) inlines

evalInline :: Monad m => Env -> Inline -> ExceptT Error.Error m Inline
evalInline env = \case
  Lit s -> pure $ Lit s
  Place code -> do
    code' <- evalExpr env code
    case code' of
      SingleQuoteString _ -> pure $ Place code'
      Bool _ -> pure $ Place code'
      Int _ -> pure $ Place code'
      Block _ -> pure $ Place code'
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
      env' = augmentVariables env named -- Note we don't add the implicit "content" entry.
      args = extractArguments env' nodes
      vars = args <> content
  if isJust (lookup "content" args) && not (null unnamed)
    then
      throwE $
        Error.EvaluateError $
          "A block of content and a content argument are provided"
    else pure vars

unnamedBlock :: Block -> [Block]
unnamedBlock (BlockImport path _ args) = [BlockFragmentCall (T.pack path) NoSym [] [] args]
unnamedBlock (BlockFragmentDef DefinitionArg _ _ _) = []
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
  (BlockFragmentDef DefinitionNormal name names children) ->
    [(name, Frag names env children)]
  (BlockFragmentDef DefinitionArg name names children) ->
    []
  (BlockFragmentCall _ _ _ _ _) -> []
  (BlockComment _ _) -> []
  (BlockFilter _ _) -> []
  (BlockRawElem _ _) -> []
  (BlockDefault _ _) -> []
  (BlockImport path (Just body) _) -> [(T.pack path, Frag [] env body)]
  (BlockImport _ _ _) -> []
  (BlockRun _ _ _) -> []
  (BlockAssignVars pairs) -> pairs
  (BlockIf _ _ _) -> []
  (BlockList _) -> []
  (BlockCode _) -> []

-- Extract fragments used as arguments of fragment calls.
extractArguments :: Env -> [Block] -> [(Text, Expr)]
extractArguments env = concatMap (extractArgument env)

extractArgument :: Env -> Block -> [(Text, Expr)]
extractArgument env = \case
  BlockDoctype -> []
  (BlockElem _ _ _ _) -> []
  (BlockText _ _) -> []
  (BlockInclude _ _ _) -> []
  (BlockFor _ _ _ _) -> []
  (BlockFragmentDef DefinitionNormal _ _ _) ->
    []
  (BlockFragmentDef DefinitionArg name names children) ->
    [(name, Frag names env children)]
  (BlockFragmentCall _ _ _ _ _) -> []
  (BlockComment _ _) -> []
  (BlockFilter _ _) -> []
  (BlockRawElem _ _) -> []
  (BlockDefault _ _) -> []
  (BlockImport _ _ _) -> []
  (BlockRun _ _ _) -> []
  (BlockAssignVars _) -> []
  (BlockIf _ _ _) -> []
  (BlockList _) -> []
  (BlockCode _) -> []

--------------------------------------------------------------------------------
simplify :: [Block] -> [Block]
simplify = concatMap simplify'

simplify' :: Block -> [Block]
simplify' = \case
  node@BlockDoctype -> [node]
  BlockElem name mdot attrs nodes -> [BlockElem name mdot attrs $ simplify nodes]
  node@(BlockText _ _) -> [node]
  BlockInclude mfilter path mnodes -> [BlockInclude mfilter path $ simplify <$> mnodes]
  BlockFragmentDef _ _ _ _ -> []
  BlockFragmentCall _ _ _ _ args -> simplify args
  BlockFor _ _ _ nodes -> simplify nodes
  node@(BlockComment _ _) -> [node]
  node@(BlockFilter _ _) -> [node]
  node@(BlockRawElem _ _) -> [node]
  BlockDefault _ nodes -> simplify nodes
  BlockImport _ mbody _ -> maybe [] simplify mbody
  BlockRun _ _ mbody -> maybe [] simplify mbody
  BlockAssignVars _ -> []
  BlockIf _ [] bs -> simplify bs
  BlockIf _ as _ -> simplify as
  BlockList nodes -> simplify nodes
  node@(BlockCode _) -> [node]

simplifyVal :: Expr -> Expr
simplifyVal = \case
  Route as ->
    let f (Block node) = Block . BlockList $ simplify' node
        f a = a
     in Route $ map (\(a, b) -> (a, f b)) as
  val -> val
