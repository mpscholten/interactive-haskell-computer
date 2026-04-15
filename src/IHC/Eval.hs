-- | The tree-walking lazy evaluator.
--
-- Three primitive operations:
--
-- * @force :: Thunk -> IO Val@        — drive a thunk to WHNF.
-- * @eval  :: Env -> ImplicitParamMap -> Expr -> IO Val@  — evaluate an expression to WHNF.
-- * @apply :: Val -> Thunk -> IO Val@ — apply a function-value to a thunk-arg.
--
-- Laziness is maintained because every place an Expr could be passed
-- as an argument or stored in a binding, we wrap it in a fresh
-- 'Thunk' instead. The thunk evaluates at most once, courtesy of the
-- BlackHole protocol.
--
-- Phase 3.6: ImplicitParamMap is threaded alongside Env. Implicit params
-- (?x) live in a separate namespace; closures capture the map at creation
-- time (lexical scoping).
module IHC.Eval
    ( eval
    , force
    , apply
    ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map

import IHC.AST
import IHC.Val

--------------------------------------------------------------------------------
-- force
--------------------------------------------------------------------------------

force :: Thunk -> IO Val
force t = do
    st <- readIORef t
    case st of
        Evaluated v -> pure v
        BlackHole   -> throwIO LoopException
        Unevaluated (Closure env ipm expr) -> do
            writeIORef t BlackHole
            v <- eval env ipm expr
            writeIORef t (Evaluated v)
            pure v

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: Env -> ImplicitParamMap -> Expr -> IO Val
eval env ipm = go
  where
    go (ELit (LInt n))   = pure (VInt n)
    go (ELit (LFloat d)) = pure (VFloat d)
    go (ELit (LStr s))   = pure (VStr s)
    go (ELit (LChar c))  = pure (VChar c)
    go (ELabel name)     = pure (VLabel name)  -- Phase 3.5: OverloadedLabels

    go (EVar name) = case lookupEnv name env of
        Just t  -> force t
        Nothing -> error ("IHC.Eval: unbound variable `" <> BC.unpack name <> "`")

    go (EApp f x) = do
        fv  <- go f                            -- function to WHNF
        xt  <- newThunkIP env ipm x            -- argument stays a thunk (lazy)
        applyIP ipm fv xt                      -- pass caller's ipm for ?-params

    go (ELam name body) =
        -- Phase 3.6: User-defined lambdas use VFunIP so the caller can
        -- pass its ImplicitParamMap at call time. The closed-over `ipm`
        -- (lexical binding) takes priority over the caller's map.
        pure $ VFunIP ipm $ \callerIPM argThunk ->
            let mergedIPM = Map.union ipm callerIPM
            in eval (extendEnv name argThunk env) mergedIPM body

    go (ELet binds body) = do
        -- Recursive group: pre-allocate a thunk per binding holding a
        -- 'BlackHole' placeholder, build the env from those (now-live)
        -- IORef pointers, then back-patch each slot with its real
        -- closure that can now see the env. This is the classic
        -- tying-the-knot pattern with mutable refs — avoids the
        -- strict-cycle hazard that 'mfix' / 'rec' would hit on the
        -- 'IO' monad given that 'Closure' has a strict env field.
        slots <- mapM (\_ -> newIORef BlackHole) binds
        let names  = map fst binds
            env'   = extendEnvMany (zip names slots) env
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure env' ipm rhs)))
              (zip binds slots)
        eval env' ipm body

    go (ECase scrut alts) = do
        v <- go scrut
        tryAlts v alts

    go (EIf c t e) = do
        cv <- go c
        case cv of
            VInt 0 -> go e
            VInt _ -> go t
            VCon "False" _ -> go e
            VCon "True"  _ -> go t
            other -> error ("IHC.Eval: if condition is not Int/Bool: "
                            <> showValForDebug other)

    go (ENeg e) = do
        v <- go e
        case v of
            VInt n   -> pure (VInt (negate n))
            VFloat d -> pure (VFloat (negate d))
            other    -> error ("IHC.Eval: negate of non-numeric: "
                               <> showValForDebug other)

    go (ETuple es) = do
        -- Build the right tuple constructor name for this arity.
        let arity = length es
            name  = BC.pack ("(" <> replicate (arity - 1) ',' <> ")")
        thunks <- mapM (newThunkIP env ipm) es
        pure (VCon name thunks)

    go (EDo stmts) = evalDo env ipm stmts

    -- Phase 3.6: Implicit parameter reference.
    -- Look up ?name in the current ImplicitParamMap. Miss -> runtime error.
    go (EImplicitRef name) = case lookupIPMap name ipm of
        Just t  -> force t
        Nothing -> error ("IHC.Eval: implicit parameter `?"
                          <> BC.unpack name <> "` is not in scope")

    -- Phase 3.6: Implicit parameter let-binding.
    -- Extend the implicit-param map for the duration of @body@.
    -- Each binding thunk captures the CURRENT env+ipm (not the extended ipm').
    go (EImplicitLet binds body) = do
        slots <- mapM (\_ -> newIORef BlackHole) binds
        let names = map fst binds
            ipm'  = foldr (\(n, sl) m -> extendIPMap n sl m) ipm
                          (zip names slots)
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure env ipm rhs)))
              (zip binds slots)
        eval env ipm' body

    -- Record construction: Con { f1 = e1, f2 = e2, ... }
    -- We ignore field names and build a positional VCon.
    go (ERecordCon name fields) = do
        thunks <- mapM (\(_, e) -> newThunkIP env ipm e) fields
        pure (VCon name thunks)

    -- RecordWildCards construction: ERecordWild should be desugared by the
    -- scheduler's desugarRecordCons pass before reaching eval.
    go (ERecordWild n) =
        error ("IHC.Eval: ERecordWild reached eval — desugarRecordCons missed: "
               <> BC.unpack n)

    -- Phase 2.11: TH splices should be expanded before eval by the
    -- scheduler's expandSplicesInModule pass. If one reaches here it's
    -- a bug — report it clearly rather than looping.
    go (ESplice _) =
        error "IHC.Eval: ESplice reached eval — splice expansion pass missed this node"

    -- Pattern match alternatives. Returns the matched alt's body or
    -- raises PatternMatchFail.
    tryAlts :: Val -> [Alt] -> IO Val
    tryAlts _ [] = throwIO (PatternMatchFail "case: non-exhaustive patterns")
    tryAlts v (Alt pat body : rest) = do
        m <- matchPat pat v
        case m of
            Just bindings ->
                eval (extendEnvMany bindings env) ipm body
            Nothing -> tryAlts v rest

-- | Try to match a pattern against a (already-WHNF) value. Returns
-- the variable bindings introduced by the pattern, or 'Nothing'.
--
-- Returns 'Thunk' rather than 'Val' because nested constructor
-- patterns may force sub-fields and rebind them; for a 'PVar' we
-- reuse the existing thunk so the match itself doesn't lose sharing.
matchPat :: Pat -> Val -> IO (Maybe [(Name, Thunk)])
matchPat PWild        _          = pure (Just [])
matchPat (PVar n)     v          = do
    -- The value is already WHNF -- bind it directly to a WHNF thunk.
    -- (This is the cheapest way; we don't have the original thunk
    -- here since the caller forced it before calling matchPat.)
    t <- newWHNFThunk v
    pure (Just [(n, t)])
matchPat (PBang p)    v          = matchPat p v
matchPat (PAs n p)    v          = do
    m <- matchPat p v
    case m of
        Nothing  -> pure Nothing
        Just bs  -> do
            t <- newWHNFThunk v
            pure (Just ((n, t) : bs))
matchPat (PTuple ps) v = do
    let arity = length ps
        tupleName = BC.pack ("(" <> replicate (arity - 1) ',' <> ")")
    case v of
        VCon cn vthunks
            | cn == tupleName && length vthunks == arity ->
                matchPat (PCon tupleName ps) v
        _ -> pure Nothing
matchPat (PLit (LInt n)) (VInt m)
    | n == m    = pure (Just [])
    | otherwise = pure Nothing
matchPat (PLit (LInt _)) _       = pure Nothing
matchPat (PLit (LFloat x)) (VFloat y)
    | x == y    = pure (Just [])
    | otherwise = pure Nothing
matchPat (PLit (LFloat _)) _     = pure Nothing
matchPat (PLit (LStr s)) (VStr t)
    | s == t    = pure (Just [])
    | otherwise = pure Nothing
matchPat (PLit (LStr _)) _       = pure Nothing
matchPat (PLit (LChar c)) (VChar d)
    | c == d    = pure (Just [])
    | otherwise = pure Nothing
matchPat (PLit (LChar _)) _      = pure Nothing
-- Unit constructor pattern matches VUnit (the canonical runtime unit).
matchPat (PCon "()" []) VUnit = pure (Just [])
matchPat (PCon name pats) (VCon vname vthunks)
    | name == vname && length pats == length vthunks =
        -- Zip sub-patterns with the constructor's field thunks. For
        -- each pair: if the sub-pattern is a 'PVar' we bind the name
        -- directly to the existing field thunk (preserving sharing
        -- and laziness -- we never force the field). For any other
        -- sub-pattern we MUST force the thunk to pattern-match its
        -- structure, then recurse.
        matchFields (zip pats vthunks) []
    | otherwise = pure Nothing
  where
    matchFields [] acc = pure (Just (reverse acc))
    matchFields ((PVar n, t) : rest) acc =
        matchFields rest ((n, t) : acc)
    matchFields ((PWild, _) : rest) acc =
        matchFields rest acc
    matchFields ((p, t) : rest) acc = do
        fv <- force t
        m  <- matchPat p fv
        case m of
            Nothing   -> pure Nothing
            Just subs -> matchFields rest (reverse subs ++ acc)
matchPat (PCon _ _) _ = pure Nothing
-- Record patterns: should have been desugared to PCon by the scheduler.
-- If they reach here (e.g. in a standalone test), fall back to failure.
matchPat (PRecord _ _) _ = pure Nothing
matchPat (PRecordWild _) _ = pure Nothing
-- ViewPatterns: (f -> p) matches v when f v matches p.
-- We evaluate f v and then match the result against p.
matchPat (PView fn p) v = do
    -- f is an Expr; we need an Env to evaluate it. We don't have the
    -- env here, so ViewPatterns MUST be desugared before reaching Eval.
    -- This case is a safeguard — in practice desugarRecordPats converts
    -- PView into a case expression via desugarViewPat in the scheduler.
    error ("IHC.Eval: PView reached matchPat — view pattern not desugared: "
            <> show fn <> " -> " <> show p)

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

apply :: Val -> Thunk -> IO Val
apply (VFun f)        arg = f arg
apply (VFunIP _ f)    arg = f Map.empty arg
apply v               _   = error ("IHC.Eval.apply: not a function: "
                                   <> showValForDebug v)

-- | Apply with the caller's ImplicitParamMap — used by EApp so that
-- implicit params flow from the call site into the callee.
applyIP :: ImplicitParamMap -> Val -> Thunk -> IO Val
applyIP callerIPM (VFun f)     arg = f arg
applyIP callerIPM (VFunIP _ f) arg = f callerIPM arg
applyIP _         v            _   = error ("IHC.Eval.applyIP: not a function: "
                                            <> showValForDebug v)

--------------------------------------------------------------------------------
-- Do-block desugaring
--
-- Desugars at eval time (not parse time) into a single 'VIO' value
-- that the driver runs. Each statement sees the env augmented by any
-- earlier 'SBind' / 'SLet' stmts.
--
-- Semantics:
--   []               -- empty do is a no-op IO, per GHC, but we never
--                      parse one. Still handle defensively.
--   [SExpr e]        -- evaluating @e@ must yield a 'VIO' (the action),
--                      and that action is the whole do-block's value.
--   SExpr e : rest   -- sequence: run the action from @e@, discard its
--                      result, then run the rest.
--   SBind x e : rest -- run the action from @e@, bind its result to @x@,
--                      then run the rest in the extended env.
--   SLet bs : rest   -- semantically @let bs in <rest as EDo>@; just
--                      extend the env (lazy recursive group) and
--                      recurse.
--------------------------------------------------------------------------------

evalDo :: Env -> ImplicitParamMap -> [Stmt] -> IO Val
evalDo _   _   []              = pure (VIO (pure VUnit))
evalDo env ipm [SExpr e]       = eval env ipm e
evalDo env ipm [SBind _ e]     =
    -- Haskell forbids a do-block to end in a bind statement, but be
    -- defensive: evaluate the action and return its value directly.
    eval env ipm e
evalDo _   _   [SLet _]        = pure (VIO (pure VUnit))
evalDo env ipm (SExpr e : rest) =
    pure $ VIO $ do
        mv <- eval env ipm e
        _  <- runIOVal mv                   -- run and discard
        restV <- evalDo env ipm rest
        runIOVal restV
evalDo env ipm (SBind name e : rest) =
    pure $ VIO $ do
        mv <- eval env ipm e
        v  <- runIOVal mv
        vT <- newWHNFThunk v
        let env' = extendEnv name vT env
        restV <- evalDo env' ipm rest
        runIOVal restV
evalDo env ipm (SLet bs : rest) = do
    -- Same tying-the-knot pattern as 'ELet', but we're inside a
    -- do-block so the scope is the rest of the stmts (not a body expr).
    slots <- mapM (\_ -> newIORef BlackHole) bs
    let names = map fst bs
        env'  = extendEnvMany (zip names slots) env
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env' ipm rhs)))
          (zip bs slots)
    evalDo env' ipm rest

-- | Force a 'VIO' to execute its suspended action. Any other value
-- is returned as-is (treated as a "pure" IO result -- shouldn't happen
-- in well-typed code, but we're optimistic and permissive).
runIOVal :: Val -> IO Val
runIOVal (VIO io) = io >>= runIOVal
runIOVal v        = pure v
