-- | The tree-walking lazy evaluator.
--
-- Three primitive operations:
--
-- * @force :: Thunk -> IO Val@        — drive a thunk to WHNF.
-- * @eval  :: Env -> Expr -> IO Val@  — evaluate an expression to WHNF.
-- * @apply :: Val -> Thunk -> IO Val@ — apply a function-value to a thunk-arg.
--
-- Laziness is maintained because every place an Expr could be passed
-- as an argument or stored in a binding, we wrap it in a fresh
-- 'Thunk' instead. The thunk evaluates at most once, courtesy of the
-- BlackHole protocol.
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
        Unevaluated (Closure env expr) -> do
            writeIORef t BlackHole
            v <- eval env expr
            writeIORef t (Evaluated v)
            pure v

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: Env -> Expr -> IO Val
eval env = go
  where
    go (ELit (LInt n))   = pure (VInt n)
    go (ELit (LFloat d)) = pure (VFloat d)
    go (ELit (LStr s))   = pure (VStr s)
    go (ELit (LChar c))  = pure (VChar c)

    go (EVar name) = case lookupEnv name env of
        Just t  -> force t
        Nothing -> error ("IHC.Eval: unbound variable `" <> BC.unpack name <> "`")

    go (EApp f x) = do
        fv  <- go f                       -- function to WHNF
        xt  <- newThunk env x             -- argument stays a thunk (lazy)
        apply fv xt

    go (ELam name body) =
        pure $ VFun $ \argThunk ->
            eval (extendEnv name argThunk env) body

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
                   writeIORef slot (Unevaluated (Closure env' rhs)))
              (zip binds slots)
        eval env' body

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
        thunks <- mapM (newThunk env) es
        pure (VCon name thunks)

    go (EDo stmts) = evalDo env stmts

    -- Pattern match alternatives. Returns the matched alt's body or
    -- raises PatternMatchFail.
    tryAlts :: Val -> [Alt] -> IO Val
    tryAlts _ [] = throwIO (PatternMatchFail "case: non-exhaustive patterns")
    tryAlts v (Alt pat body : rest) = do
        m <- matchPat pat v
        case m of
            Just bindings ->
                eval (extendEnvMany bindings env) body
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
    -- The value is already WHNF — bind it directly to a WHNF thunk.
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
        -- and laziness — we never force the field). For any other
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

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

apply :: Val -> Thunk -> IO Val
apply (VFun f) arg = f arg
apply v        _   = error ("IHC.Eval.apply: not a function: "
                            <> showValForDebug v)

--------------------------------------------------------------------------------
-- Do-block desugaring
--
-- Desugars at eval time (not parse time) into a single 'VIO' value
-- that the driver runs. Each statement sees the env augmented by any
-- earlier 'SBind' / 'SLet' stmts.
--
-- Semantics:
--   []               — empty do is a no-op IO, per GHC, but we never
--                      parse one. Still handle defensively.
--   [SExpr e]        — evaluating @e@ must yield a 'VIO' (the action),
--                      and that action is the whole do-block's value.
--   SExpr e : rest   — sequence: run the action from @e@, discard its
--                      result, then run the rest.
--   SBind x e : rest — run the action from @e@, bind its result to @x@,
--                      then run the rest in the extended env.
--   SLet bs : rest   — semantically @let bs in <rest as EDo>@; just
--                      extend the env (lazy recursive group) and
--                      recurse.
--------------------------------------------------------------------------------

evalDo :: Env -> [Stmt] -> IO Val
evalDo _   []              = pure (VIO (pure VUnit))
evalDo env [SExpr e]       = eval env e
evalDo env [SBind _ e]     =
    -- Haskell forbids a do-block to end in a bind statement, but be
    -- defensive: evaluate the action and return its value directly.
    eval env e
evalDo _   [SLet _]        = pure (VIO (pure VUnit))
evalDo env (SExpr e : rest) =
    pure $ VIO $ do
        mv <- eval env e
        _  <- runIOVal mv                   -- run and discard
        restV <- evalDo env rest
        runIOVal restV
evalDo env (SBind name e : rest) =
    pure $ VIO $ do
        mv <- eval env e
        v  <- runIOVal mv
        vT <- newWHNFThunk v
        let env' = extendEnv name vT env
        restV <- evalDo env' rest
        runIOVal restV
evalDo env (SLet bs : rest) = do
    -- Same tying-the-knot pattern as 'ELet', but we're inside a
    -- do-block so the scope is the rest of the stmts (not a body expr).
    slots <- mapM (\_ -> newIORef BlackHole) bs
    let names = map fst bs
        env'  = extendEnvMany (zip names slots) env
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env' rhs)))
          (zip bs slots)
    evalDo env' rest

-- | Force a 'VIO' to execute its suspended action. Any other value
-- is returned as-is (treated as a "pure" IO result — shouldn't happen
-- in well-typed code, but we're optimistic and permissive).
runIOVal :: Val -> IO Val
runIOVal (VIO io) = io >>= runIOVal
runIOVal v        = pure v
