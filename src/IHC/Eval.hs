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
    go (ELit (LInt n)) = pure (VInt n)
    go (ELit (LStr s)) = pure (VStr s)

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
            VInt n -> pure (VInt (negate n))
            other  -> error ("IHC.Eval: negate of non-Int: "
                             <> showValForDebug other)

    go (EDo stmts) =
        -- Run each stmt in source order; result of the do-block is the
        -- result of the last stmt. Intermediate results are discarded
        -- (no >>= yet — bind-stmts arrive in Phase 2.4).
        case reverse stmts of
            []     -> pure VUnit
            (l:rs) -> do
                mapM_ go (reverse rs)
                go l

    -- Pattern match alternatives. Returns the matched alt's body or
    -- raises PatternMatchFail.
    tryAlts :: Val -> [Alt] -> IO Val
    tryAlts _ [] = throwIO (PatternMatchFail "case: non-exhaustive patterns")
    tryAlts v (Alt pat body : rest) =
        case matchPat pat v of
            Just bindings -> do
                ts <- mapM (\(_, vv) -> newWHNFThunk vv) bindings
                eval (extendEnvMany (zip (map fst bindings) ts) env) body
            Nothing -> tryAlts v rest

-- | Try to match a pattern against a (already-WHNF) value. Returns
-- the variable bindings introduced by the pattern, or 'Nothing'.
matchPat :: Pat -> Val -> Maybe [(Name, Val)]
matchPat PWild        _          = Just []
matchPat (PVar n)     v          = Just [(n, v)]
matchPat (PLit (LInt n)) (VInt m) | n == m = Just []
matchPat (PLit (LInt _)) _       = Nothing
matchPat (PLit (LStr s)) (VStr t) | s == t = Just []
matchPat (PLit (LStr _)) _       = Nothing
matchPat (PCon name pats) (VCon vname vthunks)
    | name == vname && length pats == length vthunks =
        -- For Phase 2.0 we only ever build PCon at parse time for
        -- ADTs (Phase 2.1). For now this branch is unused.
        error "IHC.Eval.matchPat: nested constructor matching not yet implemented"
matchPat _ _ = Nothing

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

apply :: Val -> Thunk -> IO Val
apply (VFun f) arg = f arg
apply v        _   = error ("IHC.Eval.apply: not a function: "
                            <> showValForDebug v)
