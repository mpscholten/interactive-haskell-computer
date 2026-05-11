-- | C.2.3 — Core-side evaluator stub.
--
-- 'evalCore' takes a 'IHC.Core.Core' value and produces a 'Val',
-- using the same 'Thunk' / 'Env' machinery as 'IHC.Eval.eval'.
-- This is the read-only half of the dispatcher swap: it lets us
-- run the lowered Core through a tiny tree-walker so we can prove
-- (in 'CoreLowerTest') that  (lower e) ≡ eval e@ on simple
-- programs, without yet touching the production dispatcher.
--
-- Coverage at this slice:
--
--   * Atoms: 'CLit' (Int / Float / Char / String).
--   * Application chain: 'CVar' / 'CApp' / 'CLam' (closed over the
--     surrounding 'Env').
--   * Recursive 'CLet' groups (knot-tied via 'IORef' placeholders).
--   * 'CCase' pattern-match (defers to 'IHC.Eval.matchPat' so we
--     get the same semantics as the Expr-side evaluator).
--   * 'CTick' transparent pass-through.
--
-- Out of scope (raises 'error' if encountered):
--
--   * 'CDictApp' / 'CDictLam' — the lowering pass doesn't yet emit
--     these (placeholder types mean we have nothing to dispatch
--     against); the production dispatcher swap lands once the
--     elaborator is wired into 'lower'.
--   * 'CCast' — type refinement (GADT, newtype) needs the
--     refined-type info to do anything observable; today it's a
--     no-op pass-through but I want the case to be explicit so we
--     don't silently lose information.
--
-- Type annotations on Core nodes are completely ignored at this
-- slice; this is consistent with C.2.2's placeholder types.  Once
-- the elaborator is wired in, the type info will drive dictionary
-- resolution at 'CDictApp' sites.
module IHC.EvalCore
    ( evalCore
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import IHC.AST   (Lit(..))
import IHC.Classes (legacyHooks)
import IHC.Core
import IHC.Eval  (apply, force, matchPat)
import IHC.Val

-- | Evaluate a 'Core' value to WHNF.  Mirror of 'IHC.Eval.eval' for
-- the structural cases the lowering pass currently produces.
evalCore :: Env -> ImplicitParamMap -> Core -> IO Val
evalCore env ipm = go
  where
    go core = case core of
        CLit lit _ -> case lit of
            LInt n     -> pure (VInt n)
            LInteger n -> pure (VInteger n)
            LFloat d   -> pure (VFloat d)
            LChar c    -> pure (VChar c)
            LStr s     -> stringLitToList s
            LAddrStr _ -> error
                "IHC.EvalCore: LAddrStr not yet supported on Core path"

        CVar name _ -> case lookupEnv name env of
            Just t  -> force legacyHooks t
            Nothing -> error
                ( "IHC.EvalCore.evalCore: unbound variable `"
                  <> BC.unpack name <> "`" )

        CApp f x -> do
            fv <- go f
            -- Argument is wrapped in a Thunk so callee-site forcing
            -- preserves laziness.
            xT <- newThunkFromCore env ipm x
            apply legacyHooks fv xT

        CLam pat _ body -> do
            -- Build a single-argument lambda.  Multi-arg shapes
            -- arrive as nested CLams (just like the parser does for
            -- multi-arg ELams), so we don't need to handle them
            -- specially here.
            pure $ VFun $ \argT -> do
                argV <- force legacyHooks argT
                m <- matchPat legacyHooks pat argV
                case m of
                    Nothing ->
                        error ("IHC.EvalCore.evalCore: lambda pattern-match "
                               <> "failed for " <> show pat)
                    Just bindings ->
                        evalCore (extendEnvMany bindings env) ipm body

        CLet binds body -> do
            -- Recursive let group: pre-allocate placeholder slots,
            -- build the env, then back-patch each slot with its
            -- proper closure (mirrors 'IHC.Eval.eval' on 'ELet').
            slots <- traverse (\_ -> newIORef
                                       (BlackHole "<core-let-placeholder>"))
                              binds
            let names  = [n | (n, _, _) <- binds]
                envExt = extendEnvMany (zip names slots) env
            mapM_ (\((_, _, rhs), slot) ->
                       writeIORef slot
                           (Unevaluated
                                (Closure envExt ipm
                                    (error
                                       ("IHC.EvalCore: CLet rhs has no Expr; "
                                        <> "Closure stores rhs as a Core "
                                        <> "thunk via the slot — see "
                                        <> "newThunkFromCore.")))))
                   (zip binds slots)
            -- Replace each placeholder with a proper Core-backed
            -- thunk in a second pass.
            mapM_ (\((_, _, rhs), slot) -> do
                      newClosure <- newThunkFromCore envExt ipm rhs
                      cs <- readIORefThunk newClosure
                      writeIORef slot cs)
                  (zip binds slots)
            evalCore envExt ipm body

        CCase scrut _ alts -> do
            sv <- go scrut
            tryAlts sv alts

        CDictApp _ _ -> error
            "IHC.EvalCore.evalCore: CDictApp not yet supported (C.2.3 stub)"

        CDictLam _ inner ->
            -- The dictionary binder is opaque at this slice; just
            -- evaluate the body.  Real handling lands once the
            -- elaborator emits 'CDictApp' sites.
            go inner

        CCast inner _ ->
            -- Pass-through.  Type refinement has no operational
            -- effect; later slices may insert runtime checks here.
            go inner

        CTick _ inner -> go inner

    tryAlts _ [] =
        error "IHC.EvalCore.evalCore: non-exhaustive case alternatives"
    tryAlts v (CAlt p _ body : rest) = do
        m <- matchPat legacyHooks p v
        case m of
            Just bindings ->
                evalCore (extendEnvMany bindings env) ipm body
            Nothing -> tryAlts v rest

-- | Read a thunk's current state so we can copy it over a
-- placeholder slot during back-patching.  The slot already has a
-- BlackHole; once we have the proper Closure-style thunk, we want
-- to overwrite the slot with that thunk's state, not introduce a
-- second indirection.
readIORefThunk :: Thunk -> IO ThunkState
readIORefThunk t = do
    -- 'Thunk' is itself an 'IORef ThunkState'.  This is a thin
    -- alias for 'readIORef'; named to make the intent at the
    -- call-site obvious.
    readIORefRaw t

readIORefRaw :: IORef a -> IO a
readIORefRaw = readIORef

-- | Build a 'Thunk' that, when forced, evaluates a 'Core' value in
-- the given environment.  Uses 'LazyBuiltin' as the storage form
-- because we don't have an 'Expr' to wrap in a 'Closure', and we
-- want the result to memoise after first force the same way
-- 'Unevaluated' does.
newThunkFromCore :: Env -> ImplicitParamMap -> Core -> IO Thunk
newThunkFromCore env' ipm' c = newLazyBuiltinThunk (evalCore env' ipm' c)

-- | Lift a string literal's bytes to a Haskell-level @[Char]@ cons
-- list.  Mirrors the parser-side @stringToConsList@ but at eval
-- time so we don't depend on parser internals.
stringLitToList :: BC.ByteString -> IO Val
stringLitToList bs = go (BC.unpack bs)
  where
    go []     = pure (VCon "[]" [])
    go (c:cs) = do
        cT    <- newWHNFThunk (VChar c)
        restV <- go cs
        restT <- newWHNFThunk restV
        pure (VCon ":" [cT, restT])
