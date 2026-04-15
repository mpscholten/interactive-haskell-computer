-- | Phase 2 scheduler: discover top-level bindings on demand and
-- build the global environment as a recursive group of thunks.
--
-- This is *much* simpler than the Phase-1 scheduler — there is no
-- code buffer, no W^X protocol, no two-phase layout. The scheduler
-- just collects (Name, Expr) pairs and hands them to 'IHC.Eval' as
-- one big mutually-recursive 'ELet'.
module IHC.Scheduler
    ( loadProgram
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.AST
import IHC.Builtins (builtinEnv)
import qualified IHC.Parser as Parser
import IHC.Scan
import IHC.Source
import IHC.Val

-- | Discover @main@ and every binding it transitively references,
-- then build a recursive top-level environment. Returns the env and
-- @main@'s thunk so the driver can force it.
loadProgram :: Source -> IO (Env, Thunk)
loadProgram src = do
    -- Phase A — discover (collect every reachable binding's parsed Expr).
    bodiesRef <- newIORef Map.empty
    known     <- emptyKnownSymbols
    discover src bodiesRef known "main"
    bodies <- readIORef bodiesRef

    -- Phase B — build a recursive env containing every reachable
    -- top-level binding plus all the host builtins. Same tying-the-
    -- knot pattern as ELet in IHC.Eval: pre-allocate IORefs, build
    -- the env from those live pointers, back-patch each with its
    -- real closure.
    base <- builtinEnv
    let pairs = Map.toList bodies
    slots <- mapM (\_ -> newIORef BlackHole) pairs
    let env = extendEnvMany (zip (map fst pairs) slots) base
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env rhs)))
          (zip pairs slots)

    case lookupEnv "main" env of
        Just t  -> pure (env, t)
        Nothing -> error "IHC.Scheduler: no `main` binding"

-- | Recursively parse @name@ and every binding it calls. The body's
-- @Expr@ is what callees() walks for further discovery.
discover
    :: Source
    -> IORef (Map ByteString Expr)
    -> KnownSymbols
    -> ByteString
    -> IO ()
discover src bodiesRef known name = do
    bodies <- readIORef bodiesRef
    if Map.member name bodies
        then pure ()
        else do
            mLhs <- findOrResolveLhs src known name
            case mLhs of
                Nothing  -> pure ()    -- assume builtin or local — let evaluator complain
                Just lhs -> do
                    expr <- Parser.parseBodyExpr src (lhsParams lhs) (lhsBody lhs)
                    modifyIORef' bodiesRef (Map.insert name expr)
                    mapM_ (discover src bodiesRef known) (freeVars expr)

findOrResolveLhs :: Source -> KnownSymbols -> ByteString -> IO (Maybe BindingLhs)
findOrResolveLhs src known name = do
    existing <- lookupSymbol known name
    case existing of
        Just (SpanOnly lhs) -> pure (Just lhs)
        Just (Compiled _)   -> pure Nothing       -- legacy, unused in Phase 2
        Nothing             -> findBinding src known name

-- | All free variables of an expression — names referenced via 'EVar'
-- that aren't shadowed by a lambda, let, or pattern binding inside.
-- The scheduler uses this list to drive demand-driven discovery.
freeVars :: Expr -> [ByteString]
freeVars = goAll []
  where
    goAll bound = \case
        EVar n
            | n `elem` bound -> []
            | otherwise      -> [n]
        ELit _      -> []
        EApp f x    -> goAll bound f ++ goAll bound x
        ELam n e    -> goAll (n : bound) e
        ELet bs e   ->
            let names = map fst bs
                bound' = names ++ bound
            in concatMap (\(_, rhs) -> goAll bound' rhs) bs ++ goAll bound' e
        ECase s as  -> goAll bound s ++ concatMap (goAlt bound) as
        EIf c t e   -> goAll bound c ++ goAll bound t ++ goAll bound e
        EDo es      -> concatMap (goAll bound) es
        ENeg e      -> goAll bound e

    goAlt bound (Alt p e) = goAll (patBound p ++ bound) e

    patBound :: Pat -> [ByteString]
    patBound (PVar n)   = [n]
    patBound (PCon _ ps) = concatMap patBound ps
    patBound _          = []
