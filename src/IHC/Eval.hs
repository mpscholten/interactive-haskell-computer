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
    , forceMethodVal
    , apply
    , matchPat
    , runIOVal
    , ownerSentinelKey
    , currentOwner
    ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import qualified Data.Map.Strict as Map

import Control.Exception (try, SomeException)

import IHC.AST
import IHC.Classes (ClassRegistry, IHCHooks, legacyHooks, normalizeTyTag, lookupEnvFallback, lookupInstanceMethod, getSharedClassReg, triggerCoreInstanceLoad, lookupClassMethodFallback, runThExpToExpr)
import qualified IHC.Elaborate as Elab
import qualified IHC.PatSyn as PatSyn
import qualified IHC.TypeAST as TA
import IHC.TypeGlobals (globalTypeSigsRef, globalTypeSynonymsRef)
import qualified IHC.TypeReduce as TR
import IHC.Val

--------------------------------------------------------------------------------
-- force
--------------------------------------------------------------------------------

-- | Force a 'VLazyMethod' wrapper; pass non-wrapped Vals through.
-- Instance method bodies are registered lazily (see
-- 'IHC.Scheduler.evalMethodWithLazy') so the env snapshot they capture
-- doesn't need every rewrite target resolved at registration time.
-- Propagates exceptions from 'force' — callers decide whether to
-- fall back to a placeholder.
forceMethodVal :: IHCHooks -> Val -> IO Val
forceMethodVal hooks (VLazyMethod t) = force hooks t
forceMethodVal _     v               = pure v

-- | Evaluate an 'ETypedMethod' node.  Looks up the resolved instance
-- method in the class registry; if the instance registered a
-- 'methodPlaceholder' (class default with no per-instance override),
-- falls back to a known-equivalent method (e.g. Monad.return →
-- Applicative.pure).
resolveTypedMethod :: IHCHooks -> ClassRegistry -> Name -> Name -> Name -> IO Val
resolveTypedMethod hooks reg cls method tag = do
    resolved <- tryResolve
    case resolved of
        Just v  -> forceMethodVal hooks v
        Nothing -> do
            -- First-miss retry: the REPL doesn't pre-load core instance
            -- dicts (keeps startup latency low). The hook force-loads
            -- the modules the manifest reports as providers for THIS
            -- class (and the modules defining its instance head types)
            -- once per session per class; subsequent calls for the same
            -- class are free. ('lookupInstanceMethod' inside @tryResolve@
            -- already drains the Stage-2 lazy-instance catalogue on
            -- miss, so a separate drain is unnecessary here.)
            triggerCoreInstanceLoad legacyHooks cls
            resolved2 <- tryResolve
            case resolved2 of
                Just v  -> forceMethodVal hooks v
                Nothing -> do
                    -- Still no match: fall back to the existing value-
                    -- directed dispatcher.  This keeps behaviour
                    -- equivalent to pre-elaborator semantics when the
                    -- resolved tag points at an instance we haven't
                    -- loaded (e.g. @Monad (ST s)@: we don't force-load
                    -- @GHC.Internal.ST@ on startup).
                    mFallback <- lookupClassMethodFallback legacyHooks cls method
                    case mFallback of
                        Just v  -> forceMethodVal hooks v
                        Nothing -> error ("IHC.Eval.ETypedMethod: no instance `"
                                        <> BC.unpack cls <> " " <> BC.unpack tag
                                        <> "` for method `" <> BC.unpack method <> "`")
  where
    tryResolve = do
        mMethod <- lookupInstanceMethod reg cls tag method
        case mMethod of
            Just v | not (isPlaceholder v) -> pure (Just v)
            _ -> tryFallbacks (fallbackList cls method)

    isPlaceholder (VCon n []) =
        n == BC.pack "<ihc-method-placeholder>"
    isPlaceholder _ = False

    tryFallbacks [] = pure Nothing
    tryFallbacks ((c, m) : rest) = do
        v <- lookupInstanceMethod reg c tag m
        case v of
            Just vv | not (isPlaceholder vv) -> pure (Just vv)
            _ -> tryFallbacks rest

    fallbackList = typedMethodFallbacks

-- | Known equalities between class methods — used when an instance
-- relies on the class's default body instead of providing a
-- per-instance override.  Mapped to the class/method that DOES
-- have a concrete body. Shared by 'resolveTypedMethod' (the lookup
-- path) and 'allTypedMethodsResolvable' (the elaborator-rewrite
-- validation guard).
typedMethodFallbacks :: Name -> Name -> [(Name, Name)]
typedMethodFallbacks c m
  | c == BC.pack "Monad", m == BC.pack "return" =
        [(BC.pack "Applicative", BC.pack "pure")]
  | c == BC.pack "Monad", m == BC.pack "(>>)"   =
        [(BC.pack "Applicative", BC.pack "(*>)")]
  | otherwise = []

force :: IHCHooks -> Thunk -> IO Val
force hooks t = do
    st <- readIORef t
    case st of
        Evaluated v -> pure v
        BlackHole msg -> throwIO (LoopException msg)
        Unevaluated (Closure env ipm expr) -> do
            writeIORef t (BlackHole (take 500 (show expr)))
            v <- eval hooks env ipm expr
            writeIORef t (Evaluated v)
            pure v
        -- Lazy-init builtin: run the host @IO Val@ action exactly once,
        -- then memoise. Mirrors the 'Unevaluated' path (same black-hole
        -- protocol) so concurrent forces see 'LoopException' instead of
        -- double-running the initialiser. See 'IHC.Val.newLazyBuiltinThunk'.
        LazyBuiltin mkV -> do
            writeIORef t (BlackHole "<lazy-builtin>")
            v <- mkV
            writeIORef t (Evaluated v)
            pure v

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Val
eval hooks env ipm = go
  where
    go (ELit (LInt n))   = pure (VInt n)
    go (ELit (LInteger n)) = pure (VInteger n)
    go (ELit (LFloat d)) = pure (VFloat d)
    -- Source-level Haskell strings are [Char]. Keeping literals as real cons
    -- lists lets source-loaded libraries like bytestring pattern-match and
    -- recurse over them normally instead of tripping over the transitional
    -- VStr representation.
    go (ELit (LStr s))   = stringLiteralToListVal s
    go (ELit (LChar c))  = pure (VChar c)
    go (ELabel name)     = pure (VLabel name)  -- Phase 3.5: OverloadedLabels
    go EGuardFail        = throwIO (PatternMatchFail "guard failed")

    go (EVar name) = case lookupEnv name env of
        Just t  -> force hooks t
        Nothing -> do
            -- Demand-driven fallback (Haskell 2010 §4.3.2 scope + lazy
            -- body eval): a closure's frozen env may be missing a
            -- fully-qualified name whose body only became visible after
            -- the env snapshot was taken.  Consult the scheduler-
            -- installed hook before erroring.
            --
            -- Pass the owner module of the closure being evaluated (read
            -- from the @"$$owner"@ sentinel inserted in 'Env' at closure
            -- construction time — see 'IHC.Scheduler.buildSlotFromOwner'
            -- and the entry-module installation in 'loadProgramFromSource').
            -- The fallback uses owner to scope the unqualified-name
            -- search to that module's actual import declarations, per
            -- Haskell 2010 §5.5.  'Nothing' falls back to the unscoped
            -- search (entry boundary, builtins, transient lookups).
            owner <- currentOwner hooks env
            mT    <- lookupEnvFallback legacyHooks owner name
            case mT of
                Just t  -> force hooks t
                Nothing -> error ("IHC.Eval: unbound variable `"
                                  <> BC.unpack name <> "`")

    go (EApp f x) = do
        fv  <- go f                            -- function to WHNF
        xt  <- newThunkIP env ipm x            -- argument stays a thunk (lazy)
        case fv of
            VPrimObj _ -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): VPrimObj in function position: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a)
            VCon _ (_:_:_) -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): not a function while evaluating `"
                       <> show f <> "` applied to `" <> show x <> "`: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a)
            VCon n [] | not (isStateTokenNewtypeCtor n) -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): not a function while evaluating `"
                       <> show f <> "` applied to `" <> show x <> "`: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a)
            _ -> applyIP hooks ipm fv xt

    go (ELam name body) =
        -- Phase 3.6: User-defined lambdas use VFunIP so the caller can
        -- pass its ImplicitParamMap at call time. The closed-over `ipm`
        -- (lexical binding) takes priority over the caller's map.
        pure $ VFunIP ipm $ \callerIPM argThunk ->
            let mergedIPM = Map.union ipm callerIPM
            in eval hooks (extendEnv name argThunk env) mergedIPM body

    go (ELet binds body) = do
        -- Recursive group: pre-allocate a thunk per binding holding a
        -- 'BlackHole' placeholder, build the env from those (now-live)
        -- IORef pointers, then back-patch each slot with its real
        -- closure that can now see the env. This is the classic
        -- tying-the-knot pattern with mutable refs — avoids the
        -- strict-cycle hazard that 'mfix' / 'rec' would hit on the
        -- 'IO' monad given that 'Closure' has a strict env field.
        slots <- mapM (\_ -> newIORef (BlackHole "<let-placeholder>")) binds
        let names  = map fst binds
            env'   = extendEnvMany (zip names slots) env
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure env' ipm rhs)))
              (zip binds slots)
        eval hooks env' ipm body

    go (ECase scrut alts) = do
        v0 <- go scrut
        -- Primops like `newByteArray#` return an internal VIO wrapper around
        -- their unboxed-tuple result. A case expression must force that
        -- wrapper before matching, but must not eagerly execute source-built
        -- `IO` / `ST` constructors, which use `VCon`.
        v <- case v0 of
            VIO _ -> runIOVal hooks v0
            _     -> pure v0
        tryAlts v alts

    go (EIf c t e) = do
        cv <- go c
        case cv of
            VInt 0 -> go e
            VInt _ -> go t
            VCon "False" _ -> go e
            VCon "True"  _ -> go t
            other -> error ("IHC.Eval: if condition is not Int/Bool: "
                            <> showValForDebug other
                            <> " in "
                            <> show c)

    go (ENeg e) = do
        v <- go e
        case v of
            VInt n     -> pure (VInt (negate n))
            VInteger n -> pure (VInteger (negate n))
            VFloat d   -> pure (VFloat (negate d))
            other      -> error ("IHC.Eval: negate of non-numeric: "
                                 <> showValForDebug other)

    go (ETuple es) = do
        -- Build the right tuple constructor name for this arity.
        let arity = length es
            name  = BC.pack ("(" <> replicate (arity - 1) ',' <> ")")
        thunks <- mapM (newThunkIP env ipm) es
        pure (VCon name thunks)

    go (EDo stmts) = evalDo hooks env ipm stmts

    -- Phase 3.6: Implicit parameter reference.
    -- Look up ?name in the current ImplicitParamMap. Miss -> runtime error.
    go (EImplicitRef name) = case lookupIPMap name ipm of
        Just t  -> force hooks t
        Nothing -> error ("IHC.Eval: implicit parameter `?"
                          <> BC.unpack name <> "` is not in scope")

    -- Phase 3.6: Implicit parameter let-binding.
    -- Extend the implicit-param map for the duration of @body@.
    -- Each binding thunk captures the CURRENT env+ipm (not the extended ipm').
    go (EImplicitLet binds body) = do
        slots <- mapM (\_ -> newIORef (BlackHole "<implicit-let-placeholder>")) binds
        let names = map fst binds
            ipm'  = foldr (\(n, sl) m -> extendIPMap n sl m) ipm
                          (zip names slots)
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure env ipm rhs)))
              (zip binds slots)
        eval hooks env ipm' body

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

    -- Record update: ERecordUpdate should be desugared by desugarRecordCons
    -- into an ECase. If it reaches eval, it means the registry didn't know the
    -- constructor (graceful degradation: evaluate the base expression unchanged).
    go (ERecordUpdate baseExpr _) = go baseExpr

    -- Phase 2.11: TH splices should be expanded before eval by the
    -- scheduler's expandSplicesInModule pass. If one reaches here it's
    -- a bug — report it clearly rather than looping.
    go (ESplice _) =
        error "IHC.Eval: ESplice reached eval — splice expansion pass missed this node"

    -- Phase 2.12: TemplateHaskellQuotes bracket [| expr |].
    -- Produce a TH Exp-shaped Val encoding of the *syntax* of expr.
    -- We do NOT evaluate expr — we encode its AST.
    go (EQuote inner) = evalQuote inner

    -- QuasiQuoter dispatch: @[qqName|body|]@.  Look up qqName, project
    -- @quoteExp :: String -> Q Exp@ via '$fldProj$quoteExp', apply to the
    -- body string, run the resulting Q action, decode the TH Exp via the
    -- 'IHC.TH' hook, and evaluate the result in the current scope.
    go (EQuasiQuote qqName body) = do
        qqVal      <- go (EVar qqName)
        projVal    <- go (EVar (BC.pack "$fldProj$quoteExp"))
        qqT        <- newWHNFThunk qqVal
        quoteExpFn <- applyIP hooks ipm projVal qqT
        bodyT      <- newWHNFThunk =<< stringLiteralToListVal body
        qExp       <- applyIP hooks ipm quoteExpFn bodyT
        thExpVal   <- runIOVal hooks qExp
        eval hooks env ipm =<< runThExpToExpr legacyHooks thExpVal

    -- Value-level TypeApplications (@T). ihc is optimistic about types:
    -- the type argument is retained by the parser as AST metadata, but
    -- evaluation proceeds on the underlying expression as if the @T were
    -- absent. Two special cases are peeled off here so DataKinds /
    -- @GHC.TypeLits@ (@symbolVal@, @natVal@, @charVal@) can recover their
    -- type arg at runtime:
    --
    --   (a) @Proxy \@T@ — when the inner expression evaluates to the bare
    --       @VCon "Proxy" []@ produced by the @Proxy@ constructor, the
    --       type argument is attached as a singleton field (@VLabel@ for
    --       a symbol literal, @VInt@ for a nat literal, @VChar@ for a
    --       char literal). @symbolVal@ / @natVal@ / @charVal@ inspect
    --       this field at call time.
    --
    --   (b) @symbolVal \@\"name\"@ / @natVal \@42@ — when the inner
    --       expression is a direct reference to one of these functions,
    --       we can short-circuit and emit a closure that returns the
    --       type arg as its result, ignoring whatever Proxy-shaped value
    --       the caller supplies.  This keeps parity with GHC's behaviour
    --       where a call like @symbolVal \@\"email\" undefined@ works.
    --
    -- | Elaborator-produced class method resolved to a specific
    -- instance.  Direct registry lookup; no dispatcher.  Emitted only
    -- by 'IHC.Elaborate' when inference has resolved the method's
    -- type.  Falls back to an unbound-variable error if the resolved
    -- (class, tag, method) isn't in the shared class registry —
    -- that's a bug (elaborator shouldn't emit an unresolvable tag).
    go (ETypedMethod cls method tag) = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing  -> error ("IHC.Eval.ETypedMethod: no shared class registry installed "
                              <> "(elaborator fired before buildBaseEnv?)")
            Just reg -> resolveTypedMethod hooks reg cls method tag

    -- All other uses are the plain pass-through on the inner expression.
    go (ETyApp e ty0) = do
        -- Try to reduce the raw type-argument bytes through the
        -- type-family registry built by the scheduler.  For ordinary
        -- types the registry lookup misses and we keep @ty0@ as-is;
        -- for type-family applications (e.g. @GetTableName User@) we
        -- replace it with the reduced bytes (e.g. @"users"@) so the
        -- downstream DataKinds extractors see the literal directly.
        reg <- TR.getGlobalRegistry
        let ty = case TR.reduceTypeExpr reg ty0 of
                     Just reduced -> reduced
                     Nothing      -> ty0
        mTypedNullary <- tryTypedNullaryClassMethod e ty
        case mTypedNullary of
            Just v  -> pure v
            Nothing -> do
        -- Trigger on-demand elaboration: if the annotation parses to a
        -- concrete type AND the shared class registry is installed,
        -- run inference on @e@ with expected type @ty@.  The elaborator
        -- may rewrite ambiguous class method EVars into ETypedMethod
        -- nodes; we then evaluate the rewritten expression.  On failure
        -- (or if inference doesn't change anything) we fall through to
        -- the original 'goTyApp' path — preserving all existing
        -- value-directed dispatch behaviour.
                mElab <- tryElaborateTyAnn e ty
                case mElab of
                    Just e' -> goTyApp e' ty
                    Nothing -> goTyApp e ty

    tryTypedNullaryClassMethod e ty =
        case e of
            EVar method
                | method == BC.pack "maxBound"
               || method == BC.pack "minBound" -> do
                    mReg <- getSharedClassReg legacyHooks
                    case mReg of
                        Nothing -> pure Nothing
                        Just classReg -> do
                            let tag = normalizeTyTag ty
                            -- 'lookupInstanceMethod' drains the Stage-2
                            -- lazy-instance catalogue on miss.
                            mv <- lookupInstanceMethod classReg
                                    (BC.pack "Bounded") tag method
                            case mv of
                                Nothing -> pure Nothing
                                Just v -> do
                                    r <- try (forceMethodVal hooks v)
                                            :: IO (Either SomeException Val)
                                    case r of
                                        Right v' -> pure (Just v')
                                        Left _   -> pure Nothing
            _ -> pure Nothing

    -- | Helper: try to elaborate @e@ under the annotation @ty@.
    -- Returns 'Just' if elaboration rewrote something; 'Nothing'
    -- otherwise.
    tryElaborateTyAnn e ty = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing -> pure Nothing
            Just classReg -> case Elab.parseRawTypeExpr ty of
                Nothing -> pure Nothing
                Just annTy -> do
                    sigs <- readIORef globalTypeSigsRef
                    syns <- readIORef globalTypeSynonymsRef
                    r <- try (Elab.elaborate classReg sigs syns
                                (Elab.ExpectType annTy) e)
                           :: IO (Either SomeException (Expr, TA.Type))
                    case r of
                        Right (e', _) | e' /= e -> do
                            -- Validate the rewrite. The elaborator emits
                            -- 'ETypedMethod cls method tag' wherever a
                            -- name's signature looks like a class
                            -- method (constraint @cls v@ with @v@ in the
                            -- body, name in 'globalClassMethodNamesRef').
                            -- That heuristic mis-fires for top-level
                            -- functions that happen to share a name with
                            -- a class method elsewhere — the canonical
                            -- example is 'Control.Exception.try' getting
                            -- routed through 'MonadParsec.try''s
                            -- dispatcher because megaparsec's
                            -- 'Text.Megaparsec.Class' is on the
                            -- core-instance load list.
                            --
                            -- If the rewritten expression contains an
                            -- 'ETypedMethod' whose resolved
                            -- @(cls, tag, method)@ has no registered
                            -- instance (after the lazy-instance catalogue
                            -- has been drained, which 'lookupInstanceMethod'
                            -- does on miss), the dispatch path will return
                            -- a useless value-directed dispatcher
                            -- (VClassMethod → VFun with no real backing),
                            -- which then mis-binds the caller's argument.
                            -- Better to skip the rewrite and let the
                            -- evaluator resolve the bare 'EVar' through
                            -- the env (where the actual builtin or
                            -- source-loaded body lives).
                            ok <- allTypedMethodsResolvable classReg e'
                            if ok then pure (Just e') else pure Nothing
                        _ -> pure Nothing

    -- | True iff every 'ETypedMethod' node in @e@ has a real, non-placeholder
    -- registered instance for its resolved @(cls, tag, method)@. Used to
    -- decide whether to keep an elaborator rewrite or fall back to the
    -- original expression. Drains the lazy-instance catalogue for each
    -- class as a side effect (via 'lookupInstanceMethod'), which is
    -- fine — those drains would have happened anyway when the dispatch
    -- ran the rewrite.
    allTypedMethodsResolvable :: ClassRegistry -> Expr -> IO Bool
    allTypedMethodsResolvable reg = go
      where
        -- True iff there's a real method body — either directly under
        -- @(cls, tag, method)@, under one of the known fallback pairs
        -- ('Monad.return' → 'Applicative.pure'), or under the class
        -- default tag @<default>@. All three lookups go through
        -- 'lookupInstanceMethod', which drains the lazy-instance
        -- catalogue on miss.
        checkOne cls method tag = do
            direct <- lookupInstanceMethod reg cls tag method
            case nonPlaceholder direct of
                Just _  -> pure True
                Nothing -> do
                    fb <- tryFb (typedMethodFallbacks cls method) tag
                    if fb
                        then pure True
                        else do
                            mDef <- lookupInstanceMethod reg cls
                                        (BC.pack "<default>") method
                            case nonPlaceholder mDef of
                                Just _  -> pure True
                                Nothing -> pure False

        tryFb [] _              = pure False
        tryFb ((c, m):rest) tag = do
            mv <- lookupInstanceMethod reg c tag m
            case nonPlaceholder mv of
                Just _  -> pure True
                Nothing -> tryFb rest tag

        nonPlaceholder mv = case mv of
            Just (VCon n []) | n == BC.pack "<ihc-method-placeholder>" -> Nothing
            _ -> mv

        go (ETypedMethod cls method tag) = checkOne cls method tag
        go (EApp f x)            = (&&) <$> go f <*> go x
        go (ELam _ body)         = go body
        go (ELet bs body)        = (&&) <$> allM (\(_, b) -> go b) bs <*> go body
        go (ECase s as)          = (&&) <$> go s <*> allM (\(Alt _ b) -> go b) as
        go (EIf c t b)           = allM go [c, t, b]
        go (EDo stmts)           = allM goStmt stmts
        go (ENeg inner)          = go inner
        go (ETuple es)           = allM go es
        go (ERecordCon _ fs)     = allM (\(_, v) -> go v) fs
        go (ERecordUpdate s fs)  = (&&) <$> go s <*> allM (\(_, v) -> go v) fs
        go (ESplice inner)       = go inner
        go (EQuote inner)        = go inner
        go (ETyApp inner _)      = go inner
        go (EImplicitLet bs body) = (&&) <$> allM (\(_, b) -> go b) bs <*> go body
        go _                     = pure True

        goStmt (SExpr s)         = go s
        goStmt (SBind _ s)       = go s
        goStmt (SBangBind _ s)   = go s
        goStmt (SLet bs)         = allM (\(_, b) -> go b) bs
        goStmt (SImplicitLet bs) = allM (\(_, b) -> go b) bs

        allM _ []     = pure True
        allM p (x:xs) = do
            r <- p x
            if r then allM p xs else pure False

    goTyApp e ty
        | isTypeLitsFn e = pure (tyAppLitsClosure (headName e) ty)
        | otherwise      = do
            v <- go e
            case v of
                VCon "Proxy" [] -> attachProxyType ty
                -- Multi-key class dispatch: @setField \@\"name\" \@User \@String@
                -- accumulates type-arg tags onto the dispatcher so the final
                -- call can look up the instance by composite key.
                VClassMethod m slot tags fn ->
                    pure (VClassMethod m slot (tags ++ [normalizeTyTag ty]) fn)
                -- IsLabel dispatch: @(#email :: Wrap)@ should behave like
                -- @fromLabel \@"email" \@Wrap@.  We have no typechecker, so
                -- when a bare VLabel flows through a non-@Proxy@ type
                -- ascription we route it through @fromLabel@ in the env
                -- (which in turn calls @lookupUserIsLabel@ keyed by the
                -- Symbol).  This picks the right instance for
                -- @instance IsLabel "email" Wrap where fromLabel = ...@
                -- without needing an explicit @fromLabel #email@.
                --
                -- @Proxy s@ annotations are left as raw @VLabel@ on purpose:
                -- the default IHP-style instance is @IsLabel s (Proxy s')@
                -- and the pattern-match transparency at 'PCon \"Proxy\"' /
                -- 'VLabel _' (see below) lets callers treat the label as a
                -- proxy without an eager conversion.
                VLabel _
                    | isProxyTyAnnotation ty -> pure v
                    | otherwise              ->
                        case lookupEnv (BC.pack "fromLabel") env of
                            Just t -> do
                                flv <- force hooks t
                                case flv of
                                    VFun _ -> do
                                        lblT <- newWHNFThunk v
                                        apply hooks flv lblT
                                    _ -> pure v
                            Nothing -> pure v
                _               -> pure v

    -- Pattern match alternatives. Returns the matched alt's body or
    -- raises PatternMatchFail.
    tryAlts :: Val -> [Alt] -> IO Val
    tryAlts v alts0 = goAlts alts0
      where
        goAlts [] = throwIO (PatternMatchFail
            ("case: non-exhaustive patterns for "
             <> showValForDebug v
             <> " in alternatives "
             <> show (map (\(Alt p _) -> p) alts0)))
        goAlts (Alt pat body : rest) = do
            m <- matchPat hooks pat v
            case m of
                Just bindings -> do
                    r <- try (eval hooks (extendEnvMany bindings env) ipm body)
                           :: IO (Either PatternMatchFail Val)
                    case r of
                        Right result -> pure result
                        Left (PatternMatchFail "guard failed") -> goAlts rest
                        Left err -> throwIO err
                Nothing -> goAlts rest

    stringLiteralToListVal :: ByteString -> IO Val
    stringLiteralToListVal bs = goChars (BC.unpack bs)
      where
        goChars [] = pure (VCon "[]" [])
        goChars (ch:chs) = do
            chT <- newWHNFThunk (VChar ch)
            restV <- goChars chs
            restT <- newWHNFThunk restV
            pure (VCon ":" [chT, restT])

    -- Wrap a DataKinds type argument as a @Proxy@ payload. The raw bytes
    -- come straight from the parser's 'captureTypeArg' slice and may be a
    -- string literal (@\"email\"@), a natural-number literal (@42@), a
    -- character literal (@\'x\'@), or anything else (type name, type
    -- constructor application, ...). For constructors/type names we fall
    -- back to a @VLabel@ holding the raw bytes so consumers that want the
    -- name string still have something to show.
    attachProxyType :: ByteString -> IO Val
    attachProxyType tyBytes = do
        payload <- parseTyArgLit tyBytes
        payloadT <- newWHNFThunk payload
        pure (VCon "Proxy" [payloadT])

--------------------------------------------------------------------------------
-- DataKinds / GHC.TypeLits runtime helpers.
--
-- See the 'ETyApp' case in 'eval' above for the call sites.  These helpers
-- inspect the raw bytes captured by the parser's 'captureTypeArg' and
-- decide how to surface them at the Val level.
--------------------------------------------------------------------------------

-- | Names we short-circuit when seen as the @f@ in @f \@T@ (value-level
-- TypeApplications).  Used so @symbolVal \@\"email\" undefined@,
-- @natVal \@42 undefined@, etc. can resolve without needing a typechecker
-- to synthesise the @KnownSymbol@ / @KnownNat@ dictionary.
isTypeLitsFn :: Expr -> Bool
isTypeLitsFn e = case headName e of
    Just n  -> n `elem` typeLitsFnNames
    Nothing -> False

typeLitsFnNames :: [Name]
typeLitsFnNames =
    [ "symbolVal", "symbolVal'"
    , "natVal",    "natVal'"
    , "charVal",   "charVal'"
    ]

-- | Extract the outermost applied variable name from an expression.
-- Unwraps any already-applied 'ETyApp'; stops at non-variable heads.
-- Uses the unqualified name so @GHC.TypeLits.symbolVal@ matches too.
headName :: Expr -> Maybe Name
headName (EVar n)      = Just (stripQualifier n)
headName (ETyApp e _)  = headName e
headName _             = Nothing

-- | Build a closure that mimics @symbolVal \@\"T\"@, @natVal \@42@, etc.
-- The returned @VFun@ ignores its argument (typically an @undefined@
-- proxy) and returns the parsed type literal.
tyAppLitsClosure :: Maybe Name -> ByteString -> Val
tyAppLitsClosure mname tyBytes = VFun $ \_arg -> do
    payload <- parseTyArgLit tyBytes
    case (mname, payload) of
        -- symbolVal :: Proxy -> String — return a cons-list of Char.
        (Just "symbolVal",  VLabel n) -> stringLitVal n
        (Just "symbolVal'", VLabel n) -> stringLitVal n
        (Just "symbolVal",  VInt  n) -> stringLitVal (BC.pack (show n))
        (Just "symbolVal'", VInt  n) -> stringLitVal (BC.pack (show n))
        -- natVal :: Proxy -> Integer — return VInt directly.
        (Just "natVal",  VInt n) -> pure (VInt n)
        (Just "natVal'", VInt n) -> pure (VInt n)
        -- charVal :: Proxy -> Char — return VChar.
        (Just "charVal",  VChar c) -> pure (VChar c)
        (Just "charVal'", VChar c) -> pure (VChar c)
        -- Fallbacks: honour the parsed value if its shape matches.
        (_, VLabel n) -> stringLitVal n
        (_, VInt   n) -> pure (VInt n)
        (_, VChar  c) -> pure (VChar c)
        (_, other) -> pure other

-- | Parse the raw bytes from a type-application argument into a Val.
--
--   * @\"foo\"@           → @VLabel \"foo\"@
--   * @42@                → @VInt 42@
--   * @\'x\'@             → @VChar \'x\'@
--   * @Proxy \"foo\"@     → @VLabel \"foo\"@ (strip type-con prefix)
--   * anything else       → @VLabel bytes@ (best-effort: keep the text).
--
-- This is intentionally forgiving — the evaluator never demands a full
-- type parse; we just recover what DataKinds can lift to the value
-- level.  When the bytes start with a constructor (e.g. @Proxy \"x\"@
-- from a @(Proxy :: Proxy \"x\")@ annotation) we fall back to scanning
-- the bytes for the first @\"…\"@ / numeric literal.
-- | Detect a @Proxy ...@ type annotation (possibly parenthesised) without
-- a full type parse. Used by the 'ETyApp' VLabel fast-path to skip the
-- @fromLabel@ dispatch when the user wrote @(#email :: Proxy \"email\")@ —
-- we want the label to remain a raw 'VLabel' there, matching IHP's
-- default @IsLabel s (Proxy s')@ behaviour and the pattern-match
-- transparency at @PCon \"Proxy\" []@.
isProxyTyAnnotation :: ByteString -> Bool
isProxyTyAnnotation bs0 = go (strip bs0)
  where
    strip s =
        let t = BC.dropWhile (\c -> c == ' ' || c == '\t') s
            u = BC.reverse (BC.dropWhile (\c -> c == ' ' || c == '\t') (BC.reverse t))
        in if BS.length u >= 2 && BC.head u == '(' && BC.last u == ')'
              then strip (BS.init (BS.tail u))
              else u
    proxyBs = BC.pack "Proxy"
    go bs = proxyBs `BS.isPrefixOf` bs
        && (BS.length bs == 5
            || (let c = BC.index bs 5 in c == ' ' || c == ')' || c == '\t'))

parseTyArgLit :: ByteString -> IO Val
parseTyArgLit bs0 =
    let bs = stripParens bs0
    in if BS.null bs
           then pure (VLabel bs)
           else case BC.head bs of
               '"' -> pure (VLabel (stripOuter '"' bs))
               '\'' -> case BC.unpack (stripOuter '\'' bs) of
                   [c]  -> pure (VChar c)
                   _    -> pure (VLabel bs)
               c | c == '-' || (c >= '0' && c <= '9') ->
                   case BC.readInteger bs of
                       Just (n, rest)
                         | BS.null (BC.dropWhile isAsciiSpace rest) ->
                             pure (VInt (fromInteger n))
                       _ -> pure (VLabel bs)
                 | otherwise ->
                     -- Constructor/type-name prefix (e.g. @Proxy "email"@)
                     -- — rescan looking for the first lifted literal.
                     case findInnerLit bs of
                         Just v  -> pure v
                         Nothing -> pure (VLabel bs)
  where
    stripOuter q s =
        let s'  = if not (BS.null s) && BC.head s == q then BS.tail s else s
            s'' = if not (BS.null s') && BC.last s' == q
                    then BS.init s' else s'
        in s''

    -- Strip outer @( … )@ pairs iteratively. Type applications may come
    -- in via @(Proxy "email")@.
    stripParens s =
        let t = trimAsciiSpace s
        in if BS.length t >= 2 && BC.head t == '(' && BC.last t == ')'
              then stripParens (BS.init (BS.tail t))
              else t

    trimAsciiSpace s =
        BC.dropWhile isAsciiSpace
            (BC.reverse (BC.dropWhile isAsciiSpace (BC.reverse s)))

    isAsciiSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    -- Find the first string literal, character literal, or nat literal
    -- embedded in the bytes.  Used to dig a DataKinds literal out of a
    -- @Proxy "x"@ / @Proxy 42@ style annotation.
    findInnerLit :: ByteString -> Maybe Val
    findInnerLit s
        | BS.null s = Nothing
        | otherwise = case BC.head s of
            '"' ->
                -- Consume up to the next unescaped quote.
                let rest = BS.tail s
                in case BC.elemIndex '"' rest of
                    Just i  -> Just (VLabel (BS.take i rest))
                    Nothing -> Nothing
            '\'' ->
                -- Char literal: '\X' or 'X'. Look for closing quote.
                let rest = BS.tail s
                in case BC.elemIndex '\'' rest of
                    Just i | i >= 1 -> case BC.unpack (BS.take i rest) of
                        [c]        -> Just (VChar c)
                        ['\\', c]  -> Just (VChar c)
                        _          -> findInnerLit (BS.drop (i + 1) rest)
                    _ -> findInnerLit (BS.tail s)
            c | c >= '0' && c <= '9' ->
                case BC.readInteger s of
                    Just (n, _) -> Just (VInt (fromInteger n))
                    Nothing     -> findInnerLit (BS.tail s)
              | otherwise -> findInnerLit (BS.tail s)

-- | Produce a Haskell @[Char]@ (cons-chain) Val from raw UTF-8 bytes.
stringLitVal :: ByteString -> IO Val
stringLitVal bs = go (BC.unpack bs)
  where
    go [] = pure (VCon "[]" [])
    go (c:cs) = do
        cT    <- newWHNFThunk (VChar c)
        restV <- go cs
        restT <- newWHNFThunk restV
        pure (VCon ":" [cT, restT])

-- | Try to match a pattern against a (already-WHNF) value. Returns
-- the variable bindings introduced by the pattern, or 'Nothing'.
--
-- Returns 'Thunk' rather than 'Val' because nested constructor
-- patterns may force sub-fields and rebind them; for a 'PVar' we
-- reuse the existing thunk so the match itself doesn't lose sharing.

-- | Match a Haskell string literal against a runtime cons-list value
-- (@VCon ":" [h, t]@ chain ending in @VCon "[]" []@).  Succeeds when the
-- list's characters equal @s@ (byte-for-byte over the UTF-8 payload).
matchStringPatList :: IHCHooks -> ByteString -> Val -> IO (Maybe [(Name, Thunk)])
matchStringPatList hooks s0 v0 = go (BC.unpack s0) v0
  where
    go [] (VCon "[]" []) = pure (Just [])
    go (c:cs) (VCon ":" [hT, tT]) = do
        hv <- force hooks hT
        case hv of
            VChar d | d == c -> do
                tv <- force hooks tT
                go cs tv
            _ -> pure Nothing
    -- The runtime sometimes keeps short strings as VStr even after
    -- head-normalization; fall back to direct bytes comparison.
    go rest (VStr bs)
        | BC.unpack bs == rest = pure (Just [])
        | otherwise            = pure Nothing
    go _ _ = pure Nothing

pureStateFn :: Val -> Val
pureStateFn v = VFun $ \_stateThunk -> do
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    vT <- newWHNFThunk v
    pure (VCon "(#,#)" [stT, vT])

-- | All variables bound by a pattern, left-to-right.  Mirrors the
-- @patVars@ helpers in 'IHC.Parser' (which are private to that module).
-- Used by the 'PIrref' matcher to know which thunk slots to allocate.
patternVars :: Pat -> [Name]
patternVars (PVar n)         = [n]
patternVars PWild            = []
patternVars (PLit _)         = []
patternVars (PCon _ ps)      = concatMap patternVars ps
patternVars (PAs n p)        = n : patternVars p
patternVars (PBang p)        = patternVars p
patternVars (PIrref p)       = patternVars p
patternVars (PTuple ps)      = concatMap patternVars ps
patternVars (PRecord _ fps)  = concatMap (patternVars . snd) fps
patternVars (PRecordWild _)  = []
patternVars (PView _ p)      = patternVars p

-- | Attempt to match a constructor pattern against a value via the
-- pattern synonym registry: if @name@ is registered as a pattern
-- synonym with parameters @ps@ and body @b@, substitute @ps -> args@
-- in @b@ and re-match against @v@.  Falls back to 'Nothing' when
-- @name@ is not a registered synonym (i.e. genuinely an unknown
-- constructor).
tryPatSyn :: IHCHooks -> Name -> [Pat] -> Val -> IO (Maybe [(Name, Thunk)])
tryPatSyn hooks name args v = do
    mPs <- PatSyn.lookupPatSyn name
    case mPs of
        Just (PatSyn.PatSyn params body)
            | length params == length args ->
                let sub   = Map.fromList (zip params args)
                    body' = PatSyn.substPat sub body
                in matchPat hooks body' v
        _ -> pure Nothing

matchPat :: IHCHooks -> Pat -> Val -> IO (Maybe [(Name, Thunk)])
matchPat hooks PWild        _          = pure (Just [])
matchPat hooks (PVar n)     v          = do
    -- The value is already WHNF -- bind it directly to a WHNF thunk.
    -- (This is the cheapest way; we don't have the original thunk
    -- here since the caller forced it before calling matchPat.)
    t <- newWHNFThunk v
    pure (Just [(n, t)])
matchPat hooks (PBang p)    v          = matchPat hooks p v
-- Per Haskell Report §3.17.3: an irrefutable pattern @~p@ ALWAYS
-- matches.  Each variable bound by @p@ becomes a thunk that, when
-- forced, re-attempts the inner match against the original value;
-- only then can the match-fail error fire.  This is the "lazy
-- pattern" deferral.  Sharing of the inner match is preserved by the
-- thunk's IORef-backed memoisation.
matchPat hooks (PIrref p)   v          = do
    let vars = patternVars p
    binds <- traverse (\name -> do
        t <- newLazyBuiltinThunk $ do
            m <- matchPat hooks p v
            case m of
                Just bs -> case lookup name bs of
                    Just t' -> force hooks t'
                    Nothing ->
                        error ("IHC.Eval: irrefutable pattern: variable `"
                               <> BC.unpack name
                               <> "` not bound by inner pattern "
                               <> show p)
                Nothing ->
                    error ("Irrefutable pattern failed for pattern "
                           <> show p)
        pure (name, t)) vars
    pure (Just binds)
matchPat hooks (PAs n p)    v          = do
    m <- matchPat hooks p v
    case m of
        Nothing  -> pure Nothing
        Just bs  -> do
            t <- newWHNFThunk v
            pure (Just ((n, t) : bs))
matchPat hooks (PTuple ps) v = do
    let arity = length ps
        tupleName = BC.pack ("(" <> replicate (arity - 1) ',' <> ")")
    case v of
        VCon cn vthunks
            | cn == tupleName && length vthunks == arity ->
                matchPat hooks (PCon tupleName ps) v
        _ -> pure Nothing
matchPat hooks (PLit (LInt n)) (VInt m)
    | n == m    = pure (Just [])
    | otherwise = pure Nothing
matchPat hooks (PLit (LInt _)) _       = pure Nothing
-- A.3: arbitrary-precision Integer literal pattern.  Equality against
-- VInteger uses the underlying Integer; against VInt we widen to
-- compare. No match against any other shape.
matchPat hooks (PLit (LInteger n)) (VInteger m)
    | n == m    = pure (Just [])
    | otherwise = pure Nothing
matchPat hooks (PLit (LInteger n)) (VInt m)
    | n == toInteger m = pure (Just [])
    | otherwise        = pure Nothing
matchPat hooks (PLit (LInteger _)) _   = pure Nothing
matchPat hooks (PLit (LFloat x)) (VFloat y)
    | x == y    = pure (Just [])
    | otherwise = pure Nothing
matchPat hooks (PLit (LFloat _)) _     = pure Nothing
matchPat hooks (PLit (LStr s)) (VStr t)
    | s == t    = pure (Just [])
    | otherwise = pure Nothing
-- Strings are compiled to Haskell lists of 'Char' (@VCon ":" [h, t]@ /
-- @VCon "[]" []@), so a string-literal pattern must also match the
-- list-form runtime value.  Without this case a pattern like @"" -> ...@
-- fails against @VCon "[]" []@ even though they denote the same string.
matchPat hooks (PLit (LStr s)) v = matchStringPatList hooks s v
matchPat hooks (PLit (LStr _)) _       = pure Nothing
matchPat hooks (PLit (LChar c)) (VChar d)
    | c == d    = pure (Just [])
    | otherwise = pure Nothing
matchPat hooks (PLit (LChar _)) _      = pure Nothing
-- Unit constructor pattern matches VUnit (the canonical runtime unit).
matchPat hooks (PCon "()" []) VUnit = pure (Just [])
-- DataKinds: @Proxy \@"foo"@ is represented as @VCon "Proxy" [payload]@
-- but pattern @Proxy@ (nullary) should still match — the payload is
-- type-level metadata that user code doesn't observe via the ctor.
matchPat hooks (PCon "Proxy" []) (VCon "Proxy" _) = pure (Just [])
-- Boxed prim constructors are host-backed wrappers over the interpreter's
-- primitive runtime values. Pattern matching must therefore treat
-- @I# x@ / @W# x@ / @W8# x@ as wrappers around 'VInt' and @C# x@ as a
-- wrapper around 'VChar'; otherwise source bindings like
-- @new (I# len#) = ...@ never match and libraries such as @text@ fail at
-- first use after discovery succeeds.
matchPat hooks (PCon "I#" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "W#" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "W8#" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "C#" [p]) (VChar c) = do
    t <- newWHNFThunk (VChar c)
    matchFields hooks [(p, t)] []
-- F# / D#: source-loaded Num Float / Num Double instance bodies
-- pattern-match on these to access the underlying Float# / Double#.
-- The runtime stores both Float and Double as VFloat (Double internally),
-- so the unwrap just exposes that VFloat for the primops to operate on.
matchPat hooks (PCon "F#" [p]) (VFloat n) = do
    t <- newWHNFThunk (VFloat n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "D#" [p]) (VFloat n) = do
    t <- newWHNFThunk (VFloat n)
    matchFields hooks [(p, t)] []
-- Lazy ST's lifted state token is `data State s = S# (State# s)`.
-- The interpreter represents all erased State# tokens as PrimRealWorld, so
-- expose that raw token through the source constructor when lazy ST code
-- pattern-matches on `S# s`.
matchPat hooks (PCon "S#" [p]) prim@(VPrimObj PrimRealWorld) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
-- IORef/STRef source wrappers around the same host-backed mutable reference.
-- Some source functions (e.g. GHC.Exts.touch via network's withFdSocket)
-- pattern-match through IORef (STRef ref#), while ihc's newIORef builtin
-- stores the reference directly as PrimIORef.
matchPat hooks (PCon "IORef" [PCon "STRef" [p]]) prim@(VPrimObj (PrimIORef _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "STRef" [p]) prim@(VPrimObj (PrimIORef _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "TVar" [p]) prim@(VPrimObj (PrimTVar _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
-- Data.Array.Byte lifted wrappers. The interpreter keeps both mutable and
-- frozen byte arrays as the same host-backed PrimByteArray object, so the
-- source constructors just expose that underlying primitive value.
matchPat hooks (PCon "MutableByteArray" [p]) prim@(VPrimObj (PrimByteArray _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "ByteArray" [p]) prim@(VPrimObj (PrimByteArray _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "BS" pats) (VCon "BS" vthunks)
    | length pats == length vthunks =
        matchFields hooks (zip pats vthunks) []
matchPat hooks pat@(PCon "BS" _) v = do
    mBs <- charListToByteStringVal hooks v
    case mBs of
        Just bsV -> matchPat hooks pat bsV
        Nothing  -> pure Nothing
matchPat hooks (PCon "IO" [p]) v@(VCon name _)
    | name /= "IO" = matchPat hooks p (pureStateFn v)
matchPat hooks (PCon "ST" [p]) v@(VCon name _)
    | name /= "ST" = matchPat hooks p (pureStateFn v)
-- bytestring-0.12 exposes PS as a pattern synonym over the real BS
-- constructor. The parser represents the synonym as a constructor pattern, so
-- model the synonym at match time.
matchPat hooks (PCon "PS" [pFp, pOff, pLen]) (VCon "BS" [fpT, lenT]) = do
    offT <- newWHNFThunk (VInt 0)
    matchFields hooks [(pFp, fpT), (pOff, offT), (pLen, lenT)] []
matchPat hooks pat@(PCon "PS" _) v = do
    mBs <- charListToByteStringVal hooks v
    case mBs of
        Just bsV -> matchPat hooks pat bsV
        Nothing  -> pure Nothing
-- Lazy ST represents state-thread results as boxed pairs `(a, State s)`,
-- while strict ST code pattern-matches on unboxed state tuples
-- `(# State# s, a #)`. When those representations meet at
-- strictToLazyST/lazyToStrictST boundaries, expose the boxed pair in the
-- strict state-passing order.
matchPat hooks (PCon "(#,#)" [pState, pVal]) (VCon "(,)" [valT, stateT]) =
    matchFields hooks [(pState, stateT), (pVal, valT)] []
matchPat hooks (PCon name pats) v@(VCon vname vthunks)
    | name == vname && length pats == length vthunks =
        -- Zip sub-patterns with the constructor's field thunks. For
        -- each pair: if the sub-pattern is a 'PVar' we bind the name
        -- directly to the existing field thunk (preserving sharing
        -- and laziness -- we never force the field). For any other
        -- sub-pattern we MUST force the thunk to pattern-match its
        -- structure, then recurse.
        matchFieldsLocal (zip pats vthunks) []
    | otherwise =
        -- Constructor name doesn't match the value's constructor.
        -- May still succeed via a pattern synonym (e.g.
        -- @pattern Head x \<- (x:_)@ matched against a non-empty list).
        tryPatSyn hooks name pats v
  where
    matchFieldsLocal [] acc = pure (Just (reverse acc))
    matchFieldsLocal ((PVar n, t) : rest) acc =
        matchFieldsLocal rest ((n, t) : acc)
    matchFieldsLocal ((PWild, _) : rest) acc =
        matchFieldsLocal rest acc
    -- Per Haskell Report §3.17.2 + GHC BangPatterns: a bang sub-pattern
    -- forces the corresponding field thunk to WHNF before binding. The
    -- generic _ -> force arm below handles non-PVar inner patterns
    -- correctly (the force happens unconditionally), but PBang (PVar n)
    -- would otherwise route through matchPat's PBang->p collapse and
    -- bind n to an unforced thunk via the PVar arm above. Force here.
    matchFieldsLocal ((PBang inner, t) : rest) acc = do
        _ <- force hooks t  -- ! : force the field thunk; sharing preserved.
        case inner of
            PVar n -> matchFieldsLocal rest ((n, t) : acc)
            PWild  -> matchFieldsLocal rest acc
            _      -> do
                fv <- force hooks t
                m  <- matchPat hooks inner fv
                case m of
                    Nothing   -> pure Nothing
                    Just subs -> matchFieldsLocal rest (reverse subs ++ acc)
    matchFieldsLocal ((p, t) : rest) acc = do
        fv <- force hooks t
        m  <- matchPat hooks p fv
        case m of
            Nothing   -> pure Nothing
            Just subs -> matchFieldsLocal rest (reverse subs ++ acc)
-- IO constructor: VIO wraps a suspended (IO Val). When source code
-- pattern-matches on IO (e.g. `IO act`), expose the underlying
-- State# RealWorld -> (# State# RealWorld, a #) function so that
-- source-loaded unsafeDupablePerformIO & friends can deconstruct it.

-- ForeignPtr deconstruction: source code pattern-matches
-- `ForeignPtr addr# contents` on our opaque VPrimObj (PrimForeignPtr fp).
-- Expose Addr# as VPrimObj PrimPtr (the raw pointer) and
-- ForeignPtrContents as an opaque placeholder.
matchPat hooks (PCon "ForeignPtr" [pAddr, pContents]) (VPrimObj (PrimForeignPtr fp)) = do
    let rawPtr = unsafeForeignPtrToPtr fp
    addrThunk <- newWHNFThunk (VPrimObj (PrimPtr (castPtr rawPtr)))
    -- ForeignPtrContents is opaque; use the ForeignPtr itself as a stand-in
    -- so touchForeignPtr can still work if needed.
    contentsThunk <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    matchFieldsLocal [(pAddr, addrThunk), (pContents, contentsThunk)] []
  where
    matchFieldsLocal [] acc = pure (Just (reverse acc))
    matchFieldsLocal ((PVar n, t) : rest) acc =
        matchFieldsLocal rest ((n, t) : acc)
    matchFieldsLocal ((PWild, _) : rest) acc =
        matchFieldsLocal rest acc
    matchFieldsLocal ((p, t) : rest) acc = do
        fv <- force hooks t
        m  <- matchPat hooks p fv
        case m of
            Nothing   -> pure Nothing
            Just subs -> matchFieldsLocal rest (reverse subs ++ acc)

-- Ptr deconstruction: source code pattern-matches `Ptr addr#`.
matchPat hooks (PCon "Ptr" [pAddr]) (VPrimObj (PrimPtr ptr)) = do
    addrThunk <- newWHNFThunk (VPrimObj (PrimPtr ptr))
    matchFieldsLocal [(pAddr, addrThunk)] []
  where
    matchFieldsLocal [] acc = pure (Just (reverse acc))
    matchFieldsLocal ((PVar n, t) : rest) acc =
        matchFieldsLocal rest ((n, t) : acc)
    matchFieldsLocal ((PWild, _) : rest) acc =
        matchFieldsLocal rest acc
    matchFieldsLocal ((p, t) : rest) acc = do
        fv <- force hooks t
        m  <- matchPat hooks p fv
        case m of
            Nothing   -> pure Nothing
            Just subs -> matchFieldsLocal rest (reverse subs ++ acc)

-- Phase 3.5: OverloadedLabels IsLabel dispatch.
-- IHP's default instance `(s ~ s') => IsLabel s (Proxy s')` converts #email
-- into `Proxy @"email"`. Since we have no types, we make this transparent
-- at the pattern-match layer: matching `Proxy` against a `VLabel _` succeeds
-- (0 sub-patterns since Proxy is nullary). This lets source code like
-- `case lbl of Proxy -> ...` work whether the label was already forced
-- through `fromLabel` or is still a raw VLabel.
matchPat hooks (PCon "Proxy" []) (VLabel _) = pure (Just [])
matchPat hooks (PCon "IO" [p]) stFn@(VFun _) =
    matchPat hooks p stFn
matchPat hooks (PCon "IO" [p]) stFn@(VFunIP _ _) =
    matchPat hooks p stFn
matchPat hooks (PCon "IO" [p]) (VIO action) = do
    let stFn = VFun $ \_stateThunk -> do
            -- Run the IO action, return an unboxed tuple (# state, result #)
            result <- action
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            resT <- newWHNFThunk result
            pure (VCon "(#,#)" [stT, resT])
    matchPat hooks p stFn
matchPat hooks (PCon "IO" [p]) v =
    matchPat hooks p (pureStateFn v)
-- ST bridge: VIO-valued ST computations (e.g. `return 42 :: ST s Int`
-- produces `VIO (pure 42)` via the builtin `returnB`). When source code
-- pattern-matches `ST f` on such a value -- e.g. `runST (ST m)` in
-- GHC.ST -- expose the same State#-passing function the ST constructor
-- expects, so the pure ST = IO bridge is transparent at the match layer.
-- Rationale: `ST s a` is semantically identical to `IO a` in our
-- single-threaded interpreter (see CLAUDE.md: runRW# is compiler-intrinsic,
-- no userland Haskell can implement it).
matchPat hooks (PCon "ST" [p]) stFn@(VFun _) =
    matchPat hooks p stFn
matchPat hooks (PCon "ST" [p]) stFn@(VFunIP _ _) =
    matchPat hooks p stFn
matchPat hooks (PCon "ST" [p]) (VIO action) = do
    let stFn = VFun $ \_stateThunk -> do
            result <- action
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            resT <- newWHNFThunk result
            pure (VCon "(#,#)" [stT, resT])
    matchPat hooks p stFn
matchPat hooks (PCon "ST" [p]) v =
    matchPat hooks p (pureStateFn v)
matchPat hooks (PCon "STM" [p]) stFn@(VFun _) =
    matchPat hooks p stFn
matchPat hooks (PCon "STM" [p]) stFn@(VFunIP _ _) =
    matchPat hooks p stFn
matchPat hooks (PCon "STM" [p]) (VIO action) = do
    let stFn = VFun $ \_stateThunk -> do
            result <- action
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            resT <- newWHNFThunk result
            pure (VCon "(#,#)" [stT, resT])
    matchPat hooks p stFn
matchPat hooks (PCon "STM" [p]) v =
    matchPat hooks p (pureStateFn v)
-- Pattern-match-driven type-class dispatch: when a class method
-- dispatcher flows into a position where the pattern expects a
-- specific constructor, the pattern tells us the type.  Feed the
-- constructor name as a type tag to the dispatcher; the closure
-- looks up the instance method for that type (e.g. `mempty :: Text`
-- for `PCon "Text" _`) and returns the concrete value, which we
-- then re-match.  This is the backbone of nullary class-method
-- dispatch (mempty, maxBound, empty, …) in an otherwise
-- arg-directed runtime.
matchPat hooks pat@(PCon pname _) (VClassMethod _ _ _ go) = do
    dummyT <- newWHNFThunk VUnit
    resolved <- go [pname] dummyT
    case resolved of
        VClassMethod{} -> pure Nothing
        _              -> matchPat hooks pat resolved
matchPat hooks (PCon pname ppats) v = tryPatSyn hooks pname ppats v
-- Record patterns: should have been desugared to PCon by the scheduler.
-- If they reach here (e.g. in a standalone test), fall back to failure.
matchPat hooks (PRecord _ _) _ = pure Nothing
matchPat hooks (PRecordWild _) _ = pure Nothing
-- ViewPatterns: (f -> p) matches v when f v matches p.
-- We evaluate f v and then match the result against p.
matchPat hooks (PView fn p) v = do
    -- f is an Expr; we need an Env to evaluate it. We don't have the
    -- env here, so ViewPatterns MUST be desugared before reaching Eval.
    -- This case is a safeguard — in practice desugarRecordPats converts
    -- PView into a case expression via desugarViewPat in the scheduler.
    error ("IHC.Eval: PView reached matchPat — view pattern not desugared: "
            <> show fn <> " -> " <> show p)

-- | Sentinel key used to carry the owning-module name through 'Env'.
-- The closure constructed for a top-level binding @M.foo@ has its
-- 'env' extended with @ownerSentinelKey -> VStr "M"@; sub-closures that
-- extend that env (lambdas, lets) inherit the binding automatically.
-- The EVar fallback path reads this key via 'currentOwner' to scope
-- unqualified-name resolution to @M@'s actual import declarations,
-- per Haskell 2010 §5.5.  The @"$$"@ prefix matches existing IHC
-- sigil conventions (@$fldProj$name@, @$dotdot@) and is unambiguous —
-- no real Haskell identifier starts with @$$@.
ownerSentinelKey :: ByteString
ownerSentinelKey = BC.pack "$$owner"

-- | Read the owning module from 'Env', if the sentinel is present.
-- Returns 'Nothing' for envs that haven't had the sentinel installed
-- (REPL transient evals, certain entry-boundary paths) — those will
-- fall through to the unscoped legacy fallback.
currentOwner :: IHCHooks -> Env -> IO (Maybe ByteString)
currentOwner hooks env = case lookupEnv ownerSentinelKey env of
    Nothing -> pure Nothing
    Just t  -> do
        v <- force hooks t
        case v of
            VStr m -> pure (Just m)
            _      -> pure Nothing

-- | Optimistic OverloadedStrings bridge for source-loaded bytestring code.
-- String literals stay as real [Char] lists until a consumer demands a
-- narrower representation.  Data.ByteString functions pattern-match on the
-- real @BS ForeignPtr Int@ constructor, so materialize a Char list or
-- transitional VStr into that constructor at the pattern boundary.
charListToByteStringVal :: IHCHooks -> Val -> IO (Maybe Val)
charListToByteStringVal hooks (VCon "BS" _) = pure Nothing
charListToByteStringVal hooks (VStr bs) = Just <$> byteStringConFromBS bs
charListToByteStringVal hooks v = do
    chars <- go [] v
    case chars of
        Nothing -> pure Nothing
        Just cs -> Just <$> byteStringConFromBS (BC.pack (reverse cs))
  where
    go acc (VCon "[]" []) = pure (Just acc)
    go acc (VCon ":" [hT, tT]) = do
        hv <- force hooks hT
        case hv of
            VChar c -> do
                tv <- force hooks tT
                go (c : acc) tv
            _ -> pure Nothing
    go acc (VStr bs) = pure (Just (reverse (BC.unpack bs) ++ acc))
    go _ _ = pure Nothing

byteStringConFromBS :: ByteString -> IO Val
byteStringConFromBS bs = do
    let len = BS.length bs
    fp <- mallocForeignPtrBytes len
    withForeignPtr fp $ \dst ->
        BS.useAsCStringLen bs $ \(src, n) ->
            copyBytes (castPtr dst) (castPtr src) n
    fpT <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    lenT <- newWHNFThunk (VInt (fromIntegral len))
    pure (VCon "BS" [fpT, lenT])

matchFields :: IHCHooks -> [(Pat, Thunk)] -> [(Name, Thunk)] -> IO (Maybe [(Name, Thunk)])
matchFields _     [] acc = pure (Just (reverse acc))
matchFields hooks ((PVar nm, t) : rest) acc =
    matchFields hooks rest ((nm, t) : acc)
matchFields hooks ((PWild, _) : rest) acc =
    matchFields hooks rest acc
matchFields hooks ((pat, t) : rest) acc = do
    fv <- force hooks t
    m  <- matchPat hooks pat fv
    case m of
        Nothing   -> pure Nothing
        Just subs -> matchFields hooks rest (reverse subs ++ acc)

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

apply :: IHCHooks -> Val -> Thunk -> IO Val
apply _     (VFun f)                    arg = f arg
apply _     (VFunIP _ f)                arg = f Map.empty arg
apply _     (VClassMethod _ _ tags go)  arg = go tags arg
-- Source may see the compiler/runtime state-token newtype constructors
-- as constructor-shaped values rather than the builtin constructor
-- functions. Applying the nullary shell should build the one-field
-- wrapper that matchPat/runIOVal already know how to deconstruct.
apply _     (VCon n [])                 arg
    | isStateTokenNewtypeCtor n = pure (VCon n [arg])
-- Newtype-transparent application: a single-field 'VCon' built from a
-- newtype constructor (e.g. @ParsecT body@) is operationally equivalent
-- to its inner field at GHC runtime.  Some IHC code paths return the
-- wrapped 'VCon' instead of unwrapping it; if a caller then tries to
-- apply that 'VCon' as a function, project the field and retry.  Other
-- 'VCon' shapes (multi-field, enum-like) still error.
apply hooks (VCon _ [innerT])           arg = do
    inner <- force hooks innerT
    apply hooks inner arg
apply _     v                           _   = error ("IHC.Eval.apply: not a function: "
                                   <> showValForDebug v)

-- | Apply with the caller's ImplicitParamMap — used by EApp so that
-- implicit params flow from the call site into the callee.
applyIP :: IHCHooks -> ImplicitParamMap -> Val -> Thunk -> IO Val
applyIP _     _         (VFun f)                   arg = f arg
applyIP _     callerIPM (VFunIP _ f)               arg = f callerIPM arg
applyIP _     _         (VClassMethod _ _ tags go) arg = go tags arg
applyIP _     _         (VCon n [])                arg
    | isStateTokenNewtypeCtor n = pure (VCon n [arg])
-- Newtype-transparent application: see note on 'apply' above.
applyIP hooks ipm       (VCon _ [innerT])          arg = do
    inner <- force hooks innerT
    applyIP hooks ipm inner arg
applyIP hooks _         v                          arg  = do
    a <- force hooks arg
    error ("IHC.Eval.applyIP: not a function: "
           <> showValForDebug v <> " applied to " <> showValForDebug a)

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

evalDo :: IHCHooks -> Env -> ImplicitParamMap -> [Stmt] -> IO Val
evalDo hooks _   _   []              = pure (VIO (pure VUnit))
evalDo hooks env ipm [SExpr e]       = eval hooks env ipm e
evalDo hooks env ipm [SBind _ e]     =
    -- Haskell forbids a do-block to end in a bind statement, but be
    -- defensive: evaluate the action and return its value directly.
    eval hooks env ipm e
evalDo hooks _   _   [SLet _]        = pure (VIO (pure VUnit))
evalDo hooks env ipm (SExpr e : rest) =
    pure $ VIO $ do
        mv <- eval hooks env ipm e
        _  <- runIOVal hooks mv                   -- run and discard
        restV <- evalDo hooks env ipm rest
        runIOVal hooks restV
evalDo hooks env ipm (SBind name e : rest) =
    pure $ VIO $ do
        mv <- eval hooks env ipm e
        v  <- runIOVal hooks mv
        vT <- newWHNFThunk v
        let env' = extendEnv name vT env
        restV <- evalDo hooks env' ipm rest
        runIOVal hooks restV
evalDo hooks env ipm [SBangBind _ e] =
    -- Defensive: a do-block ending in a (bang-)bind is ill-formed; mirror SBind.
    eval hooks env ipm e
evalDo hooks env ipm (SBangBind name e : rest) =
    -- Per Haskell Report §3.17.2 + GHC BangPatterns: !x <- m forces the
    -- bound result to WHNF before the rest of the do-block runs. The
    -- parser desugars do-blocks to (>>=)/(>>)/lambda chains, so this
    -- branch is only hit on the defensive EDo fallback path; we still
    -- preserve the strictness contract here for completeness.
    pure $ VIO $ do
        mv <- eval hooks env ipm e
        v  <- runIOVal hooks mv
        vT <- newWHNFThunk v
        _  <- force hooks vT  -- bang: force to WHNF before continuing
        let env' = extendEnv name vT env
        restV <- evalDo hooks env' ipm rest
        runIOVal hooks restV
evalDo hooks env ipm (SLet bs : rest) = do
    -- Same tying-the-knot pattern as 'ELet', but we're inside a
    -- do-block so the scope is the rest of the stmts (not a body expr).
    slots <- mapM (\_ -> newIORef (BlackHole "<do-let-placeholder>")) bs
    let names = map fst bs
        env'  = extendEnvMany (zip names slots) env
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env' ipm rhs)))
          (zip bs slots)
    evalDo hooks env' ipm rest
evalDo hooks _   _   [SImplicitLet _] = pure (VIO (pure VUnit))
evalDo hooks env ipm (SImplicitLet bs : rest) = do
    slots <- mapM (\_ -> newIORef (BlackHole "<do-implicit-let-placeholder>")) bs
    let names = map fst bs
        ipm'  = foldr (\(n, sl) m -> extendIPMap n sl m) ipm
                      (zip names slots)
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env ipm rhs)))
          (zip bs slots)
    evalDo hooks env ipm' rest

-- | Force one 'IO' layer to execute its suspended action. Any other value
-- is returned as-is (treated as a "pure" IO result -- shouldn't happen
-- in well-typed code, but we're optimistic and permissive).
--
-- Important: do not recursively run a VIO returned by the action.  In
-- Haskell, @IO (IO a)@ is a valid action whose result is another action;
-- the inner action only runs if the program explicitly binds/runs it.
runIOVal :: IHCHooks -> Val -> IO Val
runIOVal _     (VIO io) = io
-- Source-constructed IO actions: `IO $ \s -> (# s', a #)`
-- The thunk wraps a function from State# to unboxed tuple.
runIOVal hooks (VCon "IO" [ft]) = do
    fv <- force hooks ft
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    result <- apply hooks fv rwT
    case result of
        VCon _ [_stT, resT] -> force hooks resT
        other               -> pure other
-- @STM a@ is a newtype wrapper around @State# RealWorld -> (# State# RealWorld, a #)@
-- (see 'GHC.Conc.STM').  Source-loaded STM actions arrive as
-- 'VCon "STM" [stateFn]'; if we don't unwrap them here, callers that
-- expect a plain value (e.g. 'atomically' chains, or any 'do'-bind
-- inside the warp Counter / time-manager paths) end up working on the
-- wrapper instead of its result and dispatch breaks downstream.
runIOVal hooks (VCon "STM" [ft]) = do
    fv <- force hooks ft
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    result <- apply hooks fv rwT
    case result of
        VCon _ [_stT, resT] -> force hooks resT
        other               -> pure other
runIOVal _     v        = pure v

isStateTokenNewtypeCtor :: Name -> Bool
isStateTokenNewtypeCtor n =
       n == BC.pack "IO"
    || n == BC.pack "ST"
    || n == BC.pack "STM"

--------------------------------------------------------------------------------
-- Phase 2.12: TemplateHaskellQuotes — [| expr |] evaluation
--
-- evalQuote converts an IHC AST Expr into a TH Exp-shaped Val without
-- evaluating the expression. This mirrors the encoding used by liftVal /
-- thExpToExpr in IHC.TH so that $( [| e |] ) round-trips correctly.
-- We define this here (not in IHC.TH) to avoid a module cycle since
-- IHC.TH already imports IHC.Eval.
--------------------------------------------------------------------------------

evalQuote :: Expr -> IO Val
evalQuote (EVar n)
    -- Capitalised name → ConE; lowercase/operator → VarE.
    | not (BC.null n) && BC.head n >= 'A' && BC.head n <= 'Z' = do
        nt <- newWHNFThunk (VStr n)
        pure (VCon "ConE" [nt])
    | otherwise = do
        nt <- newWHNFThunk (VStr n)
        pure (VCon "VarE" [nt])
evalQuote (ELit (LInt n)) = do
    nT   <- newWHNFThunk (VInt n)
    litT <- newWHNFThunk (VCon "IntegerL" [nT])
    pure (VCon "LitE" [litT])
evalQuote (ELit (LFloat d)) = do
    -- Store as IntegerL(round) — RationalL needs more infra for MVP.
    nT   <- newWHNFThunk (VInt (round d))
    litT <- newWHNFThunk (VCon "IntegerL" [nT])
    pure (VCon "LitE" [litT])
evalQuote (ELit (LChar c)) = do
    cT   <- newWHNFThunk (VChar c)
    litT <- newWHNFThunk (VCon "CharL" [cT])
    pure (VCon "LitE" [litT])
evalQuote (ELit (LStr bs)) = do
    charListVal <- buildCharList (BC.unpack bs)
    lT   <- newWHNFThunk charListVal
    litT <- newWHNFThunk (VCon "StringL" [lT])
    pure (VCon "LitE" [litT])
  where
    buildCharList []     = pure (VCon "[]" [])
    buildCharList (c:cs) = do
        h    <- newWHNFThunk (VChar c)
        rest <- buildCharList cs
        t    <- newWHNFThunk rest
        pure (VCon ":" [h, t])
evalQuote (EApp f x) = do
    fV <- evalQuote f
    xV <- evalQuote x
    fT <- newWHNFThunk fV
    xT <- newWHNFThunk xV
    pure (VCon "AppE" [fT, xT])
evalQuote (ENeg e) = do
    -- negate x  →  AppE (VarE "negate") (evalQuote x)
    negT <- newWHNFThunk =<< evalQuote (EVar "negate")
    xV   <- evalQuote e
    xT   <- newWHNFThunk xV
    pure (VCon "AppE" [negT, xT])
evalQuote (ETuple es) = do
    elemVals <- mapM evalQuote es
    elemTs   <- mapM newWHNFThunk elemVals
    listVal  <- buildThunkList elemTs
    lT       <- newWHNFThunk listVal
    pure (VCon "TupE" [lT])
  where
    buildThunkList []     = pure (VCon "[]" [])
    buildThunkList (x:xs) = do
        tailVal <- buildThunkList xs
        tailT   <- newWHNFThunk tailVal
        pure (VCon ":" [x, tailT])
-- Unsupported forms: emit a VarE "<unsupported>" placeholder.
evalQuote _ = do
    nt <- newWHNFThunk (VStr "<unsupported>")
    pure (VCon "VarE" [nt])
