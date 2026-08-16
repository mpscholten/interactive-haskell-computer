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
{-# LANGUAGE ScopedTypeVariables #-}
module IHC.Eval
    ( eval
    , force
    , forceMethodVal
    , apply
    , matchPat
    , runIOVal
    , ownerSentinelKey
    , currentOwner
    , hostQuasiMethodVal
    , thQuotedName
    , runLeftoverQAction
    , applyLeftoverStateFun
    , isLeftoverStateResult
    ) where

import Control.Exception (bracket_, throwIO)
import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Bits ((.&.))
import Data.Char (ord, toLower)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IORef
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, intPtrToPtr, nullPtr, plusPtr, ptrToIntPtr)
import qualified Foreign.Storable as FStorable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)

import IHC.AST
import IHC.Classes (ClassRegistry, IHCHooks, legacyHooks, normalizeTyTag, typeTagOf, lookupEnvFallback, lookupTypeSigFallback, lookupInstanceMethod, lookupInstanceMethodPattern, getSharedClassReg, triggerCoreInstanceLoad, triggerCoreInstanceLoadForTag, lookupClassMethodFallback, runThExpToExpr, pushDoCarrier, popDoCarrier, currentDoCarrier, takeLastMonadicCarrier, peekLastMonadicCarrier, setLastMonadicCarrier)
import IHC.ConstructorMetadata
    ( ConstructorTypeMetadata(..), ConstructorIdentity(..)
    , constructorMetadata, constructorScheme
    , lookupDeclaredFieldTag
    , selectorFieldTypeAt, globalConstructorTypeRegistryRef )
import IHC.Diagnostics (noteBlackHoleWait, noteForceEval, noteForceKind)
import qualified IHC.Elaborate as Elab
import qualified IHC.PatSyn as PatSyn
import qualified IHC.TypeAST as TA
import IHC.TypeAST (Scheme(..), Type(..), Pred(..), tyArrowArgs, tyHead, schemeIsResultPolymorphic, freeTyVars, expandTypeSynonyms)
import IHC.TypeGlobals (globalTypeSigsRef, globalTypeSynonymsRef, globalClassMethodNamesRef, globalAmbiguousSigsRef, globalMethodClassRef)
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

data CalleeElaboration
    = CalleeNotAttempted
    | CalleeElaborated !Expr
    | CalleeAttemptFailed

-- | If @x@ is a bare nullary 'Bounded' method (@maxBound@ / @minBound@) and
-- @f@ is a variable whose registered type signature begins with a
-- concrete type constructor (@Int -> …@), rewrite @x@ to @x :: T@ so
-- the existing 'ETyApp' nullary path can resolve the instance.
--
-- Mirrors GHC specialising @measureOff maxBound@ from
-- @measureOff :: Int -> Text -> Int@.  Does not invent a default type
-- when the callee signature is missing or polymorphic — that would
-- re-break non-Int uses like @listArray (minBound, maxBound)@ for enums.
annotateNullaryBoundedArg :: Expr -> Expr -> IO Expr
annotateNullaryBoundedArg f x = case x of
    EVar m
      | isNullaryBoundedName m -> case f of
            EVar fname -> do
                sigs <- readIORef globalTypeSigsRef
                let bareF = lastDottedComponent fname
                pure $ case Map.lookup bareF sigs `orElse` Map.lookup fname sigs of
                    Just (Scheme _ _ body) ->
                        case tyArrowArgs body of
                            (argTy : _, _)
                              | Just con <- tyHead argTy
                              , isMonoCon argTy ->
                                    ETyApp x con
                            _ -> x
                    Nothing -> x
            _ -> pure x
      | otherwise -> pure x
    _ -> pure x
  where
    isNullaryBoundedName n =
        let bare = lastDottedComponent n
        in bare == BC.pack "maxBound" || bare == BC.pack "minBound"
    lastDottedComponent n =
        case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
            Just idx -> BC.drop (idx + 1) n
            Nothing  -> n
    -- Only specialise when the domain is a bare type constructor
    -- (Int, Char, …), not a type application or variable.
    isMonoCon (TA.TyCon _) = True
    isMonoCon _            = False
    orElse (Just a) _ = Just a
    orElse Nothing  b = b

-- | Elaborate an application argument under the parameter type supplied by
-- the callee's signature. This is the bidirectional bridge missing from the
-- optimistic evaluator: result-polymorphic and multi-parameter constrained
-- expressions receive their expected type without naming any library,
-- function, or class.
elaborateExpectedArg :: IHCHooks -> Maybe Name -> Expr -> Expr -> IO Expr
elaborateExpectedArg hooks owner f x = do
    initialSigs <- readIORef globalTypeSigsRef
    methodNames <- readIORef globalClassMethodNamesRef
    ctorTypes <- readIORef globalConstructorTypeRegistryRef
    -- Constructor field types are independent of the cheap class-method
    -- reject: an OverloadedStrings literal (or its desugared cons list)
    -- must still see HostPreference / Pref from the field type.
    mFieldTy <- case appHeadAndArity f of
        Just (fn, supplied)
          | Just (Scheme _ _ body) <- constructorScheme ctorTypes owner fn
          , (args, _) <- tyArrowArgs body
          , supplied < length args ->
                pure (Just (args !! supplied))
          | otherwise -> do
                mSelector <- selectorFieldTypeAt owner fn supplied
                pure $ mSelector <|> do
                    Scheme _ _ body <- Map.lookup fn initialSigs
                        `orElse` Map.lookup (bareName fn) initialSigs
                    let (args, _) = tyArrowArgs body
                    expected <- if supplied < length args
                        then Just (args !! supplied) else Nothing
                    case TA.tyApps expected of
                        (TA.TyCon h, [elemTy])
                            | bareName (normalizeTyTag h) == BC.pack "Ptr"
                            , Just elemHead <- TA.tyHead elemTy
                            , bareName (normalizeTyTag elemHead) == BC.pack "Word8" ->
                                Just expected
                        _ -> Nothing
        Nothing -> pure Nothing
    case mFieldTy of
        Just fieldTy -> elaborateAtExpected fieldTy x
        Nothing -> do
            -- Cheap reject before any signature fallback walk.  Looking up
            -- schemes from a large owner walks a huge re-export graph.
            -- Elaborating a nested ordinary function at the callee's argument
            -- type (because the arg tree mentions a constrained name) can
            -- diverge or leave a leftover function.
            -- `runQ (listE …)`: the argument is an ordinary Quote
            -- combinator, not a class method, so the cheap reject
            -- would skip.  If the callee's next argument type is
            -- Q t (constructor head, not a name list), still
            -- elaborate so `$qWrap` can inhabit Q.
            needs <- if needsExpectedElaboration methodNames initialSigs x
                        then pure True
                        else case x of
                            EApp{} -> calleeArgExpectsQ hooks owner initialSigs f
                            _      -> pure False
            if not needs
                        then pure x
                else do
                    ambiguousSigs <- readIORef globalAmbiguousSigsRef
                    -- Include the partially-applied callee as well as applied
                    -- class-method heads in the argument.  Walking every nested
                    -- ordinary name (e.g. @string@ / @pack@ under megaparsec)
                    -- re-enters a facade ExportModule graph and never returns.
                    (sigs, scopedSigs) <- foldM (loadPreferredSig ambiguousSigs) (initialSigs, Set.empty)
                        (Set.toList (schemeNamesToLoad methodNames f x))
                    stillNeeds <- if needsExpectedElaboration methodNames sigs x
                        then pure True
                        else calleeArgExpectsQ hooks owner sigs f
                    if not stillNeeds
                        then pure x
                        else case appHeadAndArity f of
                        Just (fn, supplied) ->
                            case Elab.lookupScopedScheme ambiguousSigs scopedSigs sigs fn of
                                Just (Scheme _ _ body) ->
                                    case tyArrowArgs body of
                                        (args, _) | supplied < length args -> do
                                            mReg <- getSharedClassReg legacyHooks
                                            syns <- readIORef globalTypeSynonymsRef
                                            case mReg of
                                                Nothing -> pure x
                                                Just classReg -> do
                                                    -- Infer the residual type of the
                                                    -- partially-applied callee first. This
                                                    -- retains substitutions learned from
                                                    -- every earlier argument, unlike merely
                                                    -- indexing the original scheme's arrow
                                                    -- list. For example, after
                                                    -- @same True@, @same :: a -> a -> a@
                                                    -- has residual domain @Bool@.
                                                    residual <- try
                                                        (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                                                            ctorTypes owner scopedSigs
                                                            Elab.InferFreely f)
                                                        :: IO (Either SomeException (Expr, TA.Type))
                                                    let expected = case residual of
                                                            Right (_, residualTy) ->
                                                                case tyArrowArgs residualTy of
                                                                    (argTy : _, _) -> argTy
                                                                    _ -> args !! supplied
                                                            Left _ -> args !! supplied
                                                    -- A leftover type variable (the `f` in
                                                    -- `Alternative f => f a -> f a -> f a`)
                                                    -- is not an instance tag.  Rewriting
                                                    -- `try p` / `pure x` to
                                                    -- `ETypedMethod … "f"` made
                                                    -- `try p <|> q` capture a State#
                                                    -- function; `unParser` then saw
                                                    -- `(#,#)` applied to `cok`.
                                                    if not (expectedTypeHasConcreteHead expected)
                                                        then pure x
                                                        else do
                                                            result <- try (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                                                                            ctorTypes owner scopedSigs
                                                                            (Elab.ExpectType expected) x)
                                                                :: IO (Either SomeException (Expr, TA.Type))
                                                            let rewritten = either (const x) fst result
                                                            pure (stampResultPolyExpected sigs expected x rewritten)
                                        _ -> pure x
                                Nothing -> pure x
                        Nothing -> pure x
  where
    elaborateAtExpected expected x0
        | (TA.TyCon h, [elemTy]) <- TA.tyApps expected
        , bareName (normalizeTyTag h) == BC.pack "Ptr"
        , Just elemHead <- TA.tyHead elemTy
        , bareName (normalizeTyTag elemHead) == BC.pack "Word8" =
            pure (ETyApp x0 (BC.pack "Ptr " <> elemHead))
        -- Same rule as the signature-fallback path below: a leftover
        -- tyvar (`a` in `Box a` / `ReaperSettings workload item`) is
        -- not an instance tag.  Rewriting `[]` (parser-shared with
        -- desugared "") to `fromString` / `empty` left a class-method
        -- function in the field; TimeManager.stopManager then mapM_'s
        -- that leftover and hangs.
        | not (expectedTypeHasConcreteHead expected) = pure x0
        | otherwise = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing -> pure x0
            Just classReg -> do
                sigs <- readIORef globalTypeSigsRef
                syns <- readIORef globalTypeSynonymsRef
                ctors <- readIORef globalConstructorTypeRegistryRef
                -- OverloadedStrings inserts fromString; keep the class
                -- scheme scoped so a leftover last-writer instance
                -- cannot hide the result-polymorphic method.
                let scoped = Set.singleton (BC.pack "fromString")
                result <- try (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                                ctors owner scoped
                                (Elab.ExpectType expected) x0)
                    :: IO (Either SomeException (Expr, TA.Type))
                let rewritten = either (const x0) fst result
                pure (stampResultPolyExpected sigs expected x0 rewritten)

    -- Keep a concrete result annotation around an applied constrained alias
    -- such as @fromIntegral n@.  Its runtime representation is otherwise a
    -- bare VInt, so later FiniteBits/Storable dispatch cannot distinguish
    -- Word32/Word8 from Int even though the callee supplied that expected
    -- type.  The annotation reuses the normal ETyApp elaboration path.
    stampResultPolyExpected sigs expected original rewritten
        | Just headName <- appExprHead original
        , Just scheme <- Map.lookup headName sigs
                `orElse` Map.lookup (bareName headName) sigs
        , schemeIsResultPolymorphic scheme
        , Just tag <- TA.tyHead expected = ETyApp rewritten tag
        | otherwise = rewritten

    appExprHead (EApp h _) = appExprHead h
    appExprHead (ETyApp h _) = appExprHead h
    appExprHead (EVar n) = Just n
    appExprHead _ = Nothing

    loadPreferredSig ambiguous state@(known, scoped) name
        | bareName name `elem` map BC.pack [":", "[]"] = pure state
        | not (Set.member (bareName name) ambiguous)
        , Map.member name known || Map.member (bareName name) known = pure state
        | otherwise = do
            mScheme <- lookupTypeSigFallback hooks owner name
            clsMap <- readIORef globalMethodClassRef
            methodNames <- readIORef globalClassMethodNamesRef
            pure $ case mScheme of
                Nothing
                    | Set.member (bareName name) ambiguous ->
                        ( Map.delete name (Map.delete (bareName name) known)
                        , scoped
                        )
                    | otherwise -> state
                Just scheme
                    | not (Elab.schemeBelongsToClassMethod methodNames clsMap name scheme) ->
                        ( Map.delete name (Map.delete (bareName name) known)
                        , scoped
                        )
                    | otherwise ->
                        let bare = bareName name
                        in ( Map.insert name scheme (fst state)
                           , Set.insert bare (Set.insert name scoped)
                           )

    schemeNamesToLoad methodNames callee arg =
        Set.union (calleeHeads callee) (classMethodHeads methodNames arg)

    calleeHeads = go
      where
        go (EVar name) = Set.singleton name
        go (EApp innerF _) = go innerF
        go (ETyApp inner _) = go inner
        -- InferFreely of a constrained `runQ` (Quasi m => Q a -> m a)
        -- wraps the head as EConstrainedValue.  Expected-arg wrap of
        -- `[| e |]` still needs the original name so domain Q is visible.
        go (EConstrainedValue inner _) = go inner
        go _ = Set.empty

    classMethodHeads methodNames = go
      where
        go (EVar name)
            | Set.member (bareName name) methodNames = Set.singleton name
            | otherwise = Set.empty
        go (EApp innerF innerX) = Set.union (go innerF) (go innerX)
        go (ETyApp inner _) = go inner
        go _ = Set.empty

    -- Only *result-polymorphic* applied class-method heads need the
    -- callee's expected type (@f (pure x)@, @fromInteger n@).
    -- Argument-directed methods (@sizeOf (0 :: Int)@, @show x@) must
    -- not trigger: elaborating them at an outer callee's type drains
    -- the class catalogue.
    needsExpectedElaboration methodNames sigs = goApplied
      where
        -- OverloadedStrings: a string literal (LStr or desugared
        -- cons-list of LChar) at a non-[Char] expected type
        -- (callee argument, constructor field, annotation) must
        -- elaborate.  Desugared @"…"@ is an @(:)@ spine; matching
        -- 'EApp' first treated it as an ordinary @:@ apply and
        -- skipped fromString at `h1 "Hello world"`.
        goApplied e | Elab.isStringLiteralExpr e = True
        -- A quote buried in a cons spine (`listE [ [| e |], … ]`)
        -- must see the callee domain so expected-Q wrap can fire on
        -- each element.  Matching `EApp` first would only look at
        -- whether `:` is a result-poly class method (it is not).
        goApplied e | listSpineHasQuote e = True
        goApplied (EApp innerF _) = goHead innerF
        goApplied (ETyApp inner _) = goApplied inner
        goApplied (ELocalSig _ inner) = goApplied inner
        -- Num literals at a non-Int expected type (Size == 0).
        goApplied (ELit (LInt _)) = True
        goApplied (ELit (LInteger _)) = True
        -- A quotation at a callee argument (`runQ [| e |]`) must see
        -- the parameter type.  Expected Q wraps via `$qWrap (pure _)`;
        -- expected Exp / a tyvar stays raw.  Not a runQ name special
        -- case — any callee whose domain is Q t takes this path.
        goApplied EQuote{} = True
        goApplied _ = False

        listSpineHasQuote (ETyApp inner _) = listSpineHasQuote inner
        listSpineHasQuote (ELocalSig _ inner) = listSpineHasQuote inner
        listSpineHasQuote (EApp (EApp (EVar cons) hd) tl)
            | isListConsName cons = isQuoteExpr hd || listSpineHasQuote tl
        listSpineHasQuote _ = False
        isListConsName cons =
            let b = bareName cons
            in b == BC.pack ":" || BC.isSuffixOf (BC.pack ".:") cons
        isQuoteExpr (EQuote _) = True
        isQuoteExpr (ETyApp inner _) = isQuoteExpr inner
        isQuoteExpr (ELocalSig _ inner) = isQuoteExpr inner
        isQuoteExpr _ = False

        goHead (EVar name) =
            case Map.lookup name sigs `orElse` Map.lookup (bareName name) sigs of
                Just scheme
                    | schemeIsResultPolymorphic scheme -> True
                    -- Argument-directed class schemes (`toMarkup ::
                    -- ToMarkup a => a -> Markup`) must not be rewritten
                    -- at the caller's result type.  That picked
                    -- `ToMarkup Markup` (`id`) for
                    -- `renderHtml (toMarkup (s :: String))` and left
                    -- `go` a leftover function.
                    | schemeIsArgumentDirected scheme -> False
                    -- Last-writer may be a monomorphic instance method
                    -- (`toMarkup = string :: String -> Markup`) that hid
                    -- the class scheme.  That signature has no class
                    -- parameter in the result — do not rewrite at the
                    -- outer expected type (`ToMarkup Markup` = `id`).
                    | otherwise -> False
                -- Scheme not loaded yet: keep looking (the caller
                -- loads signatures and re-asks).  Result-poly methods
                -- still need that second pass.  A *qualified* miss
                -- must not inherit the bare last-writer
                -- (`Data.Set.fromList` vs `IsList.fromList`):
                -- InferFreely of the class scheme at @Set Text@
                -- walks @Item@ and hangs before main.
                Nothing
                    | not (Set.member (bareName name) methodNames) -> False
                    | isQualifiedName name -> False
                    | otherwise -> True
        goHead (EApp innerF _) = goHead innerF
        goHead (ETyApp inner _) = goHead inner
        goHead _ = False

        isQualifiedName n = case BC.elemIndexEnd '.' n of
            Just i -> i > 0 && i + 1 < BC.length n
            Nothing -> False

    appHeadAndArity = go 0
      where
        go n (EApp h _) = go (n + 1) h
        go n (EVar fn)  = Just (fn, n)
        go n (ETyApp h _) = go n h
        go _ _ = Nothing
    orElse (Just a) _ = Just a
    orElse Nothing  b = b

    -- True when the next argument the callee wants is a Q-headed type
    -- (`runQ :: Quasi m => Q a -> m a`).  Constructor head after
    -- taking already-applied args — not a runQ name list.
    calleeArgExpectsQ hooks' owner' sigs callee = case appHeadAndArity callee of
        Just (fn, supplied) -> do
            mScheme <- case Map.lookup fn sigs `orElse` Map.lookup (bareName fn) sigs of
                Just s  -> pure (Just s)
                Nothing -> lookupTypeSigFallback hooks' owner' fn
            pure $ case mScheme of
                Just (Scheme _ _ body) ->
                    case tyArrowArgs body of
                        (args, _) | supplied < length args ->
                            argHeadIsQ (args !! supplied)
                        _ -> False
                Nothing -> False
        Nothing -> pure False
      where
        argHeadIsQ ty = case tyHead ty of
            Just n -> bareName n == BC.pack "Q"
            Nothing -> False

    -- Class parameter lives only in the arguments (@sizeOf :: Storable a => a -> Int@).
    schemeIsArgumentDirected (Scheme _ preds body) =
        let (args, result) = tyArrowArgs body
            resultVars = freeTyVars result
            argVars = Set.unions (map freeTyVars args)
            predVars = Set.unions (map predTyVars preds)
        in not (Set.null (Set.intersection predVars argVars))
           && Set.null (Set.intersection predVars resultVars)
      where
        predTyVars (TA.Pred _ ts) = Set.unions (map freeTyVars ts)
        predTyVars (TA.QPred vs ctx p) =
            Set.difference
                (Set.unions (predTyVars p : map predTyVars ctx))
                (Set.fromList vs)

-- | True when the expected type's head is a real constructor, not a
-- leftover tyvar (`f a` from `Alternative f => f a -> f a -> f a`).
expectedTypeHasConcreteHead :: TA.Type -> Bool
expectedTypeHasConcreteHead ty =
    case tyHead ty of
        Just n -> case BC.uncons n of
            Just (c, _) -> (c >= 'A' && c <= 'Z') || c == '[' || c == '('
            Nothing     -> False
        Nothing -> False

-- | Compiler-intrinsic Quasi methods.
--
-- @location = Q qLocation@ and @instance Quasi Q where qLocation = location@
-- are a source knot.  @instance Quasi IO@ stubs the same methods with
-- @badIO@.  Neither can produce a 'Loc'.  GHC fills this dictionary in the
-- compiler; IHC does the same when the method is forced, without replacing
-- the source @location@ function.  The Q newtype still wraps the action so
-- the enclosing carrier stays @Q a@.
--
-- @loc_start@ / @loc_filename@ stay source record selectors.  The Loc
-- value uses the source constructor layout so owner-scoped schemes
-- ('lmFieldSchemes') project the right fields.
hostQuasiMethodVal :: Name -> Maybe Val
hostQuasiMethodVal method
    | method == BC.pack "qLocation" =
        Just (VIO buildHostLoc)
    | method == BC.pack "qExtsEnabled" =
        Just (VIO (pure (VCon "[]" [])))
    | method == BC.pack "qIsExtEnabled" =
        Just (VFun $ \_extT -> pure (VIO (pure (VCon "False" []))))
    | otherwise = Nothing

isQuasiClass :: Name -> Bool
isQuasiClass cls = bareName cls == BC.pack "Quasi"

-- | Dummy splice location.  Line/column are 1-based so megaparsec's
-- @mkPos@ (used by HSX's @findHSXPosition@) does not throw
-- 'InvalidPosException'.
buildHostLoc :: IO Val
buildHostLoc = do
    filenameT <- newWHNFThunk =<< charListVal "<ihc-no-source-loc>"
    packageT  <- newWHNFThunk =<< charListVal "main"
    moduleT   <- newWHNFThunk =<< charListVal "Main"
    startT    <- newWHNFThunk =<< charPosVal 1 1
    endT      <- newWHNFThunk =<< charPosVal 1 1
    pure (VCon "Loc" [filenameT, packageT, moduleT, startT, endT])

charPosVal :: Int64 -> Int64 -> IO Val
charPosVal line col = do
    lineT <- newWHNFThunk (VInt line)
    colT  <- newWHNFThunk (VInt col)
    pure (VCon "(,)" [lineT, colT])

charListVal :: String -> IO Val
charListVal [] = pure (VCon "[]" [])
charListVal (c:cs) = do
    cT <- newWHNFThunk (VChar c)
    restT <- newWHNFThunk =<< charListVal cs
    pure (VCon ":" [cT, restT])

-- | Evaluate an 'ETypedMethod' node.  Looks up the resolved instance
-- method in the class registry; if the instance registered a
-- 'methodPlaceholder' (class default with no per-instance override),
-- falls back to a known-equivalent method (e.g. Monad.return →
-- Applicative.pure).
resolveTypedMethod :: IHCHooks -> ClassRegistry -> Name -> Name -> Name -> IO Val
resolveTypedMethod hooks reg cls method tag
    | isQuasiClass cls
    , Just host <- hostQuasiMethodVal (bareName method)
    = pure host
    | otherwise = do
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
            triggerCoreInstanceLoadForTag legacyHooks cls tag
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
                        Just v  -> do
                            forced <- forceMethodVal hooks v
                            if not (isPlaceholder forced)
                                then pure forced
                                else noInstance
                        Nothing -> error ("IHC.Eval.ETypedMethod: no instance `"
                                        <> BC.unpack cls <> " " <> BC.unpack tag
                                        <> "` for method `" <> BC.unpack method <> "`")
  where
    tryResolve = do
        mMethod <- lookupInstanceMethod reg cls tag method
        case mMethod of
            Just v -> do
                forced <- forceMethodVal hooks v
                if not (isPlaceholder forced)
                    then pure (Just forced)
                    else tryPattern
            _ -> tryPattern

    -- Exact `MarkupM ()` misses `instance (a ~ ()) => IsString (MarkupM a)`
    -- registered as `MarkupM a`.  The dispatcher already uses this
    -- structural matcher; ETypedMethod must too or the pin is leftover.
    tryPattern = do
        mPat <- lookupInstanceMethodPattern reg cls tag method
        case mPat of
            Just v -> do
                forced <- forceMethodVal hooks v
                if not (isPlaceholder forced)
                    then pure (Just forced)
                    else tryFallbacks (fallbackList cls method)
            _ -> tryFallbacks (fallbackList cls method)

    isPlaceholder (VCon n []) =
        BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n
    isPlaceholder _ = False

    tryFallbacks [] = pure Nothing
    tryFallbacks ((c, m) : rest) = do
        v <- lookupInstanceMethod reg c tag m
        case v of
            Just vv -> do
                forced <- forceMethodVal hooks vv
                if not (isPlaceholder forced)
                    then pure (Just forced)
                    else tryFallbacks rest
            _ -> tryFallbacks rest

    fallbackList = typedMethodFallbacks

    noInstance = error ("IHC.Eval.ETypedMethod: no instance `"
                      <> BC.unpack cls <> " " <> BC.unpack tag
                      <> "` for method `" <> BC.unpack method <> "`")

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
        Evaluated v -> do
            -- Already-memoised path: still count so tight loops over
            -- forced method thunks (e.g. default Eq mutual recursion)
            -- show up under IHC_EVAL_STATS.
            noteForceKind "whnf" (valKindTag v)
            pure v
        BlackHole owner msg mWait -> do
            self <- myThreadId
            case owner of
                -- The black-hole was entered by a DIFFERENT thread that is
                -- still evaluating this shared thunk (warp forks a thread per
                -- connection plus timeout/date threads, all forcing shared
                -- 'Settings'/etc. thunks). This is NOT a loop — GHC's RTS
                -- blocks on a foreign black-hole. A tight 'yield' loop
                -- holds the capability, so the owner (often parked on
                -- takeMVar# / delay# / a Handle lock) never runs and
                -- forkIO's parent never resumes. Block on the wait-queue
                -- instead, then retry until the owner publishes.
                Just o | o /= self -> do
                    noteBlackHoleWait
                    case mWait of
                        Just w  -> readMVar w
                        Nothing -> threadDelay 1000
                    force hooks t
                -- Same thread re-entered (a genuine `<<loop>>`), or an
                -- owner-less knot-tying placeholder was demanded before it
                -- was filled: that IS a real loop.
                _ -> throwIO (LoopException msg)
        Unevaluated clo@(Closure env ipm expr) ->
            enterThunk hooks t (Unevaluated clo) (take 500 (show expr)) $ do
                noteForceEval (show expr)
                eval hooks env ipm expr
        TypedField canonical _ _ -> force hooks canonical
        -- Lazy-init builtin: run the host @IO Val@ action exactly once,
        -- then memoise. Mirrors the 'Unevaluated' path (same black-hole
        -- protocol) so a concurrent forcer waits (foreign owner) or sees a
        -- loop (same thread). See 'IHC.Val.newLazyBuiltinThunk'.
        LazyBuiltin mkV ->
            enterThunk hooks t (LazyBuiltin mkV) "<lazy-builtin>" $ do
                noteForceKind "lazy" "<lazy-builtin>"
                mkV

-- | Claim a thunk for evaluation: install a black-hole with a wait-queue,
-- run 'compute', publish 'Evaluated', and wake foreign waiters. If another
-- thread already entered, drop the claim and re-enter 'force'. On
-- exception restore the prior state so a waiter can take over instead of
-- spinning on a dead owner's full MVar.
enterThunk :: IHCHooks -> Thunk -> ThunkState -> String -> IO Val -> IO Val
enterThunk hooks t restore msg compute = do
    self <- myThreadId
    w <- newEmptyMVar
    won <- atomicModifyIORef' t $ \st -> case st of
        Unevaluated _ -> (BlackHole (Just self) msg (Just w), True)
        LazyBuiltin _ -> (BlackHole (Just self) msg (Just w), True)
        _             -> (st, False)
    if not won
        then force hooks t
        else do
            ev <- try compute
            case ev of
                Right v -> do
                    writeIORef t (Evaluated v)
                    _ <- tryPutMVar w ()
                    pure v
                Left (e :: SomeException) -> do
                    writeIORef t restore
                    _ <- tryPutMVar w ()
                    throwIO e

-- | Force a thunk in an implicit-param context.
--
-- GHC elaborates @(?x :: t) => a@ as a dictionary-passing function, so a
-- nullary binding @f = ?x@ reads @?x@ from the *call site*, not from the
-- empty definition-site map closed over the CAF.  Lambdas already do this
-- via 'VFunIP' + 'applyIP'.  CAF / do-block bindings do not: 'force' evals
-- them with the captured (usually empty) map and memoises the result.
--
-- When the caller has implicit params the thunk did not capture:
--
--   * Lambda-headed bindings stay on the 'force' path.  'ELam' must
--     capture the *definition-site* map so 'applyIP' can merge the
--     true call-site map (lexical wins).  Evaluating the lambda under
--     the caller's map would bake that map in and shadow later sites.
--   * Everything else is re-evaluated with @thunkIPM ∪ callerIPM@
--     (lexical wins).  IP-dependent results are not memoised — they
--     are functions of the call-site map, like GHC's dictionaries.
forceWithIPM :: IHCHooks -> ImplicitParamMap -> Thunk -> IO Val
forceWithIPM hooks callerIPM t
    | Map.null callerIPM = force hooks t
    | otherwise = do
        st <- readIORef t
        case st of
            Unevaluated clo@(Closure env thunkIPM expr)
                | Map.null (Map.difference callerIPM thunkIPM) ->
                    force hooks t
                | isLambdaHead expr ->
                    force hooks t
                | otherwise -> do
                    let merged = Map.union thunkIPM callerIPM
                    if exprHasFreeImplicitRefs expr
                        then eval hooks env merged expr
                        else
                            enterThunk hooks t (Unevaluated clo) (take 500 (show expr)) $ do
                                noteForceEval (show expr)
                                eval hooks env merged expr
            _ -> force hooks t

-- | @\\x -> …@ (plus signature / type-app wrappers).  These become
-- 'VFunIP' and receive the caller's implicit-param map at apply time.
isLambdaHead :: Expr -> Bool
isLambdaHead (ELam _ _)              = True
isLambdaHead (ELocalSig _ e)         = isLambdaHead e
isLambdaHead (ETyApp e _)            = isLambdaHead e
isLambdaHead (EConstrainedValue e _) = isLambdaHead e
isLambdaHead _                       = False

-- | GHC discharges HasCallStack with emptyCallStack when the caller
-- has no incoming stack.  The type synonym is
--   type HasCallStack = (?callStack :: CallStack)
-- so the IP name is `callStack` after expansion — not a list of
-- functions that mention HasCallStack.  Other unbound IPs still error.
defaultUnboundImplicit :: IHCHooks -> Env -> Name -> IO Val
defaultUnboundImplicit hooks env name
    | name == BC.pack "callStack" = do
        mT <- case lookupEnv emptyName env of
            Just t -> pure (Just t)
            Nothing -> do
                owner <- currentOwner hooks env
                lookupEnvFallback legacyHooks owner emptyName
        case mT of
            Just t  -> force hooks t
            Nothing -> pure (VCon emptyCtor [])
    | otherwise =
        error ("IHC.Eval: implicit parameter `?"
               <> BC.unpack name <> "` is not in scope")
  where
    emptyName = BC.pack "emptyCallStack"
    emptyCtor = BC.pack "EmptyCallStack"

-- ErrorCall is a bidirectional pattern synonym over
-- ErrorCallWithLocation.  raise# / forceToException often leave the
-- payload as a String (VStr / [Char]) or SomeException wrapping one.
-- Constructor-pattern catch handlers (`ErrorCall s`,
-- `ErrorCallWithLocation s _`) must see that payload instead of
-- leftover PatternMatchFail on CallStack / EmptyCallStack /
-- PushCallStack (or a re-raised IhcException).
isErrorCallPat :: Name -> Bool
isErrorCallPat n = bareConName n == BC.pack "ErrorCall"

isErrorCallWithLocationCon :: Name -> Bool
isErrorCallWithLocationCon n = bareConName n == BC.pack "ErrorCallWithLocation"

isPushedOrFrozenCallStack :: Name -> Bool
isPushedOrFrozenCallStack n =
    let b = bareConName n
    in b == BC.pack "PushCallStack" || b == BC.pack "FreezeCallStack"

-- Function-shaped leftover, not an Int/String/user ADT.  IHC never
-- synthesises HasCallStack frames, so these are the empty stack.
isLeftoverCallStackVal :: Val -> Bool
isLeftoverCallStackVal (VFun _) = True
isLeftoverCallStackVal (VFunIP _ _) = True
isLeftoverCallStackVal VClassMethod{} = True
isLeftoverCallStackVal (VCon n []) =
    bareConName n == BC.pack "CallStack"
isLeftoverCallStackVal _ = False

-- raise# shortcut / extractExceptionMessage leave the message as VStr
-- or a [Char] spine.  Bind that as the ErrorCall String field.
isStringPayload :: Val -> Bool
isStringPayload VStr{} = True
isStringPayload (VCon n _) =
    let b = bareConName n
    in b == BC.pack ":" || b == BC.pack "[]"
isStringPayload _ = False

stringPayloadThunk :: Val -> IO Thunk
stringPayloadThunk v = newWHNFThunk v

emptyStringThunk :: IO Thunk
emptyStringThunk = newWHNFThunk (VCon "[]" [])

-- | Source @popCallStack@ errors on EmptyCallStack because GHC always
-- synthesises the current HasCallStack call-site.  IHC never synthesises
-- frames: 'defaultUnboundImplicit' discharges @?callStack@ as
-- EmptyCallStack, and @withFrozenCallStack@ is
-- @freezeCallStack (popCallStack callStack)@.  Popping empty is
-- therefore the GHC-equivalent of popping the last remaining frame —
-- identity on EmptyCallStack.  Push / Freeze still go through source.
wrapPopEmptyCallStack :: IHCHooks -> ImplicitParamMap -> Name -> Val -> Val
wrapPopEmptyCallStack hooks ipm name src
    | bareName name == popName =
        VFunIP ipm $ \callerIPM argT -> do
            v <- force hooks argT
            case v of
                VCon n _
                    | bareName n == emptyCtor -> pure v
                    | isPushedOrFrozenCallStack n ->
                        applyIP hooks callerIPM src argT
                _
                    | isLeftoverCallStackVal v -> pure v
                    | otherwise -> applyIP hooks callerIPM src argT
    | otherwise = src
  where
    popName   = BC.pack "popCallStack"
    emptyCtor = BC.pack "EmptyCallStack"

-- | True when evaluating this expression (now or in a delayed subform)
-- can read a free @?name@.  Used to refuse memoising IP-dependent CAF
-- results so a later @let ?x = …@ sees its own binding.
exprHasFreeImplicitRefs :: Expr -> Bool
exprHasFreeImplicitRefs = go Set.empty
  where
    go bound = \case
        EImplicitRef n -> n `Set.notMember` bound
        EImplicitLet bs body ->
            let bound' = bound <> Set.fromList (map fst bs)
            in any (go bound . snd) bs || go bound' body
        EApp f x             -> go bound f || go bound x
        ELam _ body          -> go bound body
        ELet bs body         -> any (go bound . snd) bs || go bound body
        ECase s alts         -> go bound s || any (\(Alt _ e) -> go bound e) alts
        EIf c th el          -> go bound c || go bound th || go bound el
        EDo stmts            -> goStmts bound stmts
        ENeg e               -> go bound e
        ETuple es            -> any (go bound) es
        ERecordCon _ fs      -> any (go bound . snd) fs
        ERecordUpdate e fs   -> go bound e || any (go bound . snd) fs
        ESplice e            -> go bound e
        EQuote e             -> go bound e
        ETyApp e _           -> go bound e
        ELocalSig _ e        -> go bound e
        EConstrainedValue e _ -> go bound e
        EVar{}               -> False
        ELit{}               -> False
        ELabel{}             -> False
        EGuardFail           -> False
        ERecordWild{}        -> False
        EQuasiQuote{}        -> False
        ETypedMethod{}       -> False

    goStmts _ [] = False
    goStmts bound (s:ss) = case s of
        SExpr e -> go bound e || goStmts bound ss
        SBind _ e -> go bound e || goStmts bound ss
        SBangBind _ e -> go bound e || goStmts bound ss
        SLet bs -> any (go bound . snd) bs || goStmts bound ss
        SImplicitLet bs ->
            let bound' = bound <> Set.fromList (map fst bs)
            in any (go bound . snd) bs || goStmts bound' ss

-- | Cheap tag for WHNF force samples (no deep show).
valKindTag :: Val -> String
valKindTag = \case
    VInt _            -> "VInt"
    VInteger _        -> "VInteger"
    VFloat _          -> "VFloat"
    VChar _           -> "VChar"
    VStr _            -> "VStr"
    VUnit             -> "VUnit"
    VFun _            -> "VFun"
    VFieldAccessor n _ _ _ -> "VFieldAccessor:" <> BC.unpack n
    VFunIP _ _        -> "VFunIP"
    VCon n _          -> "VCon:" <> BC.unpack n
    VIO _             -> "VIO"
    VPrimObj _        -> "VPrimObj"
    VLabel _          -> "VLabel"
    VClassMethod n _ ts _ ->
        "VClassMethod:" <> BC.unpack n <> "@" <> show (length ts)
    VLazyMethod _     -> "VLazyMethod"

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

eval :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Val
eval hooks env ipm = go
  where
    hostTypedPokeMethod cls method ty
        | cls /= BC.pack "Storable" = Nothing
        | tyAnnotationHead ty `notElem` ["Ptr", "CSize"] = Nothing
        | method == BC.pack "poke" = Just $ VFun $ \ptrT -> pure $ VFun $ \valT ->
            pure $ VIO (runHostTypedPoke ty ptrT Nothing valT)
        | method `elem` map BC.pack ["pokeByteOff", "pokeElemOff"] =
            Just $ VFun $ \ptrT -> pure $ VFun $ \offT -> pure $ VFun $ \valT ->
                pure $ VIO (runHostTypedPoke ty ptrT (Just (method, offT)) valT)
        | otherwise = Nothing

    runHostTypedPoke ty ptrT mOffset valT = do
        ptrV <- force hooks ptrT
        p <- valToHostPtr ptrV
        value <- force hooks valT >>= unwrapHostPokeValue
        off <- case mOffset of
            Nothing -> pure 0
            Just (method, offT) -> do
                offV <- force hooks offT
                let n = case offV of
                        VInt x -> fromIntegral x
                        VInteger x -> fromInteger x
                        other -> error ("typed poke: offset is not an Int: "
                            <> showValForDebug other)
                pure $ if method == BC.pack "pokeElemOff"
                    then n * hostTypedSize ty else n
        writeHostTypedPoke ty p off value
        pure VUnit

    unwrapHostPokeValue (VCon _ [field]) = force hooks field >>= unwrapHostPokeValue
    unwrapHostPokeValue value = pure value

    hostTypedSize ty = case tyAnnotationHead ty of
        "Word8" -> 1; "Int8" -> 1; "CChar" -> 1; "CUChar" -> 1
        "Word16" -> 2; "Int16" -> 2; "CShort" -> 2; "CUShort" -> 2
        "Word32" -> 4; "Int32" -> 4; "CInt" -> 4; "CUInt" -> 4
        _ -> FStorable.sizeOf (undefined :: Word)

    writeHostTypedPoke ty p off value = case (tyAnnotationHead ty, value) of
        ("Ptr", v) -> valToHostPtr v >>= \q ->
            FStorable.pokeByteOff (castPtr p) off (castPtr q :: Ptr Word8)
        ("CInt", VInt n) -> pokeI32 n
        ("Int32", VInt n) -> pokeI32 n
        ("CUInt", VInt n) -> pokeW32 n
        ("Word32", VInt n) -> pokeW32 n
        ("CShort", VInt n) -> pokeI16 n
        ("Int16", VInt n) -> pokeI16 n
        ("CUShort", VInt n) -> pokeW16 n
        ("Word16", VInt n) -> pokeW16 n
        ("CChar", VInt n) -> pokeI8 n
        ("Int8", VInt n) -> pokeI8 n
        ("CUChar", VInt n) -> pokeW8 n
        ("Word8", VInt n) -> pokeW8 n
        ("Int", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Int)
        ("Word", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Word)
        ("CSize", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Word)
        ("Int64", VInt n) -> FStorable.pokeByteOff p off (n :: Int64)
        ("Word64", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Word64)
        _ -> error ("typed poke: unsupported " <> BC.unpack ty <> " value "
            <> showValForDebug value)
      where
        pokeI8 n = FStorable.pokeByteOff p off (fromIntegral n :: Int8)
        pokeW8 n = FStorable.pokeByteOff p off (fromIntegral n :: Word8)
        pokeI16 n = FStorable.pokeByteOff p off (fromIntegral n :: Int16)
        pokeW16 n = FStorable.pokeByteOff p off (fromIntegral n :: Word16)
        pokeI32 n = FStorable.pokeByteOff p off (fromIntegral n :: Int32)
        pokeW32 n = FStorable.pokeByteOff p off (fromIntegral n :: Word32)

    typedNullaryTags = goTags Map.empty
      where
        goTags acc (ETyApp (EVar n) ty)
            | bareName n `elem` map BC.pack ["minBound", "maxBound", "mempty"] =
                Map.insertWith (\_ old -> old) (bareName n) ty acc
        goTags acc (EApp f x) = goTags (goTags acc f) x
        goTags acc (ETyApp inner _) = goTags acc inner
        goTags acc (ELocalSig _ inner) = goTags acc inner
        goTags acc (ETuple xs) = foldl goTags acc xs
        goTags acc (ELet bs body) = foldl goTags (goTags acc body) (map snd bs)
        goTags acc (ELam _ body) = goTags acc body
        goTags acc (ECase scrut alts) = foldl goTags (goTags acc scrut)
            [rhs | Alt _ rhs <- alts]
        goTags acc (EIf c t e) = goTags (goTags (goTags acc c) t) e
        goTags acc _ = acc

    stampMatchingNullaries tags = stamp
      where
        stamp expr@(EVar n) = case Map.lookup (bareName n) tags of
            Just ty -> ETyApp expr ty
            Nothing
                | bareName n `elem` map BC.pack ["minBound", "maxBound"]
                , Just ty <- boundedTag ->
                    ETyApp expr ty
                | otherwise -> expr
        stamp expr@ETyApp{} = expr
        stamp (EApp f x) = EApp (stamp f) (stamp x)
        stamp (ELocalSig ty inner) = ELocalSig ty (stamp inner)
        stamp (ETuple xs) = ETuple (map stamp xs)
        stamp (ELet bs body) = ELet [(n, stamp rhs) | (n, rhs) <- bs] (stamp body)
        stamp (ELam n body) = ELam n (stamp body)
        stamp (ECase scrut alts) = ECase (stamp scrut)
            [Alt pat (stamp rhs) | Alt pat rhs <- alts]
        stamp (EIf c t e) = EIf (stamp c) (stamp t) (stamp e)
        stamp other = other
        boundedTag = case Map.lookup (BC.pack "minBound") tags of
            Just ty -> Just ty
            Nothing -> Map.lookup (BC.pack "maxBound") tags

    go (ELit (LInt n))   = pure (VInt n)
    go (ELit (LInteger n)) = pure (VInteger n)
    go (ELit (LFloat d)) = pure (VFloat d)
    -- Source-level Haskell strings are [Char]. Keeping literals as real cons
    -- lists lets source-loaded libraries like bytestring pattern-match and
    -- recurse over them normally instead of tripping over the transitional
    -- VStr representation.
    go (ELit (LStr s))   = stringLiteralToListVal s
    -- @\"...\"#@ Addr# literal: allocate a leaked malloc-backed
    -- NUL-terminated buffer of the bytes and return it as
    -- 'VPrimObj (PrimPtr p)'.
    -- The literal lives for the program's lifetime — typical use
    -- is in a top-level @bytes = unsafePackLenLiteral N "..."#@
    -- whose result thunk caches the BS — so the leak is bounded
    -- by the number of distinct literals, not by call count.
    go (ELit (LAddrStr s)) = do
        let len = BS.length s
        ptr <- mallocBytes (len + 1)
        BS.useAsCStringLen s $ \(srcPtr, _) ->
            copyBytes (castPtr ptr) (castPtr srcPtr) len
        FStorable.pokeByteOff (castPtr ptr :: Ptr Word8) len (0 :: Word8)
        pure (VPrimObj (PrimPtr (castPtr ptr)))
    go (ELit (LChar c))  = pure (VChar c)
    go (ELabel name)     = pure (VLabel name)  -- Phase 3.5: OverloadedLabels
    go EGuardFail        = throwIO (PatternMatchFail "guard failed")

    go (EVar name) = do
        v <- case lookupEnv name env of
            -- Thread the current implicit-param map into the force so a
            -- nullary @f = ?x@ / @parser = do { let x = ?settings; … }@
            -- CAF reads the *call-site* bindings (GHC dictionary passing).
            Just t -> forceWithIPM hooks ipm t
            Nothing
                | Just ctor <- unboxedTupleCtorValue name -> pure ctor
                -- Quote-at-expected-Q rewrites `[| e |]` to `$qWrap (pure quote)`
                -- outside any Q-do (`runQ [| e |]`, `$( [| e |] )`).  Q-do already
                -- binds this name.  The function only builds `VCon "Q"` — never
                -- look up the name `Q` (Queue collision, `qq_th_q_not_queue`).
                | name == qWrapKey ->
                    pure (qWrapFun hooks)
                | otherwise -> do
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
                    Just t  -> forceWithIPM hooks ipm t
                    Nothing -> error ("IHC.Eval: unbound variable `"
                                      <> BC.unpack name <> "`")
        pure (wrapPopEmptyCallStack hooks ipm name v)

    go (EApp (EApp fn earlier) later)
        | let tags = typedNullaryTags later
        , not (Map.null tags)
        , let earlier' = stampMatchingNullaries tags earlier
        , earlier' /= earlier =
            go (EApp (EApp fn earlier') later)

    -- A signature on the lazy witness argument determines the Storable
    -- dictionary.  Move that annotation onto the class method before normal
    -- application, so dispatch selects the source-loaded instance without
    -- forcing @undefined :: a@.
    go (EApp f x)
         | Just ty <- lazyStorableWitnessTy f x =
             go (EApp (ETyApp f ty) x)

    -- Signature-directed specialisation of bare nullary Bounded methods.
    -- GHC specialises @measureOff maxBound@ to @maxBound :: Int@ from
    -- @measureOff :: Int -> Text -> Int@.  Without that, @Data.Text.length
    -- = negate . measureOff maxBound@ leaves @maxBound@ as an untagged
    -- 'VClassMethod' and FFI / @I#@ patterns see @\<function\>@.  When the
    -- callee has a known monomorphic first-arg type constructor, rewrite
    -- @f maxBound@ → @f (maxBound :: T)@ so 'tryTypedNullaryClassMethod' can
    -- resolve it.  Does NOT default unbound uses to Int (that poisoned
    -- @listArray (minBound, maxBound)@ for non-Int enums like StdMethod).
    go (EApp f x) = do
        -- @n :: CInt <- getSockOpt@ sets lastExpectedResultTag so the
        -- inner unannotated @peek ptr@ (Ptr a -> IO a) can reuse typed
        -- peek instead of dispatching on the Ptr tag.
        mExpectedPeek <- tryExpectedTypedPeek (EApp f x)
        case mExpectedPeek of
            Just peekVal -> pure peekVal
            Nothing -> do
                mRuntimeTyped <- runtimeDirectedTypedCall (EApp f x)
                case mRuntimeTyped of
                  Just runtimeVal -> pure runtimeVal
                  Nothing -> do
                    mElaboratedCall <- elaborateClassMethodCallee (EApp f x)
                    let ordinaryApplication = do
                            fv <- go f
                            x0 <- annotateNullaryBoundedArg f x
                            owner <- currentOwner hooks env
                            x' <- elaborateExpectedArg hooks owner f x0
                            r <- goApp fv x'
                            -- Source catch/handle: Exception e => … (e -> IO a) …
                            -- Stamp that e onto the resulting IO/function so
                            -- source fromException (cast) can select
                            -- instance Exception e.  Structural: any
                            -- constrained scheme whose next arg is
                            -- `e -> _` for a class param e.
                            mExn <- recoverExceptionHandlerTag hooks env f x
                            pure $ case mExn of
                                Just tag -> stampExpectedResultTag tag r
                                Nothing  -> r
                    case mElaboratedCall of
                      CalleeElaborated call' -> go call'
                      CalleeAttemptFailed | isApplication f ->
                          go (suppressCalleeElaboration (EApp f x))
                      CalleeAttemptFailed -> ordinaryApplication
                      CalleeNotAttempted -> ordinaryApplication
      where
        runtimeDirectedTypedCall call = case flattenApps call of
            (headExpr, args) -> do
              mMethod <- typedOrBareMethod headExpr
              case mMethod of
               Just (cls, method) -> do
                numeric <- tryNumericComparison method args
                case numeric of
                  Just result -> pure (Just result)
                  Nothing -> dispatchTypedMethod cls method args
               Nothing -> pure Nothing

        tryNumericComparison method args
            | lastDottedMethod method `elem` map BC.pack ["==", "/=", "<", "<=", ">", ">="]
            , [leftE, rightE] <- args = do
                left <- eval hooks env ipm leftE >>= unwrapIntHash hooks
                right <- eval hooks env ipm rightE >>= unwrapIntHash hooks
                pure $ case (left, right) of
                    (VInt x, VInt y) -> Just (intClassOp method x y)
                    _ -> Nothing
            | otherwise = pure Nothing

        dispatchTypedMethod cls method args = do
                owner <- currentOwner hooks env
                ownerScheme <- lookupTypeSigFallback hooks owner method
                neutralScheme <- lookupTypeSigFallback hooks Nothing method
                let methodKey = lastDottedMethod method
                let directIndex = firstJust
                        (map (>>= directClassArgIndex)
                            [ownerScheme, neutralScheme])
                case directIndex of
                    Just idx | idx < length args -> do
                        targetV <- eval hooks env ipm (args !! idx)
                        let runtimeTag = normalizeTyTag (typeTagOf targetV)
                        do
                          hostPoke <- tryHostStorablePoke
                                cls methodKey runtimeTag targetV args
                          case hostPoke of
                            Just action -> pure (Just action)
                            Nothing -> do
                              mReg <- getSharedClassReg legacyHooks
                              case mReg of
                                Just reg -> do
                                    direct <- lookupInstanceMethod reg cls runtimeTag methodKey
                                    patterned <- case usableMethod direct of
                                        Just _ -> pure Nothing
                                        Nothing -> lookupInstanceMethodPattern reg cls runtimeTag methodKey
                                    delegated <- storableNewtypeMethod
                                        reg cls methodKey runtimeTag targetV
                                    case delegated <|>
                                         (\m -> (m, targetV)) <$>
                                             (usableMethod direct <|> usableMethod patterned) of
                                        Nothing -> pure Nothing
                                        Just (runtimeMethod, dispatchTarget) -> do
                                            thunks <- sequence
                                                [ if i == idx then newWHNFThunk dispatchTarget
                                                  else newThunkIP env ipm arg
                                                | (i, arg) <- zip [0..] args
                                                ]
                                            forcedMethod <- forceMethodVal hooks runtimeMethod
                                            Just <$> foldM (applyIP hooks ipm) forcedMethod thunks
                                _ -> pure Nothing
                    _ -> pure Nothing

        -- GND Storable instances (CInt, CUInt, …) have no source method
        -- bodies of their own: they coerce the representation instance.
        -- Delegating poke-family calls to that field instance avoids falling
        -- into Storable's mutually recursive default poke/pokeByteOff pair.
        storableNewtypeMethod reg cls method runtimeTag targetV
            | cls == BC.pack "Storable"
            , method `elem` map BC.pack ["poke", "pokeByteOff", "pokeElemOff"]
            , VCon _ [fieldThunk] <- targetV = do
                mFieldTy <- lookupDeclaredFieldTag runtimeTag
                case mFieldTy of
                    Nothing -> pure Nothing
                    Just fieldTy -> do
                        let fieldTag = normalizeTyTag fieldTy
                        direct <- lookupInstanceMethod reg cls fieldTag method
                        patterned <- case usableMethod direct of
                            Just _ -> pure Nothing
                            Nothing -> lookupInstanceMethodPattern reg cls fieldTag method
                        case usableMethod direct <|> usableMethod patterned of
                            Nothing -> pure Nothing
                            Just runtimeMethod -> do
                                fieldV <- force hooks fieldThunk
                                pure (Just (runtimeMethod, fieldV))
            | otherwise = pure Nothing

        tryHostStorablePoke cls method runtimeTag targetV args
            | cls == BC.pack "Storable"
            , method `elem` map BC.pack ["poke", "pokeByteOff", "pokeElemOff"]
            , ptrE : rest <- args = do
                primitive <- unwrapPokeValue targetV
                ptrV <- go ptrE
                p <- valToHostPtr ptrV
                markedWord8 <- isMarkedWord8Ptr p
                mPointee <- lookupTypedHostPtr p
                let ptrIsWord8 = markedWord8 || mPointee `elem`
                        map Just [BC.pack "Word8", BC.pack "CChar",
                                  BC.pack "CSChar", BC.pack "CUChar"]
                let effectiveTag
                        | runtimeTag == BC.pack "Int" && ptrIsWord8 = BC.pack "Word8"
                        | otherwise = runtimeTag
                if knownPeekResultTy effectiveTag
                    then pure (Just (VIO (do
                        off <- case (method, rest) of
                            (m, offE : _) | m /= BC.pack "poke" -> do
                                offV <- go offE
                                let n = case offV of
                                        VInt x -> fromIntegral x
                                        VInteger x -> fromInteger x
                                        other -> error ("typed poke: offset is not an Int: "
                                            <> showValForDebug other)
                                pure $ if m == BC.pack "pokeElemOff"
                                    then n * typedPeekElemSize effectiveTag else n
                            _ -> pure 0
                        writeTypedPoke effectiveTag p off primitive
                        pure VUnit)))
                    else pure Nothing
            | otherwise = pure Nothing

        unwrapPokeValue (VCon _ [field]) = force hooks field >>= unwrapPokeValue
        unwrapPokeValue value = pure value

        writeTypedPoke ty p off value = case (tyAnnotationHead ty, value) of
            ("CInt", VInt n)  -> pokeI32 n
            ("Int32", VInt n) -> pokeI32 n
            ("CUInt", VInt n) -> pokeW32 n
            ("Word32", VInt n) -> pokeW32 n
            ("CShort", VInt n) -> pokeI16 n
            ("Int16", VInt n) -> pokeI16 n
            ("CUShort", VInt n) -> pokeW16 n
            ("Word16", VInt n) -> pokeW16 n
            ("CChar", VInt n) -> pokeI8 n
            ("Int8", VInt n) -> pokeI8 n
            ("CUChar", VInt n) -> pokeW8 n
            ("Word8", VInt n) -> pokeW8 n
            ("Int", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Int)
            ("Word", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Word)
            ("Int64", VInt n) -> FStorable.pokeByteOff p off (n :: Int64)
            ("Word64", VInt n) -> FStorable.pokeByteOff p off (fromIntegral n :: Word64)
            _ -> error ("typed poke: unsupported " <> BC.unpack ty <> " value "
                <> showValForDebug value)
          where
            pokeI8 n = FStorable.pokeByteOff p off (fromIntegral n :: Int8)
            pokeW8 n = FStorable.pokeByteOff p off (fromIntegral n :: Word8)
            pokeI16 n = FStorable.pokeByteOff p off (fromIntegral n :: Int16)
            pokeW16 n = FStorable.pokeByteOff p off (fromIntegral n :: Word16)
            pokeI32 n = FStorable.pokeByteOff p off (fromIntegral n :: Int32)
            pokeW32 n = FStorable.pokeByteOff p off (fromIntegral n :: Word32)

        typedOrBareMethod (ETypedMethod cls method _) = pure (Just (cls, method))
        -- `pred` is also a conventional predicate parameter name throughout
        -- base (notably Foreign.C.Error).  Bare occurrences must retain
        -- lexical shadowing; genuine Enum.pred calls are elaborated to
        -- ETypedMethod and take the branch above.
        typedOrBareMethod (EVar method)
            | lastDottedMethod method == BC.pack "pred" = pure Nothing
            | otherwise = lookupIndexed
          where
            lookupIndexed = do
                classes <- readIORef globalMethodClassRef
                let bare = lastDottedMethod method
                pure $ case Map.lookup bare classes of
                    Just [cls] -> Just (cls, method)
                    _ -> Nothing
        typedOrBareMethod _ = pure Nothing

        flattenApps = collect []
          where
            collect args (EApp fn arg) = collect (arg : args) fn
            collect args headExpr = (headExpr, args)

        directClassArgIndex (Scheme _ preds body) = do
            classVar <- case preds of
                [TA.Pred _ [TA.TyVar v]] -> Just v
                _ -> Nothing
            let (args, _) = tyArrowArgs body
            findExact 0 classVar args
          where
            findExact _ _ [] = Nothing
            findExact i want (TA.TyVar actual : rest)
                | want == actual = Just i
                | otherwise = findExact (i + 1) want rest
            findExact i want (_ : rest) = findExact (i + 1) want rest

        firstJust [] = Nothing
        firstJust (Just x : _) = Just x
        firstJust (Nothing : xs) = firstJust xs

        usableMethod (Just (VCon n []))
            | BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n = Nothing
        usableMethod other = other

        isApplication EApp{} = True
        isApplication _ = False
        goApp fv x'' = do
          -- `(<|>) left (pure Nothing)` thunks the right arg after
          -- Alternative dispatch has already popped the do-carrier.
          -- Consume the last monadic instance tag only for a result-poly
          -- `pure`/`return`/`empty` argument so an inner apply of
          -- `(<|>) left` does not steal it.
          --
          -- An outer ascription (`p :: Parser Char; p = pure 'a' <|>
          -- pure 'b'`) scopes the carrier via expected-result-tag.
          -- Do not steal lastMonadicCarrier when that (or the
          -- do-carrier) is already usable: the first `pure` operand
          -- would consume the tag and leave the second as IO.
          mDo <- currentDoCarrier
          mExpected <- readExpectedResultTag
          let usable c = not (BC.null c) && not (isQCarrier c)
                         && c /= BC.pack "IO"
          mLast <- if isPureLikeArg x''
                       && maybe True (not . usable) mDo
                       && maybe True (not . usable) mExpected
                       then takeLastMonadicCarrier
                       else pure Nothing
          -- IO is already the result-poly default; pinning Q's
          -- `pure SourcePos` to IO hung hsx_hello.  Only a non-IO
          -- carrier (ParsecT, …) needs the annotation.
          let xAnn = case (mDo, mLast, mExpected) of
                  (Just c, _, _) | usable c -> annotatePureLike c x''
                  (_, Just c, _) | usable c -> annotatePureLike c x''
                  (_, _, Just c)
                      | let c' = normalizeTyTag c
                      , usable c' && isPureLikeArg x'' ->
                          annotatePureLike c' x''
                  _ -> x''
          xt  <- newThunkIP env ipm xAnn          -- argument stays a thunk (lazy)
          case fv of
            VPrimObj _ -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): VPrimObj in function position: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a
                       <> " while evaluating `"
                       <> show f <> "` applied to `" <> show x <> "`")
            -- leftover (# s, a #) is applied as a function; rematch
            -- in applyIP.  Other multi-field constructors still error.
            VCon n (_:_:_) | not (isUnboxedStateTupleName n) -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): not a function while evaluating `"
                       <> show f <> "` applied to `" <> show x <> "`: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a)
            VCon n [] | not (isStateTokenNewtypeCtor n) -> do
                a <- force hooks xt
                error ("IHC.Eval.go(EApp): not a function while evaluating `"
                       <> show f <> "` applied to `" <> show x <> "`: "
                       <> showValForDebug fv <> " applied to " <> showValForDebug a)
            VClassMethod name _ tags goCM ->
                applyClassMethodFast hooks name tags goCM xt
            _ -> applyIP hooks ipm fv xt

    go (ELam name body) =
        -- Phase 3.6: User-defined lambdas use VFunIP so the caller can
        -- pass its ImplicitParamMap at call time. The closed-over `ipm`
        -- (lexical binding) takes priority over the caller's map.
        -- Do not snapshot the do-carrier onto every lambda: that leaked
        -- ParsecT into runParser's Identity (`runIdentity` on a ParsecT).
        pure $ VFunIP ipm $ \callerIPM argThunk -> do
            let mergedIPM = Map.union ipm callerIPM
                env' = extendEnv name argThunk env
            eval hooks env' mergedIPM body

    go (ELet binds body) = do
        -- Recursive group: pre-allocate a thunk per binding holding a
        -- 'BlackHole' placeholder, build the env from those (now-live)
        -- IORef pointers, then back-patch each slot with its real
        -- closure that can now see the env. This is the classic
        -- tying-the-knot pattern with mutable refs — avoids the
        -- strict-cycle hazard that 'mfix' / 'rec' would hit on the
        -- 'IO' monad given that 'Closure' has a strict env field.
        slots <- mapM (\_ -> newIORef (BlackHole Nothing "<let-placeholder>" Nothing)) binds
        let names  = map fst binds
            env'   = extendEnvMany (zip names slots) env
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure env' ipm rhs)))
              (zip binds slots)
        eval hooks env' ipm body

    go (ECase scrut alts) = do
        scrutT <- newThunkIP env ipm scrut
        tryAltsFromThunk scrutT alts

    go (EIf c t e) = do
        cv0 <- go c
        -- Leftover IO / State# VFun as if-condition.
        -- Network.Socket.Info.followAddrInfo does `if ptr_ai == nullPtr`.
        -- After Settings last-write, == leftover-returns host VIO
        -- (eqAddr# primop wrapper).  Same peel as forceCaseScrut: run
        -- leftover VIO / State# VFun, then re-check Bool.  EIf is not
        -- an IO-carrier match (`catch (IO io)`), so running is correct.
        cv <- peelIfCondition hooks cv0
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

    go (EDo stmts) = evalConstructedDo hooks env ipm Nothing stmts

    -- Phase 3.6: Implicit parameter reference.
    -- Look up ?name in the current ImplicitParamMap. Miss -> runtime error.
    go (EImplicitRef name) = case lookupIPMap name ipm of
        Just t  -> force hooks t
        Nothing -> defaultUnboundImplicit hooks env name

    -- Phase 3.6: Implicit parameter let-binding.
    -- Extend the implicit-param map for the duration of @body@.
    -- Each binding thunk captures the CURRENT env+ipm (not the extended ipm').
    go (EImplicitLet binds body) = do
        slots <- mapM (\_ -> newIORef (BlackHole Nothing "<implicit-let-placeholder>" Nothing)) binds
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

    -- Record update: patch the runtime VCon's named slot.  Do not
    -- case-desugar via the unioned FieldRegistry — bare constructor
    -- names are not unique and homonyms inflate the pattern arity.
    go (ERecordUpdate baseExpr updates) = do
        base <- go baseExpr
        applyRecordUpdate hooks env ipm go base updates

    -- Phase 2.11: TH splices should be expanded before eval by the
    -- scheduler's expandSplicesInModule pass. If one reaches here it's
    -- a bug — report it clearly rather than looping.
    go (ESplice _) =
        error "IHC.Eval: ESplice reached eval — splice expansion pass missed this node"

    -- Phase 2.12: TemplateHaskellQuotes bracket [| expr |].
    -- Produce a TH Exp-shaped Val encoding of the *syntax* of expr.
    -- We do NOT evaluate expr — we encode its AST.
    go (EQuote inner) = evalQuote hooks env ipm inner

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
        case hostTypedPokeMethod cls method tag of
            Just host -> pure host
            Nothing -> do
                mReg <- getSharedClassReg legacyHooks
                case mReg of
                    Nothing  -> error ("IHC.Eval.ETypedMethod: no shared class registry installed "
                                      <> "(elaborator fired before buildBaseEnv?)")
                    Just reg -> resolveTypedMethod hooks reg cls method tag

    -- A constrained alias carries the full, elaborated instance key. For the
    -- currently supported single-dictionary form, attach every parameter tag
    -- to the dispatcher produced by the source value. Multi-parameter lookup
    -- then follows the ordinary VClassMethod path.
    go (EConstrainedValue inner [(_cls, instanceTags)]) = do
        v <- go inner
        case v of
            VClassMethod m slot _ fn ->
                -- This node carries the complete predicate key, unlike an
                -- ETyApp (which contributes one positional tag).  Replace
                -- any tags inherited through a constrained alias instead of
                -- duplicating them when aliases delegate to other aliases.
                pure (VClassMethod m slot (map normalizeTyTag instanceTags) fn)
            _ -> pure v
    go (EConstrainedValue inner _) = go inner

    -- A local declaration signature guides elaboration but is erased before
    -- runtime application. In particular, never route its complete scheme
    -- through 'goTyApp': that path is for visible @f @T@ applications and
    -- appends dispatch tags to VClassMethod values.
    go (ELocalSig scheme e) = do
        mElab <- tryElaborateLocalSig scheme e
        case mElab of
            Just e' -> go e'
            Nothing -> go e

    -- A concrete pointer ascription only carries host-memory metadata.  It
    -- does not need the general type-application elaboration path.
    go (ETyApp e ty)
        | Just elemTy <- ptrPointeeHead ty = do
            v <- go e
            markTypedPtrVal elemTy v
            pure v

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
        -- A do-block annotated with its result carrier (IO / ST /
        -- ParsecT / Q / BuildStep's IO result, …) must keep that
        -- carrier at eval time.  goTyApp would evaluate the EDo first
        -- and only then attach the type to the *value*, which is too
        -- late: evalDo has already defaulted a function-shaped first
        -- action to ParsecT.
        -- PatternSignatures @n :: CInt <- action@ keep CInt as ETyApp
        -- on the action (not on the inner peek).  Scope that result
        -- type so result-polymorphic peek/peekElemOff can see it.
        -- A bare carrier stamp (`ETyApp (EDo …) "IO"`) is not a value
        -- ascription.  Scoping expected-result-tag as IO poisoned
        -- C8.pack via createFpUptoN' seeing expected type IO.
        let withResultTag act
                | isBareMonadicCarrier ty = act
                | Just headTy <- expectedResultHead ty =
                    withExpectedResultTag headTy act
                | otherwise = act
            isBareMonadicCarrier t =
                t == BC.pack "IO" || t == BC.pack "ST"
             || t == BC.pack "STM" || t == BC.pack "Q"
            -- Publish the peeled result head (BuildStep a → IO) as
            -- lastMonadicCarrier while this ascription runs.  `>>=` of
            -- a leftover State# VFun then stays IO; without it,
            -- toLazyByteString matched Finished/Yield1 against
            -- <function>.
            withPeeledCarrier act = do
                carrier <- monadicCarrierFromType ty
                if shouldPublishPeeledCarrier carrier
                    then withLastMonadicCarrier carrier act
                    else act
        case stripToDo e of
            Just stmts ->
                withResultTag $ withPeeledCarrier $ do
                    carrier <- monadicCarrierFromType ty
                    evalConstructedDo hooks env ipm (Just carrier) stmts
            Nothing -> withResultTag (withPeeledCarrier (goTyAppBody e ty))

    goTyAppBody e ty = do
        mTypedField <- tryTypedField e ty
        case mTypedField of
            Just v -> pure v
            Nothing -> do
                mTypedPeek <- tryTypedPeek e ty
                case mTypedPeek of
                    Just v  -> pure v
                    Nothing -> do
                        mTypedNullary <- tryTypedNullaryClassMethod e ty
                        case mTypedNullary of
                          Just v -> pure v
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
                                Just e' -> case stripToDo e' of
                                    Just stmts' -> do
                                        carrier <- monadicCarrierFromType ty
                                        evalConstructedDo hooks env ipm
                                            (Just carrier)
                                            stmts'
                                    Nothing -> goTyApp e' ty
                                Nothing -> goTyApp e ty

    tryTypedField (EVar name) ty = case lookupEnv name env of
        Just thunk -> specializeTypedField thunk ty
        Nothing -> pure Nothing
    tryTypedField (EApp selector recordExpr) ty = do
        selectorVal <- go selector
        case selectorVal of
            VFieldAccessor _ clauses evidence _ -> do
                recordVal <- go recordExpr
                case recordVal of
                    VCon ctor fields
                        | Just idx <- lookup ctor clauses
                        , idx < length fields -> do
                            owner <- currentOwner hooks env
                            case agreedFieldEvidence ctor evidence of
                                Just (scheme, fieldOwner)
                                    | schemeNeedsElaboration scheme -> do
                                        view <- newIORef (TypedField (fields !! idx)
                                            scheme fieldOwner)
                                        specializeTypedField view ty
                                _ -> do
                                    registry <- readIORef globalConstructorTypeRegistryRef
                                    case constructorMetadata registry owner ctor of
                                        Just metadata
                                            | idx < length (ctmFieldTypes metadata)
                                            , let fieldTy = ctmFieldTypes metadata !! idx
                                            , containsForall fieldTy -> do
                                                view <- newIORef (TypedField (fields !! idx)
                                                    (fieldScheme fieldTy)
                                                    (ciOwner (ctmIdentity metadata)))
                                                specializeTypedField view ty
                                        _ -> pure Nothing
                    _ -> pure Nothing
            _ -> pure Nothing
    tryTypedField _ _ = pure Nothing

    specializeTypedField thunk ty = case Elab.parseRawTypeExpr ty of
        Nothing -> pure Nothing
        Just annTy -> do
            state <- readIORef thunk
            case state of
                TypedField canonical _ fieldOwner -> do
                    canonicalState <- readIORef canonical
                    case canonicalState of
                        Unevaluated (Closure closureEnv closureIpm closureExpr) -> do
                            mReg <- getSharedClassReg legacyHooks
                            case mReg of
                                Nothing -> pure Nothing
                                Just classReg -> do
                                    sigs0 <- readIORef globalTypeSigsRef
                                    syns <- readIORef globalTypeSynonymsRef
                                    ctorTypes <- readIORef globalConstructorTypeRegistryRef
                                    mHeadScheme <- case applicationHeadName closureExpr of
                                        Just method -> lookupTypeSigFallback hooks
                                            (Just fieldOwner) method
                                        Nothing -> pure Nothing
                                    let sigs = case (applicationHeadName closureExpr,
                                            mHeadScheme) of
                                          (Just method, Just scheme) ->
                                            Map.insert (bareName method) scheme
                                                (Map.insert method scheme sigs0)
                                          _ -> sigs0
                                    result <- try (Elab.elaborateOwned classReg sigs syns
                                        ctorTypes (Just fieldOwner) (Elab.ExpectType annTy)
                                        closureExpr)
                                        :: IO (Either SomeException (Expr, TA.Type))
                                    case result of
                                        Right (specialized, _) | specialized /= closureExpr ->
                                            Just <$> eval hooks
                                                (HashMap.union closureEnv env)
                                                closureIpm specialized
                                        _ -> pure Nothing
                        _ -> pure Nothing
                _ -> pure Nothing

    fieldScheme (TyForall vars preds body) = Scheme vars preds body
    fieldScheme ty = Scheme [] [] ty
    containsForall TyForall{} = True
    containsForall (TyArrow a b) = containsForall a || containsForall b
    containsForall (TyApp f x) = containsForall f || containsForall x
    containsForall _ = False

    -- Resolve a visible type application on a class method using the
    -- method→class map + scanned signature.  No method-name list:
    -- @empty \@Parser@ / @empty' \@Maybe@ / @maxBound \@Int@ share
    -- this path.  Methods that still need a value argument
    -- (@pure \@Maybe@) resolve to the instance function, which the
    -- surrounding 'EApp' then applies.
    tryTypedNullaryClassMethod e ty =
        case classMethodHead e of
            Nothing -> pure Nothing
            Just method -> do
                clsMap <- readIORef globalMethodClassRef
                methodNames <- readIORef globalClassMethodNamesRef
                sigs <- readIORef globalTypeSigsRef
                let bareMethod = lastNameComponent method
                    tag = normalizeTyTag ty
                    fromMap = Map.findWithDefault [] bareMethod clsMap
                           ++ Map.findWithDefault [] method clsMap
                    fromSig = case Map.lookup bareMethod sigs of
                        Just (TA.Scheme _ preds _) ->
                            [ c | TA.Pred c _ <- preds ]
                        Nothing -> case Map.lookup method sigs of
                            Just (TA.Scheme _ preds _) ->
                                [ c | TA.Pred c _ <- preds ]
                            Nothing -> []
                    classes = nubKeep fromMap fromSig
                    isMethod =
                        Set.member bareMethod methodNames
                        || Set.member method methodNames
                        || not (null classes)
                if not isMethod || null classes || BC.null tag
                    then pure Nothing
                    else do
                        mReg <- getSharedClassReg legacyHooks
                        case mReg of
                            Nothing -> pure Nothing
                            Just classReg ->
                                tryClasses classReg bareMethod tag classes
      where
        classMethodHead (EVar n)      = Just n
        classMethodHead (ETyApp i _)  = classMethodHead i
        classMethodHead _             = Nothing

        lastNameComponent n =
            case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
                Just idx -> BC.drop (idx + 1) n
                Nothing  -> n

        nubKeep xs ys = go [] (xs ++ ys)
          where
            go acc [] = reverse acc
            go acc (c:cs)
                | c `elem` acc = go acc cs
                | otherwise    = go (c : acc) cs

        tryClasses _ _ _ [] = pure Nothing
        tryClasses classReg method tag (cls:rest) = do
            mv0 <- lookupInstanceMethod classReg cls tag method
            mv <- case mv0 of
                Just _  -> pure mv0
                Nothing -> do
                    triggerCoreInstanceLoad legacyHooks cls
                    lookupInstanceMethod classReg cls tag method
            case mv of
                Nothing -> tryClasses classReg method tag rest
                Just v -> do
                    r <- try (forceMethodVal hooks v)
                            :: IO (Either SomeException Val)
                    case r of
                        Right v'
                            | isPlaceholderMethod v' ->
                                tryClasses classReg method tag rest
                            | otherwise -> pure (Just v')
                        Left _ -> tryClasses classReg method tag rest

        isPlaceholderMethod (VCon n []) =
            BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n
        isPlaceholderMethod _ = False

    -- | Helper: try to elaborate @e@ under the annotation @ty@.
    -- Returns 'Just' if elaboration rewrote something; 'Nothing'
    -- otherwise.
    tryElaborateTyAnn e ty
        -- @>>@ / @>>=@ / @*>@ annotated with a transformer head
        -- (@ParsecT@) must not go through ExpectType: inference can
        -- treat @ParsecT e s Identity@ as @Identity (ParsecT …)@ and
        -- rewrite the bind to Identity.  That is the
        -- @do { space; pure () }@ leftover — @runIdentity@ on a
        -- @ParsecT@.  Skip elaboration; goTyApp only appends the tag.
        -- Do not treat ParsecT as Q/Exp.
        | Just n <- applicationHeadName e
        , isSequencingBindName (bareName n) = pure Nothing
        -- Same for a do-block stamped with the carrier.  ExpectType
        -- @ParsecT a@ on prefix statements rewrote @0@ / @min@ /
        -- @compare@ through @instance Num (ParsecT e s m a)@.
        -- evalDo + do-carrier already pin `pure`/`return`.
        | Just _ <- stripToDo e = pure Nothing
        | otherwise = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing -> pure Nothing
            Just classReg -> do
                let mAnnTy = Elab.parseRawTypeExpr ty
                case mAnnTy of
                  Nothing -> pure Nothing
                  Just annTy -> do
                    sigs0 <- readIORef globalTypeSigsRef
                    syns <- readIORef globalTypeSynonymsRef
                    ctorTypes <- readIORef globalConstructorTypeRegistryRef
                    owner <- currentOwner hooks env
                    mHeadScheme <- case applicationHeadName e of
                        Just method -> lookupTypeSigFallback hooks owner method
                        Nothing -> pure Nothing
                    -- Keep a result-polymorphic class scheme (fromString
                    -- :: IsString a => String -> a) when the owner
                    -- fallback is a monomorphic last-writer instance.
                    -- Overwriting with `String -> Text` made e' == e
                    -- (no ETypedMethod) and left fromString leftover.
                    let (sigs, scoped) = case applicationHeadName e of
                            Just method ->
                                let bare = bareName method
                                    globalSch = case Map.lookup method sigs0 of
                                        Just s -> Just s
                                        Nothing -> Map.lookup bare sigs0
                                    chosen = case (globalSch, mHeadScheme) of
                                        (Just old, Just new)
                                            | schemeIsResultPolymorphic old
                                            , not (schemeIsResultPolymorphic new)
                                            -> Just old
                                            | otherwise -> Just new
                                        (Just old, Nothing) -> Just old
                                        (Nothing, new) -> new
                                in case chosen of
                                    Just scheme ->
                                        ( Map.insert bare scheme
                                            (Map.insert method scheme sigs0)
                                        , Set.fromList [method, bare]
                                        )
                                    Nothing -> (sigs0, Set.empty)
                            Nothing -> (sigs0, Set.empty)
                    r <- try (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                                ctorTypes owner scoped
                                (Elab.ExpectType annTy) e)
                           :: IO (Either SomeException (Expr, TA.Type))
                    let expandedAnn = expandTypeSynonyms syns annTy
                    pin <- case pinFromStringAtExpected expandedAnn e of
                        Just p -> pure (Just p)
                        Nothing -> pinFromIntegerAtExpected expandedAnn e
                    case r of
                        Right (e', _) | e' /= e -> do
                            -- Validate the rewrite. The elaborator emits
                            -- 'ETypedMethod cls method tag' wherever a
                            -- name's signature looks like a class
                            -- method (constraint @cls v@ with @v@ in the
                            -- body, name in 'globalClassMethodNamesRef').
                            -- That heuristic mis-fires for top-level
                            -- functions that happen to share a name with
                            -- a class method elsewhere — e.g. a
                            -- top-level 'try' shadowed by a
                            -- 'MonadParsec.try'-style class-method
                            -- dispatcher whenever the megaparsec class
                            -- ends up in scope (historically: when
                            -- @Text.Megaparsec.Class@ was eagerly
                            -- loaded; today: only if the user imports it).
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
                            if ok then pure (Just e') else keepPin classReg pin
                        _ -> keepPin classReg pin
      where
        keepPin classReg pin = case pin of
            Nothing -> pure Nothing
            Just pinned -> do
                ok <- allTypedMethodsResolvable classReg pinned
                if ok then pure (Just pinned) else pure Nothing

    -- Expected type T (not [Char]) on a string lit or `fromString x`
    -- is IsString T.  Used when inference is a no-op (e' == e) because
    -- a monomorphic instance scheme hid the class method.
    pinFromStringAtExpected annTy expr
        | Elab.isCharListType annTy = Nothing
        | Just tag <- fromStringAnnTag annTy
        , not (BC.null tag) =
            case stripToExpr expr of
                e | Elab.isStringLiteralExpr e ->
                    Just (EApp (ETypedMethod istringCls fromStringName tag) e)
                EApp f x | isFromStringHead f ->
                    Just (EApp (ETypedMethod istringCls fromStringName tag) x)
                _ -> Nothing
        | otherwise = Nothing
      where
        istringCls = BC.pack "IsString"
        fromStringName = BC.pack "fromString"
        stripToExpr (ETyApp inner _) = stripToExpr inner
        stripToExpr other = other
        isFromStringHead (EVar n) = bareName n == fromStringName
        isFromStringHead (ETyApp inner _) = isFromStringHead inner
        isFromStringHead (ETypedMethod _ m _) = m == fromStringName
        isFromStringHead _ = False
        fromStringAnnTag = Elab.fromStringResultTag

    -- Expected type T (not Int/Integer) on an integer lit or
    -- `fromInteger n` is Num T.  Used when inference is a no-op
    -- (e' == e) because a monomorphic last-writer hid the class
    -- method.  Derived Eq of a multi-ctor Num type must not see
    -- a leftover VInt.  No Size / Hint name list.
    --
    -- Do not pin at a live monadic carrier (ParsecT do / expected
    -- Parser) or at an applied monad type (`m a`).  GHC only
    -- inserts fromInteger when Num t holds; wrapping `0` / `4` as
    -- Num ParsecT made T.pack's `dstOff + 4` hit int-spine Num.+
    -- (left=0 right=<ParsecT>).
    pinFromIntegerAtExpected annTy expr
        | isIntOrIntegerAnn annTy = pure Nothing
        | monadShapedAnn annTy = pure Nothing
        | Just tag <- fromStringAnnTag annTy
        , not (BC.null tag) = do
            carrierHit <- isLiveMonadicCarrier tag
            if carrierHit
                then pure Nothing
                else pure $ case stripToExpr expr of
                    ELit (LInt n) ->
                        Just (EApp (ETypedMethod numCls fromIntegerName tag)
                                   (ELit (LInteger (fromIntegral n))))
                    ELit (LInteger _) ->
                        Just (EApp (ETypedMethod numCls fromIntegerName tag) expr)
                    EApp f x | isFromIntegerHead f ->
                        Just (EApp (ETypedMethod numCls fromIntegerName tag) x)
                    _ -> Nothing
        | otherwise = pure Nothing
      where
        numCls = BC.pack "Num"
        fromIntegerName = BC.pack "fromInteger"
        stripToExpr (ETyApp inner _) = stripToExpr inner
        stripToExpr other = other
        isFromIntegerHead (EVar n) = bareName n == fromIntegerName
        isFromIntegerHead (ETyApp inner _) = isFromIntegerHead inner
        isFromIntegerHead (ETypedMethod _ m _) = m == fromIntegerName
        isFromIntegerHead _ = False
        fromStringAnnTag = Elab.fromStringResultTag
        isIntOrIntegerAnn ty = case tyHead ty of
            Just n ->
                let b = lastNameComponent n
                in b == BC.pack "Int" || b == BC.pack "Integer"
                    || b == BC.pack "I#" || b == BC.pack "Int#"
            Nothing -> False
        -- Applied type (`ParsecT e s m a`, `IO a`) is a monad shape.
        -- Nullary Size / Hint / Word8 still pin.
        monadShapedAnn ty = case TA.tyApps ty of
            (_, _ : _) -> True
            _          -> False
        isLiveMonadicCarrier tag = do
            mDo <- currentDoCarrier
            mExpected <- readExpectedResultTag
            let same c = normalizeTyTag c == normalizeTyTag tag
            pure $ maybe False same mDo || maybe False same mExpected

    applicationHeadName (EApp f _) = applicationHeadName f
    applicationHeadName (ETyApp f _) = applicationHeadName f
    applicationHeadName (EVar n) = Just n
    applicationHeadName _ = Nothing

    bareName n = case BC.elemIndexEnd '.' n of
        Just idx -> BC.drop (idx + 1) n
        Nothing -> n

    -- Unlike visible type application, a declaration annotation is a complete
    -- Scheme.  Let the elaborator parse and instantiate its forall/context as
    -- one unit, then erase only the ELocalSig wrapper at runtime.
    tryElaborateLocalSig scheme e = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing -> pure Nothing
            Just classReg -> do
                sigs0 <- readIORef globalTypeSigsRef
                syns <- readIORef globalTypeSynonymsRef
                ctorTypes <- readIORef globalConstructorTypeRegistryRef
                owner <- currentOwner hooks env
                mHeadScheme <- case applicationHeadName e of
                    Just method -> lookupTypeSigFallback hooks owner method
                    Nothing -> pure Nothing
                let (sigs, scoped) = case (applicationHeadName e, mHeadScheme) of
                        (Just method, Just headScheme) ->
                            ( Map.insert (bareName method) headScheme
                                (Map.insert method headScheme sigs0)
                            , Set.fromList [method, bareName method]
                            )
                        _ -> (sigs0, Set.empty)
                r <- try (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                            ctorTypes owner scoped Elab.InferFreely
                            (ELocalSig scheme e))
                       :: IO (Either SomeException (Expr, TA.Type))
                case r of
                    Right (ELocalSig _ e', _) | e' /= e -> do
                        ok <- allTypedMethodsResolvable classReg e'
                        if ok then pure (Just e') else pure Nothing
                    _ -> pure Nothing

    -- Elaborate a complete class-method application before evaluating its
    -- left-associated callee spine.  A method's dictionary parameter can be
    -- determined by a later argument (for example @m :: a -> f a -> a@), but
    -- evaluating @(m x)@ first enters the value-directed 'VClassMethod'
    -- dispatcher before that later argument is visible.  Owner-scoped
    -- signature metadata lets inference see the whole application and replace
    -- the bare head with 'ETypedMethod' first.
    --
    -- This is deliberately fail-closed: no signature, an ambiguous import,
    -- failed inference, an unchanged/non-method head, or a missing instance
    -- all retain the ordinary evaluator path.
    elaborateClassMethodCallee call = case appHead call of
        Nothing -> pure CalleeNotAttempted
        Just method -> do
            methodNames <- readIORef globalClassMethodNamesRef
            sigs <- readIORef globalTypeSigsRef
            -- A constrained top-level alias (for example @convert = method@)
            -- needs the same whole-spine elaboration as the class method it
            -- delegates to.  Its own name is deliberately absent from the
            -- class-method catalogue.  The cheap global metadata check keeps
            -- ordinary application evaluation off the fallback path.
            let bare = bareName method
                exactScheme = Map.lookup method sigs
                -- Bare last-writer is not a scheme for a qualified
                -- spelling.  @Data.Set.fromList@ / @Set.fromList@ share
                -- a class-method name with @IsList.fromList@;
                -- InferFreely of that last-writer at @Set Text@ walks
                -- @Item@ and hangs before main.
                quickScheme = case exactScheme of
                    Just scheme -> Just scheme
                    Nothing | isQualifiedName method -> Nothing
                    Nothing -> Map.lookup bare sigs
                constrained = case quickScheme of
                    Just (Scheme _ [TA.Pred _ (_ : _)] _) -> True
                    Nothing -> False
                    _ -> False
                candidate
                    | isQualifiedName method
                    , Set.member bare methodNames
                    , maybe True (const False) exactScheme
                    = False
                    | otherwise = Set.member bare methodNames || constrained

                isQualifiedName n = case BC.elemIndexEnd '.' n of
                    Just i -> i > 0 && i + 1 < BC.length n
                    Nothing -> False
            owner <- if candidate then currentOwner hooks env else pure Nothing
            mScheme <- if candidate then lookupTypeSigFallback hooks owner method
                                    else pure Nothing
            case mScheme of
                Just scheme@(Scheme _ [TA.Pred _ (_ : _)] _)
                  | schemeIsResultPolymorphic scheme -> do
                    clsMap <- readIORef globalMethodClassRef
                    if not (Elab.schemeBelongsToClassMethod methodNames clsMap method scheme)
                        then pure CalleeNotAttempted
                        else do
                            mReg <- getSharedClassReg legacyHooks
                            case mReg of
                                Nothing -> pure CalleeNotAttempted
                                Just classReg -> do
                                    sigs0 <- readIORef globalTypeSigsRef
                                    syns <- readIORef globalTypeSynonymsRef
                                    ctorTypes <- readIORef globalConstructorTypeRegistryRef
                                    let sigs = Map.insert method scheme sigs0
                                        scoped = Set.fromList [method, bare]
                                    result <- try (Elab.elaborateOwnedWithScopedSigs classReg sigs syns
                                                    ctorTypes owner scoped
                                                    Elab.InferFreely call)
                                        :: IO (Either SomeException (Expr, TA.Type))
                                    case result of
                                        Right (call', _)
                                            | hasTypedCallee call' -> do
                                                ok <- allTypedMethodsResolvable classReg call'
                                                pure (if ok then CalleeElaborated call'
                                                            else CalleeAttemptFailed)
                                        _ -> pure CalleeAttemptFailed
                _ -> pure CalleeNotAttempted
      where
        appHead (EApp h _) = appHead h
        appHead (ETyApp h _) = appHead h
        appHead (EVar n) = Just n
        appHead _ = Nothing

        hasTypedCallee (EApp h _) = hasTypedCallee h
        hasTypedCallee (ETyApp h _) = hasTypedCallee h
        hasTypedCallee ETypedMethod{} = True
        hasTypedCallee (EConstrainedValue _ constraints) = not (null constraints)
        hasTypedCallee _ = False


    -- Mark every application in a failed callee spine with an
    -- operationally-transparent wrapper.  Re-entering 'go' can then use the
    -- established value-directed fallback without retrying inference on
    -- progressively shorter prefixes.
    suppressCalleeElaboration (EApp h arg) =
        EApp (EConstrainedValue (suppressCalleeElaboration h) []) arg
    suppressCalleeElaboration other = other

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
        -- True iff a *concrete instance* (or a known equivalent such as
        -- Monad.return → Applicative.pure) backs this @(cls, tag, method)@.
        -- The class default tag @<default>@ must NOT count: every Foldable
        -- method has a default, so treating that as "resolvable" kept
        -- @ETypedMethod Foldable foldr Text@ / @… Stream@ rewrites for
        -- ordinary functions that merely share the name (@Data.Text.foldr@,
        -- Fusion.Common.foldr).  Those defaults are
        -- @foldr = foldr . toList@ / @toList = foldr (:) []@ and hang.
        -- Lookups go through 'lookupInstanceMethod', which drains the
        -- lazy-instance catalogue on miss.
        checkOne cls method tag
            -- A tyvar leftover (`f` from `Alternative f => …`) is not
            -- an instance.  Keep the original expression.
            | not (expectedTypeHasConcreteHead (TA.TyCon tag)) = pure False
            | otherwise = do
            direct <- lookupInstanceMethod reg cls tag method
            case nonPlaceholder direct of
                Just _  -> pure True
                Nothing -> do
                    -- Same structural match as resolveTypedMethod:
                    -- `MarkupM ()` inhabits `MarkupM a` (`a ~ ()`).
                    patterned <- lookupInstanceMethodPattern reg cls tag method
                    case nonPlaceholder patterned of
                        Just _  -> pure True
                        Nothing -> tryFb (typedMethodFallbacks cls method) tag

        tryFb [] _              = pure False
        tryFb ((c, m):rest) tag = do
            mv <- lookupInstanceMethod reg c tag m
            case nonPlaceholder mv of
                Just _  -> pure True
                Nothing -> tryFb rest tag

        nonPlaceholder mv = case mv of
            Just (VCon n []) | BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n -> Nothing
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
        go (ELocalSig _ inner)   = go inner
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

    -- Result-polymorphic fromString: expand Html/Markup synonyms and
    -- keep applied arguments (`MarkupM ()`), not the normalizeTyTag
    -- head.  Structural — no Markup/Html name list.
    isStringTyAppTag method tyBytes
        | lastNameComponent method == BC.pack "fromString" =
            case Elab.parseRawTypeExpr tyBytes of
                Just annTy -> do
                    syns <- readIORef globalTypeSynonymsRef
                    let expanded = expandTypeSynonyms syns annTy
                    case Elab.fromStringResultTag expanded of
                        Just t | not (BC.null t) -> pure t
                        _ -> pure (normalizeTyTag tyBytes)
                Nothing -> pure (normalizeTyTag tyBytes)
        | otherwise = pure (normalizeTyTag tyBytes)

    goTyApp e ty
        | isTypeLitsFn e = pure (tyAppLitsClosure (headName e) ty)
        | otherwise      = do
            v <- go e
            case v of
                VCon "Proxy" [] -> attachProxyType ty
                -- Multi-key class dispatch: @setField \@\"name\" \@User \@String@
                -- accumulates type-arg tags onto the dispatcher so the final
                -- call can look up the instance by composite key.
                VClassMethod m slot tags fn -> do
                    -- IsString.fromString is result-polymorphic: keep the
                    -- synonym-expanded structural tag (`Html` → `Markup`
                    -- → `MarkupM ()`) so `h1 "…"` / `"…" :: Html` hit
                    -- `instance (a ~ ()) => IsString (MarkupM a)`.
                    -- normalizeTyTag would collapse `MarkupM ()` to
                    -- `MarkupM` (head only).  Other methods still take
                    -- the head (`pure @Parser` → ParsecT).
                    tag <- isStringTyAppTag m ty
                    pure (VClassMethod m slot (tags ++ [tag]) fn)
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
                VIO action
                    | Just resultTy <- peekResultTy ty ->
                        -- Keep the ascription live while the IO runs.
                        -- getSockOpt is a VIO whose last stmt is unannotated
                        -- peek; PatternSignatures put CInt on this outer
                        -- action, not on that peek.
                        pure (VIO (withExpectedResultTag resultTy
                                     (action >>= applyIOResultAnnotation resultTy)))
                _               -> applyNumericTyAnnotation ty v
    tryTypedPeek :: Expr -> ByteString -> IO (Maybe Val)
    tryTypedPeek e ty = do
        -- CSaFamily = Word16, HostAddress = Word32: typed do-bind /
        -- case ascriptions arrive as the synonym name.  Expand before
        -- the size table so we peek 2/4 bytes, not the Word8 fallback.
        -- Newtypes (PortNumber = PortNum Word16) unwrap the same way.
        ty' <- expandTypeAnnBytes ty
        case (peekResultTy ty', peekArgs e) of
            (Just resultTy, Just (isElemOff, ptrE, offE)) -> do
                resultTy' <- expandTypeAnnBytes resultTy
                -- CInt = CInt Int32: peek the declared field and wrap.
                -- Host peekB is one byte; a bare VInt fails `CInt n <-`.
                mField <- lookupDeclaredFieldTag (tyAnnotationHead resultTy')
                case mField of
                    Just fieldTy -> do
                        fieldTy' <- expandTypeAnnBytes fieldTy
                        if knownPeekResultTy fieldTy'
                            then pure (Just (VIO (do
                                v <- typedPeek isElemOff fieldTy' ptrE offE
                                wrapExpectedNewtype resultTy' v)))
                            else do
                                -- A unary newtype can have a representation
                                -- that is not a primitive-sized host value
                                -- (In6Addr wraps a four-word tuple). In that
                                -- case its source Storable instance defines
                                -- the layout and must run instead of falling
                                -- through readTypedPeek's Word8 default.
                                mStor <- lookupStorablePeekMethod resultTy'
                                case mStor of
                                    Just peekFn -> pure (Just (VIO
                                        (runSourceStorablePeek peekFn isElemOff
                                            fieldTy' ptrE offE)))
                                    Nothing -> pure Nothing
                    Nothing -> do
                        resultTy'' <- expandNewtypeAnnBytes resultTy'
                        -- A newtype with its own Storable (PortNumber.peek =
                        -- PortNum . ntohs) must run that instance, not a raw
                        -- Word16 peek.  Local newtypes without Storable fall
                        -- through to the representation-sized typed peek.
                        if resultTy'' /= resultTy'
                            then do
                                mStor <- lookupStorablePeekMethod resultTy'
                                case mStor of
                                    Just peekFn ->
                                        pure (Just (VIO (runSourceStorablePeek
                                            peekFn isElemOff resultTy'' ptrE offE)))
                                    Nothing
                                        | knownPeekResultTy resultTy'' ->
                                            pure (Just (VIO (do
                                                v <- typedPeek isElemOff resultTy'' ptrE offE
                                                v' <- applyIOResultAnnotation resultTy' v
                                                wrapNewtypePeek resultTy' v')))
                                        | otherwise -> pure Nothing
                            else if knownPeekResultTy resultTy''
                                then pure (Just (VIO (do
                                    v <- typedPeek isElemOff resultTy'' ptrE offE
                                    v' <- applyIOResultAnnotation resultTy' v
                                    wrapExpectedNewtype resultTy' v')))
                                else do
                                    mStor <- lookupStorablePeekMethod resultTy'
                                    pure $ fmap (\peekFn -> VIO
                                        (runSourceStorablePeek peekFn isElemOff
                                            resultTy' ptrE offE)) mStor
            _ -> pure Nothing

    expandTypeAnnBytes :: ByteString -> IO ByteString
    expandTypeAnnBytes ann = do
        syns <- readIORef globalTypeSynonymsRef
        case Elab.parseRawTypeExpr ann of
            Nothing -> pure ann
            Just parsed ->
                let expanded = TA.expandTypeSynonyms syns parsed
                in case renderTypeAnnotation expanded of
                    Just bs -> pure bs
                    Nothing -> pure ann

    -- Unary newtype / single-field wrapper: PortNumber → Word16.
    -- Constructor metadata, not a name list.  Stop at a sized
    -- representation (do not unwrap Word16 → Word16#).
    expandNewtypeAnnBytes :: ByteString -> IO ByteString
    expandNewtypeAnnBytes ann
        | knownPeekResultTy ann = pure ann
        | otherwise = case Elab.parseRawTypeExpr ann of
            Nothing -> pure ann
            Just t  -> do
                ctors <- readIORef globalConstructorTypeRegistryRef
                case unwrapUnaryNewtype ctors t of
                    Just inner ->
                        case renderTypeAnnotation inner of
                            Just bs -> expandTypeAnnBytes bs >>= expandNewtypeAnnBytes
                            Nothing -> pure ann
                    Nothing -> pure ann

    unwrapUnaryNewtype ctors t =
        case TA.tyHead t of
            Nothing -> Nothing
            Just headName ->
                let bare = lastNameComponent (normalizeTyTag headName)
                    matches =
                        [ metadata
                        | metadata <- Map.elems ctors
                        , lastNameComponent (ciName (ctmIdentity metadata)) == bare
                          || tyHeadIs bare (ctmResultType metadata)
                        , length (ctmFieldTypes metadata) == 1
                        ]
                in case matches of
                    [metadata] ->
                        case ctmFieldTypes metadata of
                            [fieldTy] -> Just fieldTy
                            _         -> Nothing
                    _ -> Nothing

    tyHeadIs want ty =
        case TA.tyHead ty of
            Just n -> lastNameComponent (normalizeTyTag n) == want
            Nothing -> False

    wrapNewtypePeek :: ByteString -> Val -> IO Val
    wrapNewtypePeek origTy v = do
        parsed <- case Elab.parseRawTypeExpr origTy of
            Nothing -> pure Nothing
            Just t  -> do
                ctors <- readIORef globalConstructorTypeRegistryRef
                pure (unwrapUnaryNewtype ctors t >> unaryCtorName ctors t)
        case parsed of
            Just ctor -> do
                t <- newWHNFThunk v
                pure (VCon ctor [t])
            Nothing -> wrapExpectedNewtype origTy v

    unaryCtorName ctors t =
        case TA.tyHead t of
            Nothing -> Nothing
            Just headName ->
                let bare = lastNameComponent (normalizeTyTag headName)
                    matches =
                        [ ciName (ctmIdentity metadata)
                        | metadata <- Map.elems ctors
                        , tyHeadIs bare (ctmResultType metadata)
                        , length (ctmFieldTypes metadata) == 1
                        ]
                in case matches of
                    [ctor] -> Just (lastNameComponent ctor)
                    _      -> Nothing

    lookupStorablePeekMethod :: ByteString -> IO (Maybe Val)
    lookupStorablePeekMethod tag = do
        mReg <- getSharedClassReg legacyHooks
        case mReg of
            Nothing -> pure Nothing
            Just classReg -> do
                let stor = BC.pack "Storable"
                    peekN = BC.pack "peek"
                    headTag = lastNameComponent (normalizeTyTag tag)
                    lookupPeek = do
                        direct <- lookupInstanceMethod classReg stor headTag peekN
                        patterned <- case direct of
                            Just _ -> pure Nothing
                            Nothing -> lookupInstanceMethodPattern classReg stor headTag peekN
                        pure (direct <|> patterned)
                found <- lookupPeek
                found' <- case found of
                    Just _ -> pure found
                    Nothing -> do
                        triggerCoreInstanceLoadForTag legacyHooks stor headTag
                        lookupPeek
                mv' <- case found' of
                    Just v -> pure (Just v)
                    Nothing -> do
                        ctors <- readIORef globalConstructorTypeRegistryRef
                        case Elab.parseRawTypeExpr tag >>= unaryCtorName ctors of
                            Just ctor | ctor /= headTag ->
                                lookupInstanceMethod classReg stor ctor peekN
                            _ -> pure Nothing
                case mv' of
                    Just (VCon n [])
                        | BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n ->
                            pure Nothing
                    Just v -> do
                        ev <- try (forceMethodVal hooks v)
                            :: IO (Either SomeException Val)
                        case ev of
                            Right v' -> pure (Just v')
                            Left _   -> pure Nothing
                    Nothing -> pure Nothing

    -- Source Storable.peek at (ptr `plusPtr` off).  Apply to a
    -- VCon "Ptr" so source castPtr matches alloca / plusPtr.
    runSourceStorablePeek
        :: Val -> Bool -> ByteString -> Expr -> Expr -> IO Val
    runSourceStorablePeek peekFn isElemOff resultTy ptrE offE = do
        ptrV <- go ptrE
        offV <- go offE
        p <- valToHostPtr ptrV
        off <- case offV of
            VInt n     -> pure (fromIntegral n)
            VInteger n -> pure (fromInteger n)
            _ -> error ("typed peek: offset is not an Int: "
                        <> showValForDebug offV)
        let byteOff
                | isElemOff = off * typedPeekElemSize resultTy
                | otherwise = off
            p' = plusPtr p byteOff
        inner <- newWHNFThunk (VPrimObj (PrimPtr (castPtr p')))
        pT <- newWHNFThunk (VCon (BC.pack "Ptr") [inner])
        r0 <- apply hooks peekFn pT
        r <- forceMethodVal hooks r0
        runIOVal hooks r

    knownPeekResultTy ty =
        case tyAnnotationHead ty of
            "Ptr"      -> True
            "FunPtr"   -> True
            "Char"     -> True
            "Double"   -> True
            "Float"    -> True
            "Word8"    -> True
            "Word16"   -> True
            "Word32"   -> True
            "Word64"   -> True
            "Word"     -> True
            "Int8"     -> True
            "Int16"    -> True
            "Int32"    -> True
            "Int64"    -> True
            "Int"      -> True
            "CChar"    -> True
            "CUChar"   -> True
            "CBool"    -> True
            "CShort"   -> True
            "CUShort"  -> True
            "CInt"     -> True
            "CUInt"    -> True
            "CLong"    -> True
            "CULong"   -> True
            "CLLong"   -> True
            "CULLong"  -> True
            "CSize"    -> True
            "CSsize"   -> True
            "CSSize"   -> True
            "CIntPtr"  -> True
            "CUIntPtr" -> True
            "CPtrdiff" -> True
            _          -> False

    wrapExpectedNewtype :: ByteString -> Val -> IO Val
    wrapExpectedNewtype ty v = do
        let h0 = tyAnnotationHead ty
            lastSeg n = case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
                Just idx -> BC.drop (idx + 1) n
                Nothing  -> n
            h = lastSeg h0
        case v of
            VCon n _ | n == h || lastSeg n == h ->
                pure v
            _
                | not (isConcreteTyHead h) -> pure v
                | h `elem` map BC.pack
                    ["Int", "Integer", "Char", "Double", "Float", "Bool"
                    , "Word", "Word8", "Word16", "Word32", "Word64"
                    , "Int8", "Int16", "Int32", "Int64", "Ptr", "FunPtr"
                    , "IO", "ST", "STM"] ->
                    pure v
                | otherwise -> do
                    t <- newWHNFThunk v
                    pure (VCon h [t])

    -- @peek p :: IO CInt@ and PatternSignatures @n :: CInt <- peek p@
    -- (ETyApp on the action with a bare CInt) must both reach typed peek.
    peekResultTy :: ByteString -> Maybe ByteString
    peekResultTy ty =
        case ioResultAnnotation ty of
            Just resultTy -> Just resultTy
            Nothing       -> expectedResultHead ty

    expectedResultHead :: ByteString -> Maybe ByteString
    expectedResultHead ty =
        let h = tyAnnotationHead ty
        in if isConcreteTyHead h then Just h else Nothing

    isConcreteTyHead h = case BC.uncons h of
        Just (c, _) -> (c >= 'A' && c <= 'Z') || c == '(' || c == '['
        Nothing     -> False

    -- Unannotated @peek ptr@ inside a function whose caller ascription
    -- is @IO CInt@ / @CInt@ (getSockOpt).  The pointer has no CInt tag.
    -- socketPair leftover: peekArray's inner peekElemOff also has no
    -- surviving result type; the FFI @Ptr CInt@ mark on the buffer is
    -- the width.  A list ascription (@[CInt]@ / @[fd1,fd2] <-@) peels
    -- one layer so peekElemOff sees CInt, not [].
    tryExpectedTypedPeek :: Expr -> IO (Maybe Val)
    tryExpectedTypedPeek e = do
        mTag <- readExpectedResultTag
        case peekArgs e of
            Nothing -> pure Nothing
            Just (_, ptrE, _) -> do
                let fromTag = do
                        tag <- mTag
                        let peeled = peelListElemTy tag
                        if knownPeekResultTy peeled
                            then Just peeled
                            else if knownPeekResultTy tag
                                then Just tag
                                else Nothing
                mPtrTy <- case fromTag of
                    Just _  -> pure Nothing
                    -- Pointer metadata describes the element selected by
                    -- plain @peek@.  It says nothing about an arbitrary
                    -- field selected by @peekByteOff@; treating every field
                    -- of AddrInfo as another AddrInfo recursed forever.
                    Nothing | isBarePeekExpr e -> lookupMarkedPtrElemTy ptrE
                    Nothing -> pure Nothing
                case fromTag <|> mPtrTy of
                    Just resultTy -> tryTypedPeek e resultTy
                    Nothing       -> pure Nothing

    isBarePeekExpr expr = case stripTyApps expr of
        EApp fn _ -> isPeekHead fn
        _         -> False

    peelListElemTy :: ByteString -> ByteString
    peelListElemTy ty =
        case Elab.parseRawTypeExpr ty of
            Just parsed ->
                let (headTy, args) = TA.tyApps parsed
                in case (headTy, args) of
                    (TA.TyCon h, [arg])
                        | lastNameComponent (normalizeTyTag h) == BC.pack "[]" ->
                            case renderTypeAnnotation arg of
                                Just rendered -> rendered
                                Nothing       -> ty
                    _ ->
                        if BC.length ty >= 2 && BC.head ty == '[' && BC.last ty == ']'
                            then BC.init (BC.tail ty)
                            else ty
            Nothing ->
                if BC.length ty >= 2 && BC.head ty == '[' && BC.last ty == ']'
                    then BC.init (BC.tail ty)
                    else ty

    lookupMarkedPtrElemTy :: Expr -> IO (Maybe ByteString)
    lookupMarkedPtrElemTy ptrE = do
        ev <- try (do
            ptrV <- go ptrE
            p <- valToHostPtr ptrV
            lookupTypedHostPtr p) :: IO (Either SomeException (Maybe ByteString))
        case ev of
            Right (Just ty) | not (BC.null ty) -> pure (Just ty)
            _ -> pure Nothing

    lazyStorableWitnessTy :: Expr -> Expr -> Maybe ByteString
    lazyStorableWitnessTy fn arg =
        case (storableMethodHead fn, arg) of
            (Just method, ETyApp _ ty)
                | lastNameComponent method `elem` map BC.pack ["sizeOf", "alignment"] ->
                    Just ty
            _ -> Nothing
      where
        -- Only rewrite the original bare method.  Once the dictionary tag is
        -- attached as ETyApp, normal evaluation must proceed rather than
        -- wrapping it repeatedly.
        storableMethodHead (EVar method) = Just method
        storableMethodHead _             = Nothing

    typedPeek :: Bool -> ByteString -> Expr -> Expr -> IO Val
    typedPeek isElemOff resultTy ptrE offE = do
        ptrV <- go ptrE
        offV <- go offE
        p <- valToHostPtr ptrV
        off <- case offV of
            VInt n     -> pure (fromIntegral n)
            VInteger n -> pure (fromInteger n)
            _ -> error ("typed peek: offset is not an Int: "
                        <> showValForDebug offV)
        let byteOff
                | isElemOff = off * typedPeekElemSize resultTy
                | otherwise = off
        readTypedPeek resultTy p byteOff

    typedPeekElemSize :: ByteString -> Int
    typedPeekElemSize ty =
        case tyAnnotationHead ty of
            "Ptr"      -> ptrSize
            "FunPtr"   -> ptrSize
            "Char"     -> FStorable.sizeOf (undefined :: Char)
            "Double"   -> FStorable.sizeOf (undefined :: Double)
            "Float"    -> FStorable.sizeOf (undefined :: Float)
            "Word8"    -> FStorable.sizeOf (undefined :: Word8)
            "Word16"   -> FStorable.sizeOf (undefined :: Word16)
            "Word32"   -> FStorable.sizeOf (undefined :: Word32)
            "Word64"   -> FStorable.sizeOf (undefined :: Word64)
            "Word"     -> FStorable.sizeOf (undefined :: Word)
            "Int8"     -> FStorable.sizeOf (undefined :: Int8)
            "Int16"    -> FStorable.sizeOf (undefined :: Int16)
            "Int32"    -> FStorable.sizeOf (undefined :: Int32)
            "Int64"    -> FStorable.sizeOf (undefined :: Int64)
            "Int"      -> FStorable.sizeOf (undefined :: Int)
            "CChar"    -> FStorable.sizeOf (undefined :: Int8)
            "CUChar"   -> FStorable.sizeOf (undefined :: Word8)
            "CBool"    -> FStorable.sizeOf (undefined :: Word8)
            "CShort"   -> FStorable.sizeOf (undefined :: Int16)
            "CUShort"  -> FStorable.sizeOf (undefined :: Word16)
            "CInt"     -> FStorable.sizeOf (undefined :: Int32)
            "CUInt"    -> FStorable.sizeOf (undefined :: Word32)
            "CLong"    -> FStorable.sizeOf (undefined :: Int64)
            "CULong"   -> FStorable.sizeOf (undefined :: Word64)
            "CLLong"   -> FStorable.sizeOf (undefined :: Int64)
            "CULLong"  -> FStorable.sizeOf (undefined :: Word64)
            "CSize"    -> FStorable.sizeOf (undefined :: Word)
            "CSsize"   -> FStorable.sizeOf (undefined :: Int64)
            "CSSize"   -> FStorable.sizeOf (undefined :: Int64)
            "CIntPtr"  -> FStorable.sizeOf (undefined :: Int64)
            "CUIntPtr" -> FStorable.sizeOf (undefined :: Word64)
            "CPtrdiff" -> FStorable.sizeOf (undefined :: Int64)
            _          -> FStorable.sizeOf (undefined :: Word8)
      where
        ptrSize = FStorable.sizeOf (undefined :: Ptr Word8)

    readTypedPeek :: ByteString -> Ptr Word8 -> Int -> IO Val
    readTypedPeek ty p off =
        case tyAnnotationHead ty of
            "Ptr"      -> readPtr
            "FunPtr"   -> readPtr
            "Char"     -> readChar
            "Double"   -> VFloat <$> (FStorable.peekByteOff (castPtr p :: Ptr Double) off :: IO Double)
            "Float"    -> do
                x <- FStorable.peekByteOff (castPtr p :: Ptr Float) off :: IO Float
                pure (VFloat (realToFrac x))
            "Word8"    -> readWord8
            "Word16"   -> readWord16
            "Word32"   -> readWord32
            "Word64"   -> readWord64
            "Word"     -> readWord
            "Int8"     -> readInt8
            "Int16"    -> readInt16
            "Int32"    -> readInt32
            "Int64"    -> readInt64
            "Int"      -> readInt
            "CChar"    -> readInt8
            "CUChar"   -> readWord8
            "CBool"    -> readWord8
            "CShort"   -> readInt16
            "CUShort"  -> readWord16
            "CInt"     -> readInt32
            "CUInt"    -> readWord32
            "CLong"    -> readInt64
            "CULong"   -> readWord64
            "CLLong"   -> readInt64
            "CULLong"  -> readWord64
            "CSize"    -> readWord
            "CSsize"   -> readInt64
            "CSSize"   -> readInt64
            "CIntPtr"  -> readInt64
            "CUIntPtr" -> readWord64
            "CPtrdiff" -> readInt64
            _          -> readWord8
      where
        readPtr = do
            raw <- FStorable.peekByteOff (castPtr p :: Ptr Word64) off :: IO Word64
            pure (VPrimObj (PrimPtr (castPtr (intPtrToPtr (fromIntegral raw)))))
        readChar = do
            c <- FStorable.peekByteOff (castPtr p :: Ptr Char) off :: IO Char
            pure (VChar c)
        readWord8 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Word8) off :: IO Word8
            pure (VInt (fromIntegral x))
        readWord16 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Word16) off :: IO Word16
            pure (VInt (fromIntegral x))
        readWord32 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Word32) off :: IO Word32
            pure (VInt (fromIntegral x))
        readWord64 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Word64) off :: IO Word64
            pure (VInt (fromIntegral x))
        readWord = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Word) off :: IO Word
            pure (VInt (fromIntegral x))
        readInt8 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Int8) off :: IO Int8
            pure (VInt (fromIntegral x))
        readInt16 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Int16) off :: IO Int16
            pure (VInt (fromIntegral x))
        readInt32 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Int32) off :: IO Int32
            pure (VInt (fromIntegral x))
        readInt64 = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Int64) off :: IO Int64
            pure (VInt x)
        readInt = do
            x <- FStorable.peekByteOff (castPtr p :: Ptr Int) off :: IO Int
            pure (VInt (fromIntegral x))

    valToHostPtr :: Val -> IO (Ptr Word8)
    valToHostPtr (VPrimObj (PrimPtr p)) = pure (castPtr p)
    valToHostPtr (VCon "Ptr" [t]) = force hooks t >>= valToHostPtr
    valToHostPtr (VInt n) = pure (intPtrToPtr (fromIntegral n))
    valToHostPtr (VInteger n) = pure (intPtrToPtr (fromInteger n))
    valToHostPtr VUnit = pure nullPtr
    valToHostPtr other =
        error ("typed peek: expected Ptr, got " <> showValForDebug other)

    applyIOResultAnnotation :: ByteString -> Val -> IO Val
    applyIOResultAnnotation resultTy v = do
        case ptrPointeeHead resultTy of
            Just elemTy -> markTypedPtrVal elemTy v
            _ -> pure ()
        v' <- applyNumericTyAnnotation resultTy v
        -- GND wrap only: CInt / local `newtype W = W Int32`.  Do not
        -- wrap String / lists — `IO String` is not a unary newtype.
        -- getSockOpt `n :: CInt <- peek p` ascribes the action; the
        -- host peek is a bare VInt until this wrap.
        mField <- lookupDeclaredFieldTag (tyAnnotationHead resultTy)
        case mField of
            Just _  -> wrapExpectedNewtype resultTy v'
            Nothing -> pure v'

    markTypedPtrVal :: ByteString -> Val -> IO ()
    markTypedPtrVal elemTy (VPrimObj (PrimPtr p)) =
        markTypedHostPtr p elemTy
    markTypedPtrVal elemTy (VCon "Ptr" [pT]) =
        force hooks pT >>= markTypedPtrVal elemTy
    markTypedPtrVal _ _ =
        pure ()

    ptrPointeeHead :: ByteString -> Maybe ByteString
    ptrPointeeHead ty = do
        parsed <- Elab.parseRawTypeExpr ty
        let (headTy, args) = TA.tyApps parsed
        case (headTy, args) of
            (TA.TyCon h, [arg])
                | lastNameComponent (normalizeTyTag h) == BC.pack "Ptr" ->
                    lastNameComponent . normalizeTyTag <$> TA.tyHead arg
            _ -> Nothing

    peekArgs :: Expr -> Maybe (Bool, Expr, Expr)
    peekArgs expr = case stripTyApps expr of
        EApp fn ptrE
            | isPeekHead fn -> Just (False, ptrE, ELit (LInt 0))
        EApp (EApp fn ptrE) offE
            | isPeekByteOffHead fn -> Just (False, ptrE, offE)
            | isPeekElemOffHead fn -> Just (True, ptrE, offE)
        EApp (ELam n body) ptrE ->
            case stripTyApps body of
                EApp (EApp fn (EVar v)) offE
                    | v == n
                    , isPeekByteOffHead fn -> Just (False, ptrE, offE)
                    | v == n
                    , isPeekElemOffHead fn -> Just (True, ptrE, offE)
                _ -> Nothing
        _ -> Nothing

    isPeekHead :: Expr -> Bool
    isPeekHead (EVar n) =
        lastNameComponent n == BC.pack "peek"
    isPeekHead (ETypedMethod _ n _) =
        lastNameComponent n == BC.pack "peek"
    isPeekHead (ETyApp inner _) = isPeekHead inner
    isPeekHead _ = False

    isPeekByteOffHead :: Expr -> Bool
    isPeekByteOffHead (EVar n) =
        lastNameComponent n == BC.pack "peekByteOff"
    isPeekByteOffHead (ETypedMethod _ n _) =
        lastNameComponent n == BC.pack "peekByteOff"
    isPeekByteOffHead (ETyApp inner _) = isPeekByteOffHead inner
    isPeekByteOffHead _ = False

    isPeekElemOffHead :: Expr -> Bool
    isPeekElemOffHead (EVar n) =
        lastNameComponent n == BC.pack "peekElemOff"
    isPeekElemOffHead (ETypedMethod _ n _) =
        lastNameComponent n == BC.pack "peekElemOff"
    isPeekElemOffHead (ETyApp inner _) = isPeekElemOffHead inner
    isPeekElemOffHead _ = False

    stripTyApps :: Expr -> Expr
    stripTyApps (ETyApp inner _) = stripTyApps inner
    stripTyApps other           = other

    -- Explicit numeric ascriptions stand in for the typechecker when an
    -- overloaded expression has already evaluated to IHC's default integral
    -- representation.  This lets code such as
    -- @sqrt (fromIntegral n :: Double)@ dispatch on the annotated result type
    -- instead of the intermediate @Int@ runtime tag.
    applyNumericTyAnnotation :: ByteString -> Val -> IO Val
    applyNumericTyAnnotation ty v =
        case tyAnnotationHead ty of
            "Double" -> pure (toFloat v)
            "Float"  -> pure (toFloat v)
            -- Fixed-width words need a carrier so Num dispatch hits
            -- Word8/Word/… instances, not bare Int (VInt).
            "Word8"  -> boxWordCtor "W8#" 0xff v
            "Word16" -> boxWordCtor "W16#" 0xffff v
            "Word32" -> boxWordCtor "W32#" 0xffffffff v
            "Word64" -> boxWordCtor "W64#" maxBound v
            "Word"   -> boxWordCtor "W#" maxBound v
            -- Optimistic OverloadedStrings: @"…" :: ByteString@ still
            -- evaluates the literal as a [Char] list (or VStr).  Pack it
            -- into a real BS so S.any / sanitizeHeaders / peekFp see a
            -- source-shaped ForeignPtr (packChars path), not a char list.
            "ByteString" -> toByteString v
            "StrictByteString" -> toByteString v
            -- @"…" :: CI ByteString@ (http-types HeaderName, warp headers).
            -- Without this the literal stays a [Char] list; CI field
            -- accessors then synthesise a VStr for foldedCase and
            -- BS.== / responseKeyIndex hang or miss.  Build a real
            -- @CI original foldedCase@ of packed ByteStrings.
            "CI"
                | BC.pack "ByteString" `BC.isInfixOf` ty -> toCIByteString v
                | otherwise -> pure v
            h | h `elem` integralAnnotationHeads -> pure (toIntegral v)
            _        -> pure v
      where
        toFloat (VInt n)     = VFloat (fromIntegral n)
        toFloat (VInteger n) = VFloat (fromInteger n)
        toFloat other        = other

        toIntegral (VPrimObj (PrimPtr p)) =
            VInt (fromIntegral (ptrToIntPtr p))
        toIntegral other = other

        toByteString v0 = do
            mBs <- charListToByteStringVal hooks v0
            case mBs of
                Just bsV -> pure bsV
                Nothing  -> pure v0

        -- IsString (CI ByteString) ≅ mk . fromString: pack the chars,
        -- then foldCase via ASCII lower (FoldCase ByteString = B.map
        -- toLower8).  Already-a-CI values pass through.
        toCIByteString v0@(VCon "CI" _) = pure v0
        toCIByteString v0 = do
            charsM <- extractCharList v0
            case charsM of
                Nothing -> pure v0
                Just chars -> do
                    -- Independent BS buffers for strict CI fields.
                    origV <- byteStringConFromBS (BC.pack chars)
                    foldedV <- byteStringConFromBS
                        (BC.pack (map toLower chars))
                    origT <- newWHNFThunk origV
                    foldedT <- newWHNFThunk foldedV
                    pure (VCon "CI" [origT, foldedT])

        extractCharList :: Val -> IO (Maybe String)
        extractCharList = go []
          where
            go acc (VCon "[]" []) = pure (Just (reverse acc))
            go acc (VCon ":" [hT, tT]) = do
                hv <- force hooks hT
                case hv of
                    VChar c -> do
                        tv <- force hooks tT
                        go (c : acc) tv
                    _ -> pure Nothing
            go acc (VStr bs) = pure (Just (reverse acc ++ BC.unpack bs))
            go _ _ = pure Nothing
        boxWordCtor :: ByteString -> Int64 -> Val -> IO Val
        boxWordCtor ctor mask v0 = case asWordBits v0 of
            Just n -> do
                t <- newWHNFThunk (VInt (n .&. mask))
                pure (VCon ctor [t])
            Nothing -> pure v0

        asWordBits (VInt n) = Just n
        asWordBits (VInteger n)
            | n >= 0
            , n <= toInteger (maxBound :: Word64) = Just (fromIntegral n)
        asWordBits (VCon "W8#" _)  = Nothing  -- already boxed; leave alone below
        asWordBits _ = Nothing

        integralAnnotationHeads =
            [ "Int", "Word", "Word8", "Word16", "Word32", "Word64"
            , "Int8", "Int16", "Int32", "Int64"
            , "CSize", "CInt", "CLong", "CULong", "CUInt"
            , "CChar", "CUChar", "CShort", "CUShort"
            , "CLLong", "CULLong", "CBool"
            , "CSsize", "CSSize", "CIntPtr", "CUIntPtr", "CPtrdiff"
            ]

    ioResultAnnotation :: ByteString -> Maybe ByteString
    ioResultAnnotation ty = do
        parsed <- Elab.parseRawTypeExpr ty
        let (headTy, args) = TA.tyApps parsed
        case (headTy, args) of
            (TA.TyCon h, [arg])
                | lastNameComponent (normalizeTyTag h) == BC.pack "IO" ->
                    renderTypeAnnotation arg
            _ -> Nothing

    renderTypeAnnotation :: TA.Type -> Maybe ByteString
    renderTypeAnnotation = top
      where
        top t = case t of
            TA.TyVar v   -> Just (bareTypeName v)
            TA.TyCon c   -> Just (bareTypeName c)
            TA.TyApp _ _ -> let (h, args) = TA.tyApps t in renderApp h args
            _            -> Nothing

        renderApp h args = do
            hb <- atom h
            parts <- mapM atom args
            pure (BC.intercalate (BC.singleton ' ') (hb : parts))

        atom t = case t of
            TA.TyVar v   -> Just (bareTypeName v)
            TA.TyCon c   -> Just (bareTypeName c)
            TA.TyApp _ _ -> do
                inner <- top t
                Just (BC.concat [BC.singleton '(', inner, BC.singleton ')'])
            _            -> Nothing

        bareTypeName c =
            case BC.elemIndexEnd (toEnum (fromEnum '.')) c of
                Just idx | idx + 1 < BC.length c -> BC.drop (idx + 1) c
                _ -> c

    tyAnnotationHead :: ByteString -> ByteString
    tyAnnotationHead ty =
        let rawHead = case Elab.parseRawTypeExpr ty >>= TA.tyHead of
                Just h  -> h
                Nothing -> ty
        in lastNameComponent (normalizeTyTag rawHead)

    lastNameComponent :: ByteString -> ByteString
    lastNameComponent n =
        case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
            Just idx -> BC.drop (idx + 1) n
            Nothing  -> n

    -- Pattern match alternatives. Returns the matched alt's body or
    -- raises PatternMatchFail.
    tryAltsFromThunk :: Thunk -> [Alt] -> IO Val
    tryAltsFromThunk scrutT alts0 = goAlts alts0
      where
        goAlts [] = do
            v <- forceCaseScrut scrutT []
            throwIO (PatternMatchFail
                ("case: non-exhaustive patterns for "
                 <> showValForDebug v
                 <> " in alternatives "
                 <> show (map (\(Alt p _) -> p) alts0)))
        goAlts current@(Alt pat body : rest)
            | Just inner <- lazyAltPat pat = do
                bindings <- bindIrrefutablePat inner scrutT
                r <- try (eval hooks (extendEnvMany bindings env) ipm body)
                       :: IO (Either PatternMatchFail Val)
                case r of
                    Right result -> pure result
                    Left (PatternMatchFail "guard failed") -> goAlts rest
                    Left err -> throwIO err
            | otherwise = do
                v <- forceCaseScrut scrutT current
                tryAlts v current

        lazyAltPat (PIrref p) = Just p
        lazyAltPat _          = Nothing

    bindIrrefutablePat :: Pat -> Thunk -> IO [(Name, Thunk)]
    bindIrrefutablePat pat scrutT =
        traverse bindName (patternVars pat)
      where
        bindName name = do
            t <- newLazyBuiltinThunk $ do
                v <- force hooks scrutT
                m <- matchPat hooks pat v
                case m of
                    Just bs -> case lookup name bs of
                        Just t' -> force hooks t'
                        Nothing ->
                            error ("IHC.Eval: irrefutable pattern: variable `"
                                   <> BC.unpack name
                                   <> "` not bound by inner pattern "
                                   <> show pat)
                    Nothing ->
                        error ("Irrefutable pattern failed for pattern "
                               <> show pat
                               <> "; scrutinee was "
                               <> showValForDebug v)
            pure (name, t)

    forceCaseScrut :: Thunk -> [Alt] -> IO Val
    forceCaseScrut scrutT altsForMatch = do
        v0 <- force hooks scrutT
        -- Primops like `newByteArray#` return an internal VIO wrapper around
        -- their unboxed-tuple result. A refutable case must force that wrapper
        -- before matching, but must not eagerly execute source-built `IO` /
        -- `ST` constructors, which use `VCon`.
        --
        -- The exception combinators (`catch`, `mask`/`block`/`unblock`,
        -- `bracket`, `finally`, `onException`, ...) are source-loaded and
        -- destructure the `IO` newtype carrier directly, e.g.
        --   catch (IO io) handler   = IO $ catch# io handler'
        --   unsafeUnmask (IO io)    = IO $ unmaskAsyncExceptions# io
        -- When the scrutinee of such a match is a `VIO` action, running it
        -- here (via `runIOVal`) would execute the protected computation
        -- OUTSIDE the `catch#` frame, so an exception it raises escapes the
        -- intended handler. Suppress the eager run whenever a remaining alt
        -- destructures the `IO`/`ST`/`STM` newtype carrier: `matchPat`'s
        -- @PCon "IO"@/@"ST"@/@"STM"@ arms wrap the `VIO` lazily as a
        -- State#-passing function without forcing it.
        let destructuresMonadCarrier =
                any (\(Alt p _) -> patHeadIsMonadCarrier p) altsForMatch
            -- Leftover State# VFun (BuildStep / copyBytes coerce
            -- dropped the IO newtype) cased as the IO *result*
            -- (Finished / Yield1 / Done / …).  Same peel as VIO.
            -- Skip PVar-only and IO/ST/STM-carrier alts so
            -- `case f of x ->` and `catch (IO io)` stay unrun.
            -- No name list of result constructors.
            casesDataCtor =
                any (\(Alt p _) -> patIsDataCtor p) altsForMatch
            -- bindIO / thenIO: `case m s of (# new_s, a #) -> …`.
            -- After the IO newtype is dropped, `m s` can still be a
            -- leftover State# VFun (one apply short).  Apply it so
            -- the `(#,#)` alt sees the tuple — do not runIOVal
            -- (that unwraps to `a`).  Warp.run after bind+listen
            -- hits this on acceptLoop's try/catch (curl then RST).
            wantsUnboxedStateTuple =
                any (\(Alt p _) -> patHeadIsUnboxedStateTuple p) altsForMatch
        case v0 of
            VIO _ | not destructuresMonadCarrier -> runIOVal hooks v0
            fn | isLeftoverStateFun fn && wantsUnboxedStateTuple -> do
                result <- applyLeftoverStateFun hooks fn
                -- Cons PAP (`:` ) is also VFun; applying RealWorld
                -- saturates it to `<:...>`.  Only rematch a State#
                -- result (`(#,#)`, IO/ST, VIO).
                if isLeftoverStateResult result
                    then case result of
                        VIO _ | not destructuresMonadCarrier ->
                            runIOVal hooks result
                        _ -> pure result
                    else pure fn
            leftoverFn@(VFun _)
                | not destructuresMonadCarrier && casesDataCtor -> do
                    v1 <- runIOVal hooks leftoverFn
                    case v1 of
                        VIO _ -> runIOVal hooks v1
                        _     -> pure v1
            leftoverFn@(VFunIP _ _)
                | not destructuresMonadCarrier && casesDataCtor -> do
                    v1 <- runIOVal hooks leftoverFn
                    case v1 of
                        VIO _ -> runIOVal hooks v1
                        _     -> pure v1
            _ -> pure v0

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
                    typedBindings <- attachPatternFieldEvidence pat bindings
                    r <- try (eval hooks (extendEnvMany typedBindings env) ipm body)
                           :: IO (Either PatternMatchFail Val)
                    case r of
                        Right result -> pure result
                        Left (PatternMatchFail "guard failed") -> goAlts rest
                        Left err -> throwIO err
                Nothing -> goAlts rest

    attachPatternFieldEvidence pat bindings = do
        registry <- readIORef globalConstructorTypeRegistryRef
        owner <- currentOwner hooks env
        let evidence = collectEvidence registry owner pat
        traverse (wrapBinding evidence) bindings

    wrapBinding evidence (name, canonical) = case Map.lookup name evidence of
        Just (scheme, fieldOwner) -> do
            state <- readIORef canonical
            case state of
                TypedField{} -> pure (name, canonical)
                _ -> do
                    view <- newIORef (TypedField canonical scheme fieldOwner)
                    pure (name, view)
        Nothing -> pure (name, canonical)

    collectEvidence registry owner pat = case pat of
        PCon ctor fields -> case constructorMetadata registry owner ctor of
            Just metadata
                | not (ctmDataFamily metadata)
                , null (ctmExistentialVars metadata)
                , length fields == length (ctmFieldTypes metadata) ->
                    Map.unions (zipWith
                        (fieldEvidence (ciOwner (ctmIdentity metadata)))
                        fields (ctmFieldTypes metadata))
            _ -> Map.empty
        PAs _ inner -> collectEvidence registry owner inner
        PBang inner -> collectEvidence registry owner inner
        PIrref inner -> collectEvidence registry owner inner
        _ -> Map.empty
      where
        fieldEvidence fieldOwner fieldPat fieldTy = case fieldPat of
            PVar name | containsForall fieldTy ->
                Map.singleton name (fieldScheme fieldTy, fieldOwner)
            PAs name inner -> Map.insert name (fieldScheme fieldTy, fieldOwner)
                (fieldEvidence fieldOwner inner fieldTy)
            PBang inner -> fieldEvidence fieldOwner inner fieldTy
            PIrref inner -> fieldEvidence fieldOwner inner fieldTy
            _ -> Map.empty

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

-- | Does this pattern destructure the @IO@ / @ST@ / @STM@ newtype
-- carrier?  Such a match (e.g. @catch (IO io) h = …@,
-- @unsafeUnmask (IO io) = …@) must NOT trigger the @ECase@ eager-run
-- heuristic: running the wrapped @VIO@ there would execute the protected
-- action outside the surrounding @catch#@ / @mask@ frame and lose the
-- exception-path cleanup.  See the long note at @go (ECase …)@.
--
-- We look through @!p@ / @~p@ / @\@as@ wrappers (GHC accepts
-- @catch !(IO io)@ — see @catchAny@) so the head ctor is still found.
patHeadIsMonadCarrier :: Pat -> Bool
patHeadIsMonadCarrier (PCon n [_]) = n `elem` monadCarrierCtors
patHeadIsMonadCarrier (PBang inner)  = patHeadIsMonadCarrier inner
patHeadIsMonadCarrier (PIrref inner) = patHeadIsMonadCarrier inner
patHeadIsMonadCarrier (PAs _ inner)  = patHeadIsMonadCarrier inner
patHeadIsMonadCarrier _              = False

patIsDataCtor :: Pat -> Bool
patIsDataCtor p@(PCon n _) =
    -- thenIO / bindIO case `(# new_s, a #)` on a leftover VFun.
    -- Unboxed state tuples are not Finished/Yield1-shaped data:
    -- treating them as such ran a cons PAP (`:`) via runIOVal and
    -- leftover-failed as `<:...>`.  Rematch peels real State#.
    not (patHeadIsMonadCarrier p) && not (isUnboxedStateTupleName n)
patIsDataCtor (PBang inner)  = patIsDataCtor inner
patIsDataCtor (PIrref inner) = patIsDataCtor inner
patIsDataCtor (PAs _ inner)  = patIsDataCtor inner
patIsDataCtor _              = False

patHeadIsUnboxedStateTuple :: Pat -> Bool
patHeadIsUnboxedStateTuple (PCon n _) = isUnboxedStateTupleName n
patHeadIsUnboxedStateTuple (PBang inner)  = patHeadIsUnboxedStateTuple inner
patHeadIsUnboxedStateTuple (PIrref inner) = patHeadIsUnboxedStateTuple inner
patHeadIsUnboxedStateTuple (PAs _ inner)  = patHeadIsUnboxedStateTuple inner
patHeadIsUnboxedStateTuple _              = False

-- | The single-field newtype constructors whose runtime carrier is a
-- @VIO@ thunk the interpreter must hand to 'matchPat' unrun.
monadCarrierCtors :: [Name]
monadCarrierCtors = [BC.pack "IO", BC.pack "ST", BC.pack "STM"]

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
matchStringPatList hooks s0 v0 = do
    mBs <- byteStringPayloadBytes hooks v0
    case mBs of
        Just bs
            | bs == s0  -> pure (Just [])
            | otherwise -> pure Nothing
        Nothing -> go (BC.unpack s0) v0
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

-- Packed bytes of a source-shaped ByteString / VStr, for PLit LStr.
-- http-types decodePathSegments special-cases "" and "/" via
-- OverloadedStrings ByteString patterns; those used to miss VCon BS/PS.
byteStringPayloadBytes :: IHCHooks -> Val -> IO (Maybe ByteString)
byteStringPayloadBytes _ (VStr bs) = pure (Just bs)
byteStringPayloadBytes hooks (VCon n fields)
    | sameConName n (BC.pack "BS")
    , [fpT, lenT] <- fields = do
        lenv <- force hooks lenT
        mLen <- unwrapIntPayload hooks lenv
        case mLen of
            Just 0 -> pure (Just BS.empty)
            Just nLen | nLen > 0 -> do
                fpv <- force hooks fpT
                mPtr <- foreignPtrBasePtr hooks fpv
                case mPtr of
                    Just p -> Just <$> BS.packCStringLen (castPtr p, nLen)
                    Nothing -> pure Nothing
            _ -> pure Nothing
    | sameConName n (BC.pack "PS")
    , [fpT, offT, lenT] <- fields = do
        offv <- force hooks offT
        lenv <- force hooks lenT
        mOff <- unwrapIntPayload hooks offv
        mLen <- unwrapIntPayload hooks lenv
        case (mOff, mLen) of
            (Just _, Just 0) -> pure (Just BS.empty)
            (Just off, Just nLen) | nLen > 0, off >= 0 -> do
                fpv <- force hooks fpT
                mPtr <- foreignPtrBasePtr hooks fpv
                case mPtr of
                    Just p -> Just <$> BS.packCStringLen
                        (castPtr (p `plusPtr` off), nLen)
                    Nothing -> pure Nothing
            _ -> pure Nothing
byteStringPayloadBytes _ _ = pure Nothing

unwrapIntPayload :: IHCHooks -> Val -> IO (Maybe Int)
unwrapIntPayload _ (VInt n) = pure (Just (fromIntegral n))
unwrapIntPayload _ (VInteger n) = pure (Just (fromInteger n))
unwrapIntPayload hooks (VCon c [t])
    | isIntHashCtor c || bareConName c `elem` numericNewtypeCons = do
        inner <- force hooks t
        unwrapIntPayload hooks inner
unwrapIntPayload _ _ = pure Nothing

foreignPtrBasePtr :: IHCHooks -> Val -> IO (Maybe (Ptr Word8))
foreignPtrBasePtr _ (VPrimObj (PrimPtr p)) =
    pure (Just (castPtr p))
foreignPtrBasePtr _ (VPrimObj (PrimForeignPtr fp)) =
    pure (Just (castPtr (unsafeForeignPtrToPtr fp)))
foreignPtrBasePtr hooks (VCon n fields)
    | sameConName n (BC.pack "ForeignPtr")
    , addrT:_ <- fields = do
        av <- force hooks addrT
        foreignPtrBasePtr hooks av
    | sameConName n (BC.pack "Ptr")
    , [addrT] <- fields = do
        av <- force hooks addrT
        foreignPtrBasePtr hooks av
foreignPtrBasePtr _ _ = pure Nothing

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

-- Numeric newtypes whose constructors are representation-transparent after
-- source-loaded coerce/fromIntegral paths. Keep this explicit; unwrapping every
-- single-field constructor would make ordinary ADTs match primitive patterns.
numericNewtypeCons :: [Name]
numericNewtypeCons =
    [ "CSize", "CInt", "CLong", "CULong", "CUInt", "CChar", "CUChar"
    , "CShort", "CUShort", "CLLong", "CULLong"
    , "CSsize", "CSSize", "CIntPtr", "CUIntPtr", "CPtrdiff"
    , "Int8", "Int16", "Int32", "Int64"
    , "Word", "Word8", "Word16", "Word32", "Word64"
    ]

intSizedPrimCons :: [Name]
intSizedPrimCons = ["I8#", "I16#", "I32#", "I64#"]

wordSizedPrimCons :: [Name]
wordSizedPrimCons = ["W8#", "W16#", "W32#", "W64#"]

-- | @W#@ plus the fixed-width word prim wrappers.  Source Bits/Num
-- instances pattern-match these; optimistic fromIntegral leaves a
-- bare 'VInt'.  One predicate so matchPat does not grow a per-width
-- name list (W16# / W32# / W64# used to miss the VInt bridge that
-- W# / W8# already had).
isWordPrimCon :: Name -> Bool
isWordPrimCon n =
    let b = bareConName n
    in b == "W#" || b `elem` wordSizedPrimCons

-- | Small Integers that can sit in the interpreter's VInt payload.
wordPrimIntegerInRange :: Name -> Integer -> Bool
wordPrimIntegerInRange c n
    -- Preserve the existing W8# tight range; other widths accept any
    -- Int64-sized Integer the way W# already does.
    | bareConName c == "W8#" = n >= 0 && n <= 255
    | otherwise =
        n >= toInteger (minBound :: Int64)
        && n <= toInteger (maxBound :: Int64)

bareConName :: Name -> Name
bareConName n = case BC.elemIndexEnd '.' n of
    Just idx -> BC.drop (idx + 1) n
    Nothing  -> n

sameConName :: Name -> Name -> Bool
sameConName a b = a == b || bareConName a == bareConName b

recordUpdateFieldProjName :: Name -> Name
recordUpdateFieldProjName fname = BC.pack "$fldProj$" <> fname

-- | Apply @base { f = e, ... }@ to a WHNF value.  Slot indices come
-- from the field accessor already in the env (or the @$fldProj$@ /
-- fallback binding).  Arity comes from the value, never from a merged
-- FieldRegistry homonym.
applyRecordUpdate
    :: IHCHooks
    -> Env
    -> ImplicitParamMap
    -> (Expr -> IO Val)
    -> Val
    -> [(Name, Expr)]
    -> IO Val
applyRecordUpdate hooks env _ipm evalExpr base updates = goVal base
  where
    goVal (VCon "SomeException" [innerT]) = do
        inner <- force hooks innerT
        goVal inner
    goVal (VCon conName args) = do
        newArgs <- foldM (patchSlot conName) args updates
        pure (VCon conName newArgs)
    goVal other
        -- Erased newtype: a single-field update at index 0 is the new
        -- payload itself (mirrors the field-accessor transparent path).
        | [(fname, expr)] <- updates = do
            mClauses <- resolveFieldClauses hooks env fname
            case mClauses of
                Just [(_, 0)] -> evalExpr expr
                Just clauses
                    | all (\(_, i) -> i == 0) clauses -> evalExpr expr
                _ -> unknown other
        | otherwise = unknown other

    patchSlot conName args (fname, expr) = do
        newV <- evalExpr expr
        newT <- newWHNFThunk newV
        mClauses <- resolveFieldClauses hooks env fname
        case mClauses >>= pickFieldIndex conName (length args) of
            Just idx -> pure (replaceAt idx newT args)
            Nothing  -> unknown (VCon conName args)

    unknown _ = do
        t <- newWHNFThunk (VStr (BC.pack "record update: unknown constructor"))
        throwIO (IhcException (BC.pack "record update: unknown constructor") t)

pickFieldIndex :: Name -> Int -> [(Name, Int)] -> Maybe Int
pickFieldIndex conName arity clauses =
    case [idx | (n, idx) <- clauses, sameConName n conName, idx < arity] of
        (i:_) -> Just i
        []    -> case [idx | (_, idx) <- clauses, idx < arity] of
            (i:_) -> Just i
            []    -> Nothing

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs =
    [ if j == i then x else y
    | (j, y) <- zip [0 ..] xs
    ]

resolveFieldClauses :: IHCHooks -> Env -> Name -> IO (Maybe [(Name, Int)])
resolveFieldClauses hooks env fname = firstClauses
    [fname, recordUpdateFieldProjName fname]
  where
    firstClauses [] = pure Nothing
    firstClauses (k:ks) = do
        mVal <- lookupForced k
        case mVal of
            Just (VFieldAccessor _ clauses _ _) -> pure (Just clauses)
            _ -> firstClauses ks

    lookupForced k = case lookupEnv k env of
        Just t -> Just <$> force hooks t
        Nothing -> do
            owner <- currentOwner hooks env
            mT <- lookupEnvFallback hooks owner k
            case mT of
                Just t  -> Just <$> force hooks t
                Nothing -> pure Nothing

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
                           <> show p
                           <> "; scrutinee was "
                           <> showValForDebug v)
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
        -- Unboxed 2-tuple constructor name as used elsewhere in IHC.
        unboxed2 = BC.pack "(#,#)"
    case v of
        VCon cn vthunks
            | cn == tupleName && length vthunks == arity ->
                matchPat hooks (PCon tupleName ps) v
            -- Lazy ST (Control.Monad.ST.Lazy) binds results as boxed
            -- pairs @(r, new_s)@ via irrefutable patterns, while
            -- strict ST / runSTArray produce unboxed
            -- @(# State# s, a #)@.  Bridge both orders:
            --   boxed (val, state)  ←→  unboxed (# state, val #)
            | arity == 2
            , cn == unboxed2
            , length vthunks == 2
            , [pVal, pState] <- ps
            , [stateT, valT] <- vthunks ->
                matchFields hooks [(pVal, valT), (pState, stateT)] []
        _ -> pure Nothing
matchPat hooks (PLit (LInt n)) (VInt m)
    | n == m    = pure (Just [])
    | otherwise = pure Nothing
-- UNPACK Int fields (ByteString's length, etc.) arrive as I# / IS.
-- `hPut _ (BS _ 0)` must compare the literal against the inner Int#,
-- not treat I# as a failed PLit (or worse, as a wildcard I# _).
matchPat hooks (PLit (LInt n)) (VCon c [t])
    | isIntHashCtor c || isWordPrimCon c
      || bareConName c `elem` intSizedPrimCons = do
        inner <- force hooks t
        matchPat hooks (PLit (LInt n)) inner
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
matchPat _hooks (PLit (LChar _)) _      = pure Nothing
-- LAddrStr is only used to construct VPrimObj at eval time; it
-- never appears as a pattern in source code (Addr# literals don't
-- have pattern syntax), so a non-match catch-all is correct.
matchPat _hooks (PLit (LAddrStr _)) _   = pure Nothing
matchPat _hooks (PCon "[]" []) (VStr s)
    | BC.null s = pure (Just [])
    | otherwise = pure Nothing
matchPat hooks (PCon ":" [pHead, pTail]) (VStr s) =
    case BC.uncons s of
        Nothing -> pure Nothing
        Just (c, rest) -> do
            hT <- newWHNFThunk (VChar c)
            tT <- newWHNFThunk (VStr rest)
            matchFields hooks [(pHead, hT), (pTail, tT)] []
-- Unit constructor pattern matches VUnit (the canonical runtime unit).
matchPat hooks (PCon "()" []) VUnit = pure (Just [])
-- DataKinds: @Proxy \@"foo"@ is represented as @VCon "Proxy" [payload]@
-- but pattern @Proxy@ (nullary) should still match — the payload is
-- type-level metadata that user code doesn't observe via the ctor.
matchPat hooks (PCon "Proxy" []) (VCon "Proxy" _) = pure (Just [])
-- Coercible Bool newtypes from base, notably @Any@ and @All@, can reach
-- source-loaded Bool functions through @coerce@-based definitions before
-- IHC has full type-directed coercion.  Expose their single Bool field
-- when the demanded pattern is exactly @True@ or @False@.
matchPat hooks pat@(PCon boolCtor []) (VCon wrapper [innerT])
    | boolCtor == BC.pack "True" || boolCtor == BC.pack "False"
    , wrapper == BC.pack "Any" || wrapper == BC.pack "All" = do
        inner <- force hooks innerT
        matchPat hooks pat inner
-- Boxed prim constructors are host-backed wrappers over the interpreter's
-- primitive runtime values. Pattern matching must therefore treat
-- @I# x@ / @W# x@ / @W8# x@ as wrappers around 'VInt' and @C# x@ as a
-- wrapper around 'VChar'.  Some Char#/Int# source paths also cross this
-- representation boundary: @chr#@ yields a 'VChar', while a later source
-- @I#@ match wants the ordinal Int# payload.  Otherwise source bindings like
-- @new (I# len#) = ...@ never match and libraries such as @text@ fail at
-- first use after discovery succeeds.
matchPat hooks (PCon "I#" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "I#" [p]) (VChar c) = do
    t <- newWHNFThunk (VInt (fromIntegral (ord c)))
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "I#" [p]) (VInteger n)
    | n >= toInteger (minBound :: Int64)
    , n <= toInteger (maxBound :: Int64) = do
        t <- newWHNFThunk (VInt (fromInteger n))
        matchFields hooks [(p, t)] []
-- Cross-rep match: source-loaded code that destructures a boxed
-- scalar via @I# d@ / @W# d@ / @W8# d@ can be handed a value
-- originally constructed through the 'Integer' path — small Integers
-- live in 'VCon "IS" [VInt n]' and carry the same underlying 'Int#'
-- as 'VInt'.  This shows up after 'fromIntegral' or implicit
-- 'fromInteger' chains reach e.g. source-loaded
-- @plusForeignPtr (ForeignPtr addr c) (I# d) = …@ or
-- @poke (W8# byte#)@-shaped lambdas in @Data.ByteString.Char8@.
-- Without these cases the function falls through to "Non-exhaustive
-- patterns" even though the runtime value semantically IS an
-- Int#-shaped boxed scalar.
matchPat hooks (PCon "I#" [p]) (VCon "IS" [t]) =
    matchFields hooks [(p, t)] []
matchPat hooks pat@(PCon "I#" [_]) (VCon c [t])
    | bareConName c `elem` numericNewtypeCons = do
        inner <- force hooks t
        matchPat hooks pat inner
matchPat hooks (PCon "I#" [p]) (VCon c [t])
    | bareConName c `elem` intSizedPrimCons =
        matchFields hooks [(p, t)] []
-- Cross-rep: Word#/Word8# carriers often appear as the second operand of
-- @fromIntegral n + (48 :: Word8)@ (http-date @i2w8@, PackInt @_0 + …@)
-- while the left stays a bare 'VInt' after optimistic fromIntegral. Num
-- Int's @(I# x) + (I# y)@ must accept the W8#/W# payload so the digit
-- packing path does not PatternMatchFail with args=<int> W8# 48.
matchPat hooks (PCon "I#" [p]) (VCon c [t])
    | isWordPrimCon c =
        matchFields hooks [(p, t)] []
-- composeHeader `17 + slen + foldl' fieldLength 0 headers` leftovers as
-- I# I# args=19 <function>: the bang where-clause in signed IO arrives
-- as a State# VFun / VIO instead of the Int. Apply RealWorld and rematch.
matchPat hooks p@(PCon n _) v
    | isNumericPrimPatName n
    , isLeftoverStateFun v = do
        result <- runIOVal hooks v
        if leftoverStateFunUnchanged v result
            then pure Nothing
            else matchPat hooks p result
matchPat hooks (PCon c [p]) (VInt n)
    | bareConName c `elem` intSizedPrimCons = do
        t <- newWHNFThunk (VInt n)
        matchFields hooks [(p, t)] []
-- Word-sized prim wrappers are representation-transparent over VInt
-- the same way I# already is.  Optimistic fromIntegral / unannotated
-- literals stay VInt; source Bits/Num instances still pattern-match
-- @W32# x#@ / @I# i#@.  W# and W8# had this bridge; W16# / W32# /
-- W64# did not — so @fromIntegral x `shiftL` i :: Word32@ (the
-- Network.Socket.Types HostAddress pack helper) died as
-- W32#/I# args=127 24.
matchPat hooks pat@(PCon c [_]) (VCon inner [t])
    | isWordPrimCon c
    , bareConName inner `elem` numericNewtypeCons = do
        innerV <- force hooks t
        matchPat hooks pat innerV
matchPat hooks (PCon c [p]) (VCon inner [t])
    | isWordPrimCon c
    , isWordPrimCon inner || inner == "IS" =
        matchFields hooks [(p, t)] []
matchPat hooks (PCon c [p]) (VInt n)
    | isWordPrimCon c = do
        t <- newWHNFThunk (VInt n)
        matchFields hooks [(p, t)] []
matchPat hooks (PCon c [p]) (VInteger n)
    | isWordPrimCon c
    , wordPrimIntegerInRange c n = do
        t <- newWHNFThunk (VInt (fromInteger n))
        matchFields hooks [(p, t)] []
matchPat hooks (PCon "C#" [p]) (VChar c) = do
    t <- newWHNFThunk (VChar c)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "C#" [p]) v@(VInt _) = do
    t <- newWHNFThunk v
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
-- Optimistic numeric defaulting: unannotated integer literals stay
-- VInt, but Floating/Fractional Double methods (log, /, logBase, …)
-- pattern-match on D#/F#.  Without these bridges,
-- @logBase 10 (201 :: Double)@ evaluates @log 10@ through the Double
-- instance with a VInt argument, D# fails, and @/@ then sees
-- args=<number> <function> (unapplied method fallout).  That blocked
-- warp's packIntegral (len = ceiling $ logBase 10 n').
matchPat hooks (PCon "D#" [p]) (VInt n) = do
    t <- newWHNFThunk (VFloat (fromIntegral n))
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "F#" [p]) (VInt n) = do
    t <- newWHNFThunk (VFloat (fromIntegral n))
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "D#" [p]) (VInteger n) = do
    t <- newWHNFThunk (VFloat (fromInteger n))
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "F#" [p]) (VInteger n) = do
    t <- newWHNFThunk (VFloat (fromInteger n))
    matchFields hooks [(p, t)] []
-- ghc-bignum's @Integer@ data constructors:
--
--     data Integer = IS !Int# | IP !BigNat# | IN !BigNat#
--
-- IHC's runtime represents Integer as 'VInt' (Int64 range) or
-- 'VInteger' (arbitrary precision).  Source-loaded code that
-- pattern-matches @case n of IS k -> ...@ needs to see the
-- underlying primitive — the same transparent-constructor trick
-- as I# / F# / D# above.
--
-- @IS k@ binds k to an @Int#@-shaped 'VInt'.  Matches:
--   * 'VInt n'                          (already Int-range)
--   * 'VInteger n' iff @n@ fits in Int64 (parser-routed overflow
--                                         that fits after a sign flip)
matchPat hooks (PCon "IS" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "IS" [p]) (VInteger n)
    | n >= toInteger (minBound :: Int64)
    , n <= toInteger (maxBound :: Int64) = do
        t <- newWHNFThunk (VInt (fromInteger n))
        matchFields hooks [(p, t)] []
-- @IP bn@ / @IN bn@ bind bn to a @BigNat#@.  Since Phase 1 we
-- have a 'VPrimObj (PrimBigNat _)' runtime for BigNat#, so the
-- bound field is wrapped in that — Phase 2 BigNat# primops
-- ('bigNatAdd', 'bigNatEq#', …) can then dispatch on it directly
-- without further unwrapping.  The 'VInteger' magnitude is
-- converted to host 'Natural' for IP (positive) and IN (negated
-- to get the unsigned magnitude).  Phase 3 ('tryIntegerCollapse')
-- ensures the construct direction matches: source-level
-- @IP someBigNat@ collapses to 'VInteger' (large) or 'VInt'
-- (small), and this matchPat then materialises a 'VPrimObj' for
-- the field.
matchPat hooks (PCon "IP" [p]) (VInteger n)
    | n > toInteger (maxBound :: Int64) = do
        t <- newWHNFThunk (VPrimObj (PrimBigNat (fromInteger n)))
        matchFields hooks [(p, t)] []
matchPat hooks (PCon "IN" [p]) (VInteger n)
    | n < toInteger (minBound :: Int64) = do
        t <- newWHNFThunk (VPrimObj (PrimBigNat (fromInteger (negate n))))
        matchFields hooks [(p, t)] []
-- Phase 1: BigNat# is now backed by 'VPrimObj (PrimBigNat n)'.
-- Source-loaded code that pattern-matches @case n of IP bn -> ...@
-- against an Integer whose runtime is the new BigNat# representation
-- binds 'bn' to the underlying VPrimObj directly so Phase 2 BigNat#
-- primops can dispatch on it.  Sign-direction (IP vs IN) is encoded
-- in the source-level constructor used to build the Integer; the
-- BigNat# itself is always an unsigned magnitude.
matchPat hooks (PCon "IP" [p]) v@(VPrimObj (PrimBigNat _)) = do
    t <- newWHNFThunk v
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "IN" [p]) v@(VPrimObj (PrimBigNat _)) = do
    t <- newWHNFThunk v
    matchFields hooks [(p, t)] []
-- ghc-bignum's @Natural@ data constructors (mirror of Integer IS/IP/IN):
--
--     data Natural = NS !Word# | NB !BigNat#
--
-- Literals like @(5 :: Natural)@ and @fromInteger@ land as 'VInt' /
-- 'VInteger' (same optimistic path as 'Integer').  Source that
-- pattern-matches @NS w@ / @NB bn@ needs the transparent-constructor
-- bridge so @integerFromNatural@, @naturalToWord#@, etc. resolve.
-- Without this, @integerFromNatural (5 :: Natural)@ fails with
-- @Non-exhaustive patterns … NS … NB@ and warp's request path dies
-- when Natural/Integer conversions run after @recv@.
matchPat hooks (PCon "NS" [p]) (VInt n) = do
    t <- newWHNFThunk (VInt n)
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "NS" [p]) (VInteger n)
    | n >= 0
    , n <= toInteger (maxBound :: Word64) = do
        t <- newWHNFThunk (VInt (fromIntegral n))
        matchFields hooks [(p, t)] []
matchPat hooks (PCon "NS" [p]) (VCon "W#" [t]) =
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "NB" [p]) (VInteger n)
    | n > toInteger (maxBound :: Word64) = do
        t <- newWHNFThunk (VPrimObj (PrimBigNat (fromInteger n)))
        matchFields hooks [(p, t)] []
matchPat hooks (PCon "NB" [p]) v@(VPrimObj (PrimBigNat _)) = do
    t <- newWHNFThunk v
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
-- MVar source wrappers around the same host-backed synchronisation object.
-- Source-loaded handle/event code can pass a raw PrimMVar into
-- GHC.Internal.MVar functions, whose clauses pattern-match on MVar mvar#.
matchPat hooks (PCon "MVar" [p]) prim@(VPrimObj (PrimMVar _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "TVar" [p]) prim@(VPrimObj (PrimTVar _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
-- System.Posix.Types.Fd is a source-loaded newtype over a CInt.  Host-backed
-- socket helpers expose the live fd as a plain VInt, so make Fd transparent at
-- the pattern boundary for source code and FFI wrappers that unwrap it.
matchPat hooks (PCon "Fd" [p]) v@(VInt _) = do
    t <- newWHNFThunk v
    matchFields hooks [(p, t)] []
-- (PCon "Ptr" against VPrimObj PrimPtr is already handled by the
-- existing clause further down in this file — the source-loaded
-- @data Ptr a = Ptr Addr#@'s derived @Eq@ body
-- @Ptr a == Ptr b = isTrue# (eqAddr# a b)@ relies on it once the
-- synthesised @Eq Ptr@ instance from
-- 'IHC.Scheduler.registerDerivedEqInstances' fires.  Adding a second
-- clause here would shadow that existing handler.)
-- Data.Array.Byte lifted wrappers. The interpreter keeps both mutable and
-- frozen byte arrays as the same host-backed PrimByteArray object, so the
-- source constructors just expose that underlying primitive value.
matchPat hooks (PCon "MutableByteArray" [p]) prim@(VPrimObj (PrimByteArray _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon "ByteArray" [p]) prim@(VPrimObj (PrimByteArray _)) = do
    t <- newWHNFThunk prim
    matchFields hooks [(p, t)] []
matchPat hooks (PCon name pats) (VCon vname vthunks)
    | sameConName name (BC.pack "BS")
    , sameConName vname (BC.pack "BS")
    = if length pats == length vthunks
        then matchFields hooks (zip pats vthunks) []
        else pure Nothing
matchPat hooks pat@(PCon name _) v
    | sameConName name (BC.pack "BS") = do
        mBs <- charListToByteStringVal hooks v
        case mBs of
            Just bsV -> matchPat hooks pat bsV
            Nothing  -> pure Nothing
matchPat hooks (PCon "IO" [p]) v@(VCon name _)
    | name /= "IO" = matchPat hooks p (pureStateFn v)
matchPat hooks (PCon "ST" [p]) v@(VCon name _)
    | name /= "ST" = matchPat hooks p (pureStateFn v)
matchPat hooks (PCon "STM" [p]) v@(VCon name _)
    | name /= "STM" = matchPat hooks p (pureStateFn v)
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
-- Optimistic String → lazy ByteString (Empty / Chunk) bridge.
-- responseLBS status200 [] "Hello, Warp!" leaves the body as a VStr /
-- [Char] (user file has no OverloadedStrings).  Warp's write path then
-- foldrChunks / foldlChunks / L.length, which pattern-match Empty/Chunk
-- and used to PatternMatchFail with args="Hello, Warp!".  Same
-- discipline as the PCon "BS" char-list conversion above: convert at
-- the pattern boundary, do not shim L.length / lazyByteString.
matchPat hooks pat@(PCon name _) v
    | (name == BC.pack "Empty" || name == BC.pack "Chunk")
    , isStringyOrStrictBs v = do
        mLbs <- charListToLazyByteStringVal hooks v
        case mLbs of
            Just lbsV -> matchPat hooks pat lbsV
            Nothing   -> pure Nothing
-- thenIO leftover: `thenIO (IO m) k = case m s of (# new_s, _ #) -> …`
-- matches leftover <function> when `m s` is a State# VFun not applied
-- (memcpyFp >> pokeFp / hPutBuf / sendBuf).  Apply RealWorld and rematch
-- the unboxed tuple — do not runIOVal (that unwraps to `a`).
matchPat hooks p@(PCon n _) v
    | isUnboxedStateTupleName n
    , isLeftoverStateFun v = do
        result <- applyLeftoverStateFun hooks v
        if isLeftoverStateResult result
            then matchPat hooks p result
            else pure Nothing
-- Lazy ST represents state-thread results as boxed pairs `(a, State s)`,
-- while strict ST code pattern-matches on unboxed state tuples
-- `(# State# s, a #)`. When those representations meet at
-- strictToLazyST/lazyToStrictST boundaries, expose the boxed pair in the
-- strict state-passing order.
matchPat hooks (PCon "(#,#)" [pState, pVal]) (VCon "(,)" [valT, stateT]) =
    matchFields hooks [(pState, stateT), (pVal, valT)] []
-- Reverse direction: lazy ST's @(r, new_s) = res@ where @res@ came from
-- a strict ST action returning @(# s, r #)@.  runSTArray / warp header
-- indexing hit this (Irrefutable pattern failed for PTuple r,new_s).
matchPat hooks (PCon "(,)" [pVal, pState]) (VCon "(#,#)" [stateT, valT]) =
    matchFields hooks [(pVal, valT), (pState, stateT)] []
matchPat hooks (PCon "Nothing" []) (VCon "Just" [excT]) = do
    -- fromException is type-directed in GHC. IHC's Val-level helper
    -- returns `Just (SomeException inner)`, so a failed `Just
    -- (Concrete ...)` downcast must still be able to reach the following
    -- `Nothing` alternative in guard-style code.
    exc <- force hooks excT
    case exc of
        VCon "SomeException" _ -> pure (Just [])
        _                      -> pure Nothing
matchPat hooks pat@(PCon pname _) (VCon "SomeException" [innerT])
    | pname /= BC.pack "SomeException" = do
        inner <- force hooks innerT
        matchPat hooks pat inner
-- ErrorCall err <- ErrorCallWithLocation err _
matchPat hooks (PCon n [pMsg]) (VCon vn (msgT:_))
    | isErrorCallPat n && isErrorCallWithLocationCon vn =
        matchFields hooks [(pMsg, msgT)] []
-- ErrorCallWithLocation err loc against ErrorCall err
matchPat hooks (PCon n [pMsg, pLoc]) (VCon vn [msgT])
    | isErrorCallWithLocationCon n && isErrorCallPat vn = do
        locT <- emptyStringThunk
        matchFields hooks [(pMsg, msgT), (pLoc, locT)] []
-- String-shaped raise# payload: ErrorCall s / ErrorCallWithLocation s _
matchPat hooks (PCon n [pMsg]) v
    | isErrorCallPat n
    , isStringPayload v = do
        msgT <- stringPayloadThunk v
        matchFields hooks [(pMsg, msgT)] []
matchPat hooks (PCon n [pMsg, pLoc]) v
    | isErrorCallWithLocationCon n
    , isStringPayload v = do
        msgT <- stringPayloadThunk v
        locT <- emptyStringThunk
        matchFields hooks [(pMsg, msgT), (pLoc, locT)] []
-- Discharged / leftover CallStack.  getCallStack / callStack case
-- EmptyCallStack first; IHC never pushes frames, so a leftover
-- function or type-name shell is the empty stack.  Push / Freeze
-- values still take their own alternatives.
matchPat hooks (PCon n []) v
    | bareConName n == BC.pack "EmptyCallStack"
    , isLeftoverCallStackVal v =
        pure (Just [])
matchPat hooks (PCon name pats) v@(VCon vname vthunks)
    | sameConName name vname && (length pats == length vthunks
                                 || null pats) =
        -- null pats: desugared from Con {..} where field registry
        -- doesn't know the fields.  Match the constructor name,
        -- ignore field count.
        -- Zip sub-patterns with the constructor's field thunks. For
        -- each pair: if the sub-pattern is a 'PVar' we bind the name
        -- directly to the existing field thunk (preserving sharing
        -- and laziness -- we never force the field). For any other
        -- sub-pattern we MUST force the thunk to pattern-match its
        -- structure, then recurse.
        do
            -- State-threading primops return unboxed tuples whose first field
            -- is usually an unlifted State# token.  In IHC that token is held
            -- in a thunk so forcing it is what triggers delayed side effects
            -- such as writeWord8OffAddr#.  A source case/let that destructures
            -- (# State#, ... #) must therefore demand the first slot even when
            -- the pattern is a plain variable.
            forceUnboxedTupleStateSlot name vthunks
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

    forceUnboxedTupleStateSlot ctor (stateT : _)
        | isUnboxedTupleCtor ctor = () <$ force hooks stateT
    forceUnboxedTupleStateSlot _ _ = pure ()

    isUnboxedTupleCtor ctor =
        BC.pack "(#" `BS.isPrefixOf` ctor
        && BC.pack "#)" `BS.isSuffixOf` ctor
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

-- plusAddr# keeps a ForeignPtr-derived Addr# as PrimForeignPtr so the
-- finalizer is not dropped.  `Ptr src#` in copyBytes must accept that
-- shape the same way it accepts a bare PrimPtr.
matchPat hooks (PCon "Ptr" [pAddr]) (VPrimObj (PrimForeignPtr fp)) = do
    addrThunk <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
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
-- Data.ByteString.Builder.Internal
--   newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)
--   runBuilderWith (Builder b) = b
--
-- foldrChunks / mappend can leave a raw VFun (the BuildStep
-- transformer) when optimistic eval drops the newtype wrapper.
-- Expose that function through Builder — same newtype transparency
-- as IO/ST/STM over VFun.  responseLBS writes the body via
-- toLazyByteString (lazyByteString lbs).
matchPat hooks (PCon "Builder" [p]) fn@(VFun _) =
    matchPat hooks p fn
matchPat hooks (PCon "Builder" [p]) fn@(VFunIP _ _) =
    matchPat hooks p fn
-- Leftover State# VFun (copyBytes after coerce) inhabits IO.
-- A cons PAP (`:`) is also VFun / VFunIP; wrapping it as IO makes
-- thenIO's `m s` saturate `:` with RealWorld and fail as `<:...>`.
-- Probe: only State# results inhabit IO. Replay the already-applied
-- tuple so memcpy is not run twice.
matchPat hooks (PCon "IO" [p]) stFn@(VFun _) = do
    probed <- applyLeftoverStateFun hooks stFn
    if isLeftoverStateResult probed
        then matchPat hooks p (VFun $ \_ -> pure probed)
        else pure Nothing
matchPat hooks (PCon "IO" [p]) stFn@(VFunIP _ _) = do
    probed <- applyLeftoverStateFun hooks stFn
    if isLeftoverStateResult probed
        then matchPat hooks p (VFun $ \_ -> pure probed)
        else pure Nothing
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
-- ST bridge: VIO-valued ST computations can arise from IO-shaped
-- result-polymorphic dispatch or older source paths. When source code
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
-- RecordWildCards: @Con {..}@ matches any VCon with that constructor name.
-- All fields are bound as wildcards (no new bindings).
matchPat _hooks (PRecordWild conName) (VCon cn _)
    | cn == conName = pure (Just [])
matchPat _hooks (PRecordWild _) _ = pure Nothing
-- ViewPatterns: (f -> p) matches v when f v matches p.
-- Pattern synonyms such as `pattern Empty <- (null -> True)` expand
-- to PView and reach matchPat (desugarRecordPats only rewrites case
-- alts, not synonym bodies).  Evaluate the view via env-fallback —
-- no extra name list; any EVar/EApp view works.
matchPat hooks (PView fn p) v = do
    mViewed <- tryViewApply hooks fn v
    case mViewed of
        Just viewed -> matchPat hooks p viewed
        Nothing     -> pure Nothing

-- Resolve a view-pattern function (`null`, `not . null`) through the
-- same fallback as EVar so pattern-synonym views see source-loaded
-- ByteString/Text `null`.
tryViewApply :: IHCHooks -> Expr -> Val -> IO (Maybe Val)
tryViewApply hooks fn v = do
    mFn <- evalViewExpr hooks fn
    case mFn of
        Nothing -> pure Nothing
        Just fnVal -> do
            vt <- newWHNFThunk v
            r <- try (apply hooks fnVal vt) :: IO (Either SomeException Val)
            case r of
                Right viewed -> pure (Just viewed)
                Left _       -> pure Nothing

evalViewExpr :: IHCHooks -> Expr -> IO (Maybe Val)
evalViewExpr hooks (EVar n) = do
    owner <- currentOwner hooks HashMap.empty
    mSlot <- lookupEnvFallback hooks owner n
    case mSlot of
        Just slot -> Just <$> force hooks slot
        Nothing   -> do
            mBare <- lookupEnvFallback hooks Nothing (bareConName n)
            case mBare of
                Just slot -> Just <$> force hooks slot
                Nothing   -> pure Nothing
evalViewExpr hooks (EApp f x) = do
    mf <- evalViewExpr hooks f
    mx <- evalViewExpr hooks x
    case (mf, mx) of
        (Just fv, Just xv) -> do
            xt <- newWHNFThunk xv
            r <- try (apply hooks fv xt) :: IO (Either SomeException Val)
            case r of
                Right viewed -> pure (Just viewed)
                Left _       -> pure Nothing
        _ -> pure Nothing
evalViewExpr hooks (ETyApp e _) = evalViewExpr hooks e
evalViewExpr _     _            = pure Nothing

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

-- | Captures the do-block's monadic carrier in the sequencing
-- lambda's env so 'evalDo' can publish it on the thread-local stack
-- when the continuation actually runs (see 'pushDoCarrier').
doCarrierKey :: ByteString
doCarrierKey = BC.pack "$$doCarrier"

-- | Synthetic wrapper for a sequenced @Q@ continuation.  Looking up the
-- name @Q@ would collide with a later @data Queue e = Q …@ (see
-- @qq_th_q_not_queue@).  The function just builds @VCon "Q"@.
qWrapKey :: ByteString
qWrapKey = BC.pack "$qWrap"

isQCarrier :: Name -> Bool
isQCarrier n = n == BC.pack "Q"

-- | Inhabit Q.  Already-Q values stay Q — `$qWrap (pure (ListE es))`
-- after Applicative Q's `pure x = Q (pure x)` must not nest
-- `Q (Q (ListE …))` (`thExpToExpr` then sees constructor Q).
qWrapFun :: IHCHooks -> Val
qWrapFun hooks = VFun $ \innerT -> do
    inner <- force hooks innerT
    case inner of
        VCon n _ | isQCarrier (bareName n) -> pure inner
        _ -> do
            t <- newWHNFThunk inner
            pure (VCon (BC.pack "Q") [t])

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

-- | True when @v@ is a String / [Char] / strict 'BS' that a lazy
-- ByteString consumer (Empty/Chunk) should see as one packed chunk.
isStringyOrStrictBs :: Val -> Bool
isStringyOrStrictBs (VStr _)      = True
isStringyOrStrictBs (VCon "BS" _) = True
isStringyOrStrictBs (VCon ":" _)  = True
isStringyOrStrictBs (VCon "[]" _) = True
isStringyOrStrictBs _             = False

-- | Pack a String / [Char] / strict BS into source-shaped
-- @Empty@ / @Chunk strict Empty@ so foldrChunks / L.length match.
charListToLazyByteStringVal :: IHCHooks -> Val -> IO (Maybe Val)
charListToLazyByteStringVal _ (VCon name _)
    | name == BC.pack "Empty" || name == BC.pack "Chunk" = pure Nothing
charListToLazyByteStringVal hooks v@(VCon "BS" _) =
    Just <$> strictBsToLazyVal hooks v
charListToLazyByteStringVal hooks v = do
    mBs <- charListToByteStringVal hooks v
    case mBs of
        Nothing  -> pure Nothing
        Just bsV -> Just <$> strictBsToLazyVal hooks bsV

strictBsToLazyVal :: IHCHooks -> Val -> IO Val
strictBsToLazyVal hooks v@(VCon "BS" [_, lenT]) = do
    lenV <- force hooks lenT
    case lenV of
        VInt 0 -> pure (VCon "Empty" [])
        _ -> do
            bsT <- newWHNFThunk v
            emptyT <- newWHNFThunk (VCon "Empty" [])
            pure (VCon "Chunk" [bsT, emptyT])
strictBsToLazyVal _ v = pure v

byteStringConFromBS :: ByteString -> IO Val
byteStringConFromBS bs = do
    let len = BS.length bs
    fp <- mallocForeignPtrBytes len
    withForeignPtr fp $ \dst ->
        BS.useAsCStringLen bs $ \(src, n) ->
            copyBytes (castPtr dst) (castPtr src) n
    let rawPtr = castPtr (unsafeForeignPtrToPtr fp) :: Ptr Word8
    -- Full buffer range so plusForeignPtr peeks inside S.any still hit
    -- isHostWord8PtrVal (same as mallocForeignPtrBytesB).
    markWord8PtrRange rawPtr len
    markTypedHostPtr rawPtr (BC.pack "Word8")
    -- Match source-shaped ForeignPtr: VCon "ForeignPtr" [addr, guts],
    -- not a bare PrimForeignPtr.  Bare PrimForeignPtr as the BS field
    -- broke peekFp / S.any on OverloadedStrings ByteStrings.
    addrT <- newWHNFThunk (VPrimObj (PrimPtr rawPtr))
    gutsT <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    fpT  <- newWHNFThunk (VCon "ForeignPtr" [addrT, gutsT])
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
-- Record-selector scheme consumption
--------------------------------------------------------------------------------

-- | Fail-closed per-constructor evidence. Duplicate disagreeing schemes
-- are treated as missing so we never elaborate with the wrong residual.
agreedFieldEvidence :: Name -> [(Name, Scheme, Name)] -> Maybe (Scheme, Name)
agreedFieldEvidence ctor evidence = case matches of
    [] -> Nothing
    first : rest
        | all (== first) rest -> Just first
        | otherwise -> Nothing
  where
    matches = [(scheme, owner) | (ctor', scheme, owner) <- evidence, ctor' == ctor]

-- | Only function / quantified field types need residual-scheme
-- elaboration. Ordinary payload projections stay on the cheap force path.
schemeNeedsElaboration :: Scheme -> Bool
schemeNeedsElaboration (Scheme _ _ ty) = go ty
  where
    go TyArrow{} = True
    go TyForall{} = True
    go (TyApp f x) = go f || go x
    go _ = False

projectedFieldHeadName :: Expr -> Maybe Name
projectedFieldHeadName (EApp f _) = projectedFieldHeadName f
projectedFieldHeadName (ETyApp f _) = projectedFieldHeadName f
projectedFieldHeadName (EVar n) = Just n
projectedFieldHeadName _ = Nothing

-- | Project a record field and, when durable selector evidence exists,
-- elaborate the residual field scheme before returning the payload.
-- Canonical field thunks stay shared; elaboration only evaluates a view.
applyFieldAccessor
    :: IHCHooks
    -> [(Name, Int)]
    -> [(Name, Scheme, Name)]
    -> (Thunk -> IO Val)
    -> Thunk
    -> IO Val
applyFieldAccessor hooks clauses evidence fallback recordT = do
    recordVal <- force hooks recordT
    case recordVal of
        VCon "SomeException" [innerT] ->
            applyFieldAccessor hooks clauses evidence fallback innerT
        VCon ctor fields
            | Just idx <- lookup ctor clauses
            , idx < length fields
            , Just (scheme, fieldOwner) <- agreedFieldEvidence ctor evidence
            , schemeNeedsElaboration scheme -> do
                mElab <- tryElaborateProjectedField hooks (fields !! idx) scheme fieldOwner
                case mElab of
                    Just v -> pure v
                    Nothing -> fallback recordT
        _ -> fallback recordT

tryElaborateProjectedField
    :: IHCHooks
    -> Thunk
    -> Scheme
    -> Name
    -> IO (Maybe Val)
tryElaborateProjectedField hooks fieldThunk scheme fieldOwner = do
    state <- readIORef fieldThunk
    case state of
        TypedField canonical innerScheme innerOwner ->
            tryElaborateProjectedField hooks canonical innerScheme innerOwner
        Unevaluated (Closure closureEnv closureIpm closureExpr) -> do
            mReg <- getSharedClassReg legacyHooks
            case mReg of
                Nothing -> pure Nothing
                Just classReg -> do
                    sigs0 <- readIORef globalTypeSigsRef
                    syns <- readIORef globalTypeSynonymsRef
                    ctorTypes <- readIORef globalConstructorTypeRegistryRef
                    mHeadScheme <- case projectedFieldHeadName closureExpr of
                        Just method -> lookupTypeSigFallback hooks
                            (Just fieldOwner) method
                        Nothing -> pure Nothing
                    let sigs = case (projectedFieldHeadName closureExpr, mHeadScheme) of
                          (Just method, Just headScheme) ->
                            Map.insert (bareName method) headScheme
                                (Map.insert method headScheme sigs0)
                          _ -> sigs0
                        Scheme _ _ expected = scheme
                    result <- try (Elab.elaborateOwned classReg sigs syns
                        ctorTypes (Just fieldOwner)
                        (Elab.ExpectType expected) closureExpr)
                        :: IO (Either SomeException (Expr, TA.Type))
                    case result of
                        Right (specialized, _) ->
                            Just <$> eval hooks closureEnv closureIpm specialized
                        Left _ -> pure Nothing
        _ -> pure Nothing

--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

-- Host Eq/Ord on unboxed Int/Char and Semigroup on Ordering.
-- Source instances are a handful of primops; interpreting them is
-- ~20ms each.  compareText does min + Prelude.compare + compare of
-- lengths plus (<>) — Set.fromList of generated Text was ~1.6s per
-- insert.  Representation check only; no Text type-name list.
applyClassMethodFast
    :: IHCHooks
    -> Name
    -> [ByteString]
    -> ([ByteString] -> Thunk -> IO Val)
    -> Thunk
    -> IO Val
applyClassMethodFast hooks name tags go arg
    | not (null tags) = go tags arg
    | not (isUnboxedClassMethod name) = go tags arg
    | otherwise = do
        av0 <- force hooks arg
        av <- unwrapIntHash hooks av0
        case unboxedClassFirst hooks name av (go tags arg) of
            Just next -> next
            Nothing   -> go tags arg

unwrapIntHash :: IHCHooks -> Val -> IO Val
unwrapIntHash hooks v = case v of
    VCon n [t]
        | bareConName n `elem` numericNewtypeCons
          || isWordPrimCon n
          || bareConName n `elem` intSizedPrimCons ->
            force hooks t >>= unwrapIntHash hooks
    _ -> pure v

isUnboxedClassMethod :: Name -> Bool
isUnboxedClassMethod n =
    let bare = lastDottedMethod n
    in bare == BC.pack "compare"
    || bare == BC.pack "<"
    || bare == BC.pack "<="
    || bare == BC.pack ">"
    || bare == BC.pack ">="
    || bare == BC.pack "min"
    || bare == BC.pack "max"
    || bare == BC.pack "=="
    || bare == BC.pack "/="
    || bare == BC.pack "<>"
    || bare == BC.pack "mappend"

lastDottedMethod :: Name -> Name
lastDottedMethod n =
    case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
        Just idx -> BC.drop (idx + 1) n
        Nothing  -> n

unboxedClassFirst
    :: IHCHooks -> Name -> Val -> IO Val -> Maybe (IO Val)
unboxedClassFirst hooks name av fallback = case av of
    VInt x -> Just $ pure $ VFun $ \t2 -> do
        bv0 <- force hooks t2
        bv <- unwrapIntHash hooks bv0
        case bv of
            VInt y -> pure (intClassOp name x y)
            _      -> fallback >>= \f -> apply hooks f t2
    VChar x -> Just $ pure $ VFun $ \t2 -> do
        bv <- force hooks t2
        case bv of
            VChar y -> pure (charClassOp name x y)
            _       -> fallback >>= \f -> apply hooks f t2
    VCon n []
        | isOrderingCon n
        , let bare = lastDottedMethod name
        , bare == BC.pack "<>" || bare == BC.pack "mappend" ->
            Just $ pure $ VFun $ \t2 -> do
                bv <- force hooks t2
                case bv of
                    VCon n2 [] | isOrderingCon n2 ->
                        pure (if n == BC.pack "EQ" then bv else av)
                    _ -> fallback >>= \f -> apply hooks f t2
    _ -> Nothing

isOrderingCon :: Name -> Bool
isOrderingCon n =
    n == BC.pack "LT" || n == BC.pack "EQ" || n == BC.pack "GT"

intClassOp :: Name -> Int64 -> Int64 -> Val
intClassOp name x y = case BC.unpack (lastDottedMethod name) of
    "compare" -> orderingVal (compare x y)
    "<"       -> boolCon (x < y)
    "<="      -> boolCon (x <= y)
    ">"       -> boolCon (x > y)
    ">="      -> boolCon (x >= y)
    "min"     -> VInt (min x y)
    "max"     -> VInt (max x y)
    "=="      -> boolCon (x == y)
    "/="      -> boolCon (x /= y)
    other     -> error ("IHC.Eval.intClassOp: unexpected method " <> other)

charClassOp :: Name -> Char -> Char -> Val
charClassOp name x y = case BC.unpack (lastDottedMethod name) of
    "compare" -> orderingVal (compare x y)
    "<"       -> boolCon (x < y)
    "<="      -> boolCon (x <= y)
    ">"       -> boolCon (x > y)
    ">="      -> boolCon (x >= y)
    "min"     -> VChar (min x y)
    "max"     -> VChar (max x y)
    "=="      -> boolCon (x == y)
    "/="      -> boolCon (x /= y)
    other     -> error ("IHC.Eval.charClassOp: unexpected method " <> other)

boolCon :: Bool -> Val
boolCon True  = VCon (BC.pack "True") []
boolCon False = VCon (BC.pack "False") []

orderingVal :: Ordering -> Val
orderingVal LT = VCon (BC.pack "LT") []
orderingVal EQ = VCon (BC.pack "EQ") []
orderingVal GT = VCon (BC.pack "GT") []


--------------------------------------------------------------------------------
-- apply
--------------------------------------------------------------------------------

apply :: IHCHooks -> Val -> Thunk -> IO Val
apply _     (VFun f)                    arg = f arg
apply hooks (VFieldAccessor _ clauses evidence f) arg =
    applyFieldAccessor hooks clauses evidence f arg
apply _     (VFunIP _ f)                arg = f Map.empty arg
apply hooks (VClassMethod name _ tags go) arg =
    applyClassMethodFast hooks name tags go arg
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
-- leftover (# s, a #) applied as a function: extract a and apply
-- when a is itself applicable.  Inverse of leftover State# VFun
-- rematch (thenIO matches leftover VFun as (#,#)).
apply hooks v@(VCon n [_, _])           arg
    | isUnboxedStateTupleName n = do
        mA <- peelLeftoverStateTuple hooks v
        case mA of
            Just a -> apply hooks a arg
            Nothing -> error ("IHC.Eval.apply: not a function: "
                              <> showValForDebug v)
-- VIO applied to a state token: the source-loaded IO bind extracts
-- the state function from IO via pattern matching, but sometimes the
-- unwrapped value is still VIO (not a VFun state function).  Run the
-- IO action and return (# state, result #) as the state function would.
apply hooks (VIO io) arg = do
    result <- io
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    resT <- newWHNFThunk result
    pure (VCon "(#,#)" [stT, resT])
apply _     v                           _   = error ("IHC.Eval.apply: not a function: "
                                   <> showValForDebug v)

-- | Apply with the caller's ImplicitParamMap — used by EApp so that
-- implicit params flow from the call site into the callee.
applyIP :: IHCHooks -> ImplicitParamMap -> Val -> Thunk -> IO Val
applyIP _     _         (VFun f)                   arg = f arg
applyIP hooks _         (VFieldAccessor _ clauses evidence f) arg =
    applyFieldAccessor hooks clauses evidence f arg
applyIP _     callerIPM (VFunIP _ f)               arg = f callerIPM arg
applyIP hooks _         (VClassMethod name _ tags go) arg =
    applyClassMethodFast hooks name tags go arg
applyIP _     _         (VCon n [])                arg
    | isStateTokenNewtypeCtor n = pure (VCon n [arg])
-- Newtype-transparent application: see note on 'apply' above.
applyIP hooks ipm       (VCon _ [innerT])          arg = do
    inner <- force hooks innerT
    applyIP hooks ipm inner arg
-- leftover (# s, a #) applied as a function: see note on 'apply'.
applyIP hooks ipm       v@(VCon n [_, _])          arg
    | isUnboxedStateTupleName n = do
        mA <- peelLeftoverStateTuple hooks v
        case mA of
            Just a -> applyIP hooks ipm a arg
            Nothing -> do
                a <- force hooks arg
                error ("IHC.Eval.applyIP: not a function: "
                       <> showValForDebug v <> " applied to " <> showValForDebug a)
-- VIO applied to a state token: same bridge as 'apply', but on the
-- implicit-param-aware application path used by user closures and
-- quasiquote expansion.
applyIP hooks _         (VIO io)                   _arg = do
    result <- io
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    resT <- newWHNFThunk result
    pure (VCon "(#,#)" [stT, resT])
applyIP hooks _         v                          arg  = do
    a <- force hooks arg
    error ("IHC.Eval.applyIP: not a function: "
           <> showValForDebug v <> " applied to " <> showValForDebug a)

--------------------------------------------------------------------------------
-- Do-block desugaring
--
-- Desugars at eval time (not parse time) into a single monadic action.
-- IO-shaped blocks use 'VIO'; ST-shaped blocks preserve the 'ST'
-- carrier so source-loaded callers like @runSTArray st = runST
-- (st >>= unsafeFreezeSTArray)@ keep dispatching through the real
-- @Monad (ST s)@ instance. Each statement sees the env augmented by
-- any earlier 'SBind' / 'SLet' stmts.
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

-- Host VIO and source-constructed @IO $ \s -> …@ (VCon "IO" [stateFn]).
-- Data.ByteString.Internal.create's wrapAction is withForeignPtr, which
-- returns the latter.  evalDo must treat both as IO; otherwise the
-- statement falls through to doMonadicSequence / @>>@ and the
-- result-polymorphic fallback (ParsecT) runs the poke callback but
-- never returns the ByteString (Warp composeHeader hang).
-- First-statement VFun is IO unless the do is an explicit non-IO
-- carrier (ParsecT).  Nested library dos (`memcpyFp` /
-- `unsafeWithForeignPtr`) have no stamp (`mCarrier = Nothing`);
-- treating that leftover State# VFun as ParsecT left snoc/copy
-- buffers uninitialized.  Parser dos start as `VCon "ParsecT"`.
-- Later VFun is always IO (`ioSeq`).
recoverExceptionHandlerTag
    :: IHCHooks -> Env -> Expr -> Expr -> IO (Maybe ByteString)
recoverExceptionHandlerTag hooks env f x = do
    owner <- currentOwner hooks env
    let (headE, prevArgs) = appSpine f
    mHeadScheme <- case headE of
        EVar n -> lookupTypeSigFallback hooks owner n
        _      -> pure Nothing
    case mHeadScheme of
        -- Source catch's multiline Haddock signature may not be in
        -- the scheme table.  `catch action onIoe` is still a partial
        -- apply of a handler `T -> IO _`; stamp T.
        Nothing -> case f of
            EApp _ _ -> handlerFirstArgTag hooks owner x
            _        -> pure Nothing
        Just (Scheme _ preds body) -> do
            let classParams = classParamVars preds
                (argTys, resultTy) = tyArrowArgs body
                remaining = drop (length prevArgs) argTys
                ioResult = typeMentionsName (BC.pack "IO") resultTy
            case remaining of
                (TyArrow (TyVar e) _ : _)
                    | e `elem` classParams || ioResult ->
                        handlerFirstArgTag hooks owner x
                _ -> pure Nothing
  where
    appSpine (EApp g y) = let (h, ys) = appSpine g in (h, ys ++ [y])
    appSpine (ETyApp g _) = appSpine g
    appSpine (ELocalSig _ g) = appSpine g
    appSpine g = (g, [])

    classParamVars preds =
        [ v | Pred _ ts <- preds, TyVar v <- ts ]

    typeMentionsName n t = case t of
        TyCon c -> lastTypeComponent c == n
        TyApp a b -> typeMentionsName n a || typeMentionsName n b
        TyArrow a b -> typeMentionsName n a || typeMentionsName n b
        TyForall _ _ b -> typeMentionsName n b
        _ -> False

handlerFirstArgTag
    :: IHCHooks -> Maybe Name -> Expr -> IO (Maybe ByteString)
handlerFirstArgTag hooks owner expr = case expr of
    EVar n -> do
        mSch <- lookupTypeSigFallback hooks owner n
        pure (mSch >>= schemeFirstArgTag)
    ELocalSig raw inner ->
        case Elab.parseRawTypeExpr raw >>= schemeFromRawType of
            Just tag -> pure (Just tag)
            Nothing  -> handlerFirstArgTag hooks owner inner
    ETyApp inner ty ->
        case Elab.parseRawTypeExpr ty >>= schemeFromRawType of
            Just tag -> pure (Just tag)
            Nothing  -> handlerFirstArgTag hooks owner inner
    ELam _ _ -> pure Nothing
    _ -> pure Nothing

schemeFromRawType :: Type -> Maybe ByteString
schemeFromRawType ty =
    let (args, _) = tyArrowArgs ty
    in case args of
        (a:_) -> typeConTag a
        []    -> Nothing

schemeFirstArgTag :: Scheme -> Maybe ByteString
schemeFirstArgTag (Scheme _ _ body) = schemeFromRawType body

typeConTag :: Type -> Maybe ByteString
typeConTag t = case TA.tyApps t of
    (TyCon n, _) -> Just (lastTypeComponent n)
    _            -> Nothing

lastTypeComponent :: ByteString -> ByteString
lastTypeComponent n = case BC.elemIndexEnd '.' n of
    Just idx | idx + 1 < BC.length n -> BC.drop (idx + 1) n
    _ -> n

stampExpectedResultTag :: ByteString -> Val -> Val
stampExpectedResultTag tag (VIO action) =
    VIO (withExpectedExceptionTag tag action)
stampExpectedResultTag tag (VFun f) =
    VFun $ \t -> do
        r <- withExpectedExceptionTag tag (f t)
        pure (stampExpectedResultTag tag r)
stampExpectedResultTag tag (VFunIP ipm f) =
    VFunIP ipm $ \ipm' t -> do
        r <- withExpectedExceptionTag tag (f ipm' t)
        pure (stampExpectedResultTag tag r)
stampExpectedResultTag tag (VCon "IO" [stateFnT]) =
    VIO $ do
        stateFn <- force legacyHooks stateFnT
        stT <- newWHNFThunk (VPrimObj PrimRealWorld)
        raw <- withExpectedExceptionTag tag (apply legacyHooks stateFn stT)
        runIOVal legacyHooks raw
stampExpectedResultTag _ v = v

isIODoAction :: Bool -> Maybe Name -> Val -> Bool
isIODoAction _ _ (VIO _) = True
isIODoAction ioSeq mCarrier (VFun _) =
    ioSeq || not (isExplicitParserCarrier mCarrier)
isIODoAction ioSeq mCarrier (VFunIP _ _) =
    ioSeq || not (isExplicitParserCarrier mCarrier)
-- Leftover first stmt `const (return ())` / Warp
-- `settingsInstallShutdownHandler` can land as already-run unit or
-- an unresolved `return`/`pure` VClassMethod.  Sending that to
-- doMonadicSequence / ParsecT leftover-returns the rest of the do
-- (runSettingsSocket never reaches runSettingsConnection).  Treat
-- them as IO unless the do is an explicit parser carrier.  No
-- Settings / accept name list.
isIODoAction ioSeq mCarrier VUnit =
    ioSeq || not (isExplicitParserCarrier mCarrier)
isIODoAction ioSeq mCarrier (VClassMethod _ _ _ _) =
    ioSeq || not (isExplicitParserCarrier mCarrier)
isIODoAction ioSeq mCarrier (VCon n _)
    | n == BC.pack "IO" || BC.isSuffixOf (BC.pack ".IO") n = True
    | n == BC.pack "()" =
        ioSeq || not (isExplicitParserCarrier mCarrier)
isIODoAction _ _ _ = False

-- Finished data in an IO / unstamped / Q do (`x <- LitE`).  compile
-- of a TextNode is a quoted Exp tree; the bind must see an
-- already-computed value, not leftover monad.  Parser / ST dos keep
-- source @>>=@.  Not a LitE / ParsecT / listE name list — the do
-- carrier decides.
isComputedDoValue :: Bool -> Maybe Name -> Val -> Bool
isComputedDoValue ioSeq mCarrier v
    | isIODoAction ioSeq mCarrier v = False
    | VClassMethod{} <- v = False
    | isExplicitParserCarrier mCarrier = False
    | Just c <- mCarrier, usableStmtCarrier c = False
    | otherwise = True

-- Explicit ParsecT/parser stamp — not IO/ST/Q and not unstamped.
isExplicitParserCarrier :: Maybe Name -> Bool
isExplicitParserCarrier (Just n) =
    usableStmtCarrier n
isExplicitParserCarrier Nothing = False

carrierIsIO :: Maybe Name -> Bool
carrierIsIO (Just n) =
    n == BC.pack "IO" || BC.isSuffixOf (BC.pack ".IO") n
    || n == BC.pack "ST" || BC.isSuffixOf (BC.pack ".ST") n
carrierIsIO Nothing = False

-- | Evaluate a source do-block that *constructs* a monadic value
-- (a parser CAF, an IO action, …), then drop the leftover
-- 'lastMonadicCarrier' tag.  Class dispatch of @>>=@ / @(<|>)@ / @void@
-- (@fmap@) publishes that tag so a later result-poly @pure@ can stay
-- on the same carrier; after the do-block has produced a value the
-- tag must not leak into @runParser@'s Identity (@runIdentity@ on a
-- @ParsecT@).
evalConstructedDo
    :: IHCHooks -> Env -> ImplicitParamMap -> Maybe Name -> [Stmt] -> IO Val
evalConstructedDo hooks env ipm mCarrier stmts = do
    v <- evalDo hooks env ipm mCarrier stmts
    -- Drop a leftover ParsecT tag so it cannot leak into runParser's
    -- Identity.  Keep ST: mapM_ / @>>@ of writeArray runs *inside* the
    -- ST state function, and nested dos (writeArray's own do) must not
    -- steal the carrier.  Pushing ST as do-carrier instead regresses
    -- sequential writeArray (getBounds = return $!).
    mTaken <- takeLastMonadicCarrier
    case mTaken of
        Just t | isSTCarrierTag t -> setLastMonadicCarrier t
        _ -> pure ()
    wrapStDoResult hooks v

-- | Evaluate a do-statement application, forcing each argument to WHNF
-- before apply.  `bindPortTCP (settingsPort set) "*4"` must finish
-- Settings (port field) before entering bindPortTCP — a lazy Settings
-- argument while bind is already entered hung (infinite ShowS compose).
-- Nested calls inside the callee stay lazy.
evalDoAction :: IHCHooks -> Env -> ImplicitParamMap -> Maybe Name -> Expr -> IO Val
evalDoAction hooks env ipm mCarrier e = do
    -- peekArray / pokeElemOff live in do-stmts.  The force-arg peel
    -- below never reaches eval's dest-marked peek/poke intercept, so
    -- unannotated Ptr a used 8-byte Int (socketPair fd2=0 → ENOTTY).
    -- Only divert when the dest is actually marked / ascribed.
    marked <- destIsMarkedStorablePtr hooks env ipm e
    methodClasses <- readIORef globalMethodClassRef
    mv <- if marked || hasLazyStorageHead e
              then eval hooks env ipm e
              else go methodClasses e
    pinDoCarrierMethod hooks mCarrier mv
  where
    -- Peel applications so a do-action like @runQ [| e |]@ still
    -- applys.  The argument must see the callee domain: expected Q
    -- wraps via `$qWrap (pure _)`; unconstrained / Exp-domain quotes
    -- stay raw.  Same rule as elaborateExpectedArg on ordinary EApp.
    -- Preserve the whole spine for elaborator-produced typed methods.  The
    -- main evaluator uses later value arguments to correct a stale instance
    -- tag (notably generic `with` calling `poke ptr val`); peeling here would
    -- enter the pinned method after the first Ptr argument and hide `val`.
    go methodClasses app@EApp{}
        | hasRuntimeDirectedHead methodClasses app = eval hooks env ipm app
    go methodClasses (EApp f x) = do
        owner <- currentOwner hooks env
        x' <- elaborateExpectedArg hooks owner f x
        fv <- go methodClasses f
        xv <- eval hooks env ipm x'
        xt <- newWHNFThunk xv
        applyIP hooks ipm fv xt
    go _ expr = eval hooks env ipm expr

    hasRuntimeDirectedHead methodClasses = goHead
      where
        goHead (EApp f _) = goHead f
        goHead (ETyApp f _) = goHead f
        goHead ETypedMethod{} = True
        goHead (EVar method) = case Map.lookup (lastDottedMethod method) methodClasses of
            Just [_] -> True
            _ -> False
        goHead _ = False

    -- The do fast path normally forces application arguments to WHNF for
    -- legacy Settings/socket calls.  Mutable-cell allocation is specified to
    -- store its argument lazily: Warp relies on `newIORef (error ...)` being
    -- overwritten before the value is ever demanded.
    hasLazyStorageHead = goHead
      where
        goHead (EApp f _) = goHead f
        goHead (ETyApp f _) = goHead f
        goHead (ELocalSig _ f) = goHead f
        goHead (EVar n) = lastDottedMethod n `elem`
            map BC.pack ["newIORef", "newSTRef", "newMutVar#"]
        goHead _ = False

-- | True when this do-statement is a peek/poke family application
-- whose dest pointer is FFI-marked or ascribed @Ptr T@.
destIsMarkedStorablePtr
    :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Bool
destIsMarkedStorablePtr hooks env ipm e =
    case destPtrExpr e of
        Nothing -> pure False
        Just ptrE
            | ptrAscriptionKnown ptrE -> pure True
            | otherwise -> do
                ev <- try (eval hooks env ipm ptrE)
                    :: IO (Either SomeException Val)
                case ev of
                    Right ptrV -> ptrValHasTypedMark hooks ptrV
                    Left _     -> pure False

destPtrExpr :: Expr -> Maybe Expr
destPtrExpr e = peekPtr e <|> pokePtr e
  where
    strip (ETyApp inner _) = strip inner
    strip other            = other
    peekPtr expr = case strip expr of
        EApp fn ptrE | isPeekName fn -> Just ptrE
        EApp (EApp fn ptrE) _ | isPeekOffName fn -> Just ptrE
        _ -> Nothing
    pokePtr expr = case strip expr of
        EApp (EApp fn ptrE) _ | isPokeName fn -> Just ptrE
        EApp (EApp (EApp fn ptrE) _) _ | isPokeOffName fn -> Just ptrE
        _ -> Nothing
    isPeekName (EVar n) = lastBare n == BC.pack "peek"
    isPeekName (ETypedMethod _ m _) = lastBare m == BC.pack "peek"
    isPeekName (ETyApp inner _) = isPeekName inner
    isPeekName _ = False
    isPeekOffName (EVar n) =
        lastBare n `elem` map BC.pack ["peekElemOff", "peekByteOff"]
    isPeekOffName (ETypedMethod _ m _) =
        lastBare m `elem` map BC.pack ["peekElemOff", "peekByteOff"]
    isPeekOffName (ETyApp inner _) = isPeekOffName inner
    isPeekOffName _ = False
    isPokeName (EVar n) = lastBare n == BC.pack "poke"
    isPokeName (ETypedMethod _ m _) = lastBare m == BC.pack "poke"
    isPokeName (ETyApp inner _) = isPokeName inner
    isPokeName _ = False
    isPokeOffName (EVar n) =
        lastBare n `elem` map BC.pack ["pokeElemOff", "pokeByteOff"]
    isPokeOffName (ETypedMethod _ m _) =
        lastBare m `elem` map BC.pack ["pokeElemOff", "pokeByteOff"]
    isPokeOffName (ETyApp inner _) = isPokeOffName inner
    isPokeOffName _ = False

ptrAscriptionKnown :: Expr -> Bool
ptrAscriptionKnown (ETyApp _ ty) =
    case pointeeHead ty of
        Just _ -> True
        Nothing -> False
ptrAscriptionKnown _ = False

pointeeHead :: ByteString -> Maybe ByteString
pointeeHead ty = do
    parsed <- Elab.parseRawTypeExpr ty
    let (headTy, args) = TA.tyApps parsed
    case (headTy, args) of
        (TA.TyCon h, [arg])
            | lastBare (normalizeTyTag h) == BC.pack "Ptr" ->
                lastBare . normalizeTyTag <$> TA.tyHead arg
        _ -> Nothing

ptrValHasTypedMark :: IHCHooks -> Val -> IO Bool
ptrValHasTypedMark hooks v = do
    ev <- try (go v) :: IO (Either SomeException Bool)
    pure (case ev of Right True -> True; _ -> False)
  where
    go (VPrimObj (PrimPtr p)) = do
        m <- lookupTypedHostPtr p
        pure $ case m of
            Just ty -> not (BC.null ty)
            Nothing -> False
    go (VCon "Ptr" [t]) = force hooks t >>= go
    go _ = pure False

lastBare :: ByteString -> ByteString
lastBare n = case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
    Just idx -> BC.drop (idx + 1) n
    Nothing  -> n

-- Leftover VClassMethod in statement position (`guard False` = `empty`)
-- is resolved with the do-carrier tag.  Structural: any VClassMethod,
-- no name list of guard / empty / Alternative / ParsecT.
pinDoCarrierMethod :: IHCHooks -> Maybe Name -> Val -> IO Val
pinDoCarrierMethod _hooks mCarrier v = case v of
    VClassMethod _ _ tags go -> do
        mDo <- currentDoCarrier
        let tag = case mCarrier of
                Just c | usableStmtCarrier c -> Just c
                _ -> case mDo of
                    Just c | usableStmtCarrier c -> Just c
                    _ -> Nothing
        case tag of
            Nothing -> pure v
            Just c -> do
                dummyT <- newWHNFThunk VUnit
                let pinTags = if null tags then [c] else tags
                resolved <- go pinTags dummyT
                case resolved of
                    VClassMethod{} -> pure v
                    _ -> pure resolved
    _ -> pure v

usableStmtCarrier :: Name -> Bool
usableStmtCarrier tag =
    not (BC.null tag) && not (isQCarrier tag) && tag /= BC.pack "IO"

evalDo :: IHCHooks -> Env -> ImplicitParamMap -> Maybe Name -> [Stmt] -> IO Val
evalDo hooks env ipm mCarrier stmts = do
    envCarrier <- case lookupEnv doCarrierKey env of
        Nothing -> pure Nothing
        Just slot -> do
            tagV <- force hooks slot
            case tagV of
                VStr tag | not (BC.null tag), not (isQCarrier tag) ->
                    pure (Just tag)
                _ -> pure Nothing
    let carrier = case mCarrier of
            Just c | not (BC.null c) -> Just c
            _ -> envCarrier
        io0 = carrierIsIO carrier
        go = evalDoGo io0 hooks env ipm carrier stmts
        -- BuildStep peel → IO: leftover first-stmt VFun / source
        -- `>>=` must see lastMonadicCarrier IO (same restore as ST).
        run = case carrier of
            Just tag | shouldPublishPeeledCarrier tag ->
                withLastMonadicCarrier tag go
            _ -> go
        -- Push the ETyApp stamp when $$doCarrier is absent so the
        -- first statement (`guard False` = leftover `empty`) sees it.
        -- Skip Q (must not leak into runIdentity) and IO (already the
        -- result-poly default).
        pushTag = case envCarrier of
            Just t | usableStmtCarrier t -> Just t
            _ -> case carrier of
                Just c | usableStmtCarrier c -> Just c
                _ -> Nothing
    case pushTag of
        Just tag -> bracket_ (pushDoCarrier tag) popDoCarrier run
        Nothing  -> run

evalDoGo :: Bool -> IHCHooks -> Env -> ImplicitParamMap -> Maybe Name -> [Stmt] -> IO Val
evalDoGo _     hooks _   _   _        []              = pure (VIO (pure VUnit))
evalDoGo _     hooks env ipm mCarrier [SExpr e]       =
    -- Single-stmt EDo is the whole CAF (`p = do { pure 'M' }`).
    -- Multi-stmt do sequences via >>= and annotatePureLike on the tail;
    -- this path used to ignore mCarrier, so result-poly `pure` defaulted
    -- to IO and unParser saw `(#,#)`.
    evalDoAction hooks env ipm mCarrier (pinSingleDoStmt mCarrier e)
evalDoGo _     hooks env ipm mCarrier [SBind _ e]     =
    evalDoAction hooks env ipm mCarrier (pinSingleDoStmt mCarrier e)
evalDoGo _     hooks _   _   _        [SLet _]        = pure (VIO (pure VUnit))
evalDoGo ioSeq hooks env ipm mCarrier (SExpr e : rest) =
    do
        mv <- evalDoAction hooks env ipm mCarrier e
        case mv of
            VCon "Just" _ ->
                evalDoMaybe hooks env ipm rest
            VCon "Nothing" [] ->
                pure mv
            VCon "ST" [stateFnT] ->
                doSTSequence hooks env ipm stateFnT Nothing rest
            -- IO fast path: host VIO and source VCon "IO" (see isIODoAction).
            _ | isIODoAction ioSeq mCarrier mv -> pure $ VIO $ do
                _  <- runIOVal hooks mv
                restV <- evalDoGo True hooks env ipm mCarrier rest
                runIOVal hooks restV
            -- Other monads (ParsecT, ReaderT, …): sequence via source
            -- @>>@ so multi-statement do works for non-IO carriers.
            -- HSX/megaparsec hits this; the previous catch-all forced
            -- @runIOVal@ and produced @<(#,#)> applied to <function>@.
            _ -> doMonadicSequence hooks env ipm mv Nothing mCarrier rest
evalDoGo ioSeq hooks env ipm mCarrier (SBind name e : rest) =
    do
        mv <- evalDoAction hooks env ipm mCarrier e
        case mv of
            VCon "Just" [vT] ->
                evalDoMaybe hooks (extendEnv name vT env) ipm rest
            VCon "Nothing" [] ->
                pure mv
            VCon "ST" [stateFnT] ->
                doSTSequence hooks env ipm stateFnT (Just (name, False)) rest
            _ | isIODoAction ioSeq mCarrier mv -> pure $ VIO $ do
                v  <- runIOVal hooks mv
                vT <- newWHNFThunk v
                let env' = extendEnv name vT env
                restV <- evalDoGo True hooks env' ipm mCarrier rest
                runIOVal hooks restV
            _ | isComputedDoValue ioSeq mCarrier mv -> do
                vT <- newWHNFThunk mv
                evalDoGo ioSeq hooks (extendEnv name vT env) ipm mCarrier rest
            _ -> doMonadicSequence hooks env ipm mv (Just (name, False)) mCarrier rest
evalDoGo _     hooks env ipm mCarrier [SBangBind _ e] =
    -- Defensive: a do-block ending in a (bang-)bind is ill-formed; mirror SBind.
    evalDoAction hooks env ipm mCarrier e
evalDoGo ioSeq hooks env ipm mCarrier (SBangBind name e : rest) =
    -- Per Haskell Report §3.17.2 + GHC BangPatterns: !x <- m forces the
    -- bound result to WHNF before the rest of the do-block runs. The
    -- parser desugars do-blocks to (>>=)/(>>)/lambda chains, so this
    -- branch is only hit on the defensive EDo fallback path; we still
    -- preserve the strictness contract here for completeness.
    do
        mv <- evalDoAction hooks env ipm mCarrier e
        case mv of
            VCon "ST" [stateFnT] ->
                doSTSequence hooks env ipm stateFnT (Just (name, True)) rest
            _ | isIODoAction ioSeq mCarrier mv -> pure $ VIO $ do
                v  <- runIOVal hooks mv
                vT <- newWHNFThunk v
                _  <- force hooks vT  -- bang: force to WHNF before continuing
                let env' = extendEnv name vT env
                restV <- evalDoGo True hooks env' ipm mCarrier rest
                runIOVal hooks restV
            _ | isComputedDoValue ioSeq mCarrier mv -> do
                vT <- newWHNFThunk mv
                _  <- force hooks vT
                evalDoGo ioSeq hooks (extendEnv name vT env) ipm mCarrier rest
            _ -> doMonadicSequence hooks env ipm mv (Just (name, True)) mCarrier rest
evalDoGo ioSeq hooks env ipm mCarrier (SLet bs : rest) = do
    -- Same tying-the-knot pattern as 'ELet', but we're inside a
    -- do-block so the scope is the rest of the stmts (not a body expr).
    slots <- mapM (\_ -> newIORef (BlackHole Nothing "<do-let-placeholder>" Nothing)) bs
    let names = map fst bs
        env'  = extendEnvMany (zip names slots) env
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env' ipm rhs)))
          (zip bs slots)
    forceStrictDoLetBinds hooks env' bs
    evalDoGo ioSeq hooks env' ipm mCarrier rest
evalDoGo _     hooks _   _   _        [SImplicitLet _] = pure (VIO (pure VUnit))
evalDoGo ioSeq hooks env ipm mCarrier (SImplicitLet bs : rest) = do
    slots <- mapM (\_ -> newIORef (BlackHole Nothing "<do-implicit-let-placeholder>" Nothing)) bs
    let names = map fst bs
        ipm'  = foldr (\(n, sl) m -> extendIPMap n sl m) ipm
                      (zip names slots)
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env ipm rhs)))
          (zip bs slots)
    evalDoGo ioSeq hooks env ipm' mCarrier rest

-- | Sequence a non-IO monadic action with the rest of a do-block using
-- source-loaded @>>=@ / @>>@ (class dispatch). @mBind@:
--
--   * @Nothing@            — @m >> do { rest }@  (SExpr)
--   * @Just (n, False)@    — @m >>= \\n -> do { rest }@
--   * @Just (n, True)@     — bang-bind: force @n@ with @seq@ after bind
doMonadicSequence
    :: IHCHooks
    -> Env
    -> ImplicitParamMap
    -> Val
    -> Maybe (Name, Bool)
    -> Maybe Name
    -> [Stmt]
    -> IO Val
doMonadicSequence hooks env ipm mv mBind mCarrier rest = do
    actT <- newWHNFThunk mv
    let actName = BC.pack "$doAct"
        carrier0 = case mCarrier of
            Just c | not (BC.null c) -> c
            _ -> monadicCarrierTag mv
        -- @Parser@ / @Parsec e s@ collapse to the same instance head
        -- as @ParsecT@.  No name list of user synonyms.
        carrier = normalizeTyTag carrier0
    carrierT <- newWHNFThunk (VStr carrier)
    qWrapT <- newWHNFThunk (qWrapFun hooks)
    let env0    = extendEnv actName actT env
        envAct
            | isQCarrier carrier = extendEnv qWrapKey qWrapT env0
            | otherwise          = extendEnv doCarrierKey carrierT env0
        restE   = case rest of
            [SExpr e] -> annotatePureLike carrier e
            _         -> ETyApp (EDo (annotateCarrierResult carrier rest)) carrier
        -- @>>=@ keeps the carrier pin (bind-then-pure is GREEN).
        -- @>>@ must stay value-directed on the first action: annotating
        -- it as @ETyApp (EVar ">>") carrier@ lets ExpectType treat
        -- @ParsecT e s Identity@ as Identity and @runIdentity@ a
        -- ParsecT.  @space@ / @void $ takeWhileP@ is a VCon ParsecT,
        -- so @>>@ sees that constructor.  Same as the GREEN isolate.
        bodyE   = case mBind of
            Nothing ->
                EApp (EApp (EVar ">>") (EVar actName)) restE
            Just (n, False) ->
                EApp (EApp (ETyApp (EVar ">>=") carrier) (EVar actName))
                     (ELam n restE)
            Just (n, True) ->
                EApp (EApp (ETyApp (EVar ">>=") carrier) (EVar actName))
                     (ELam n (EApp (EApp (EVar "seq") (EVar n)) restE))
        -- Publish the carrier while @>>@ / @pure@ are applied so
        -- Identity (from @runParserT'@) is not the last writer.
        runBody
            | isQCarrier carrier || BC.null carrier =
                eval hooks envAct ipm bodyE
            | otherwise =
                bracket_ (pushDoCarrier carrier) popDoCarrier $
                    eval hooks envAct ipm bodyE
    runBody

-- | Attach the carrier type to the result-polymorphic final action in a
-- source-shaped do-block.  This is the small amount of expected-type
-- propagation needed for @do { x <- p; pure x }@ without globally changing
-- the IO-first default used by warp and ordinary IO programs.
monadicCarrierTag :: Val -> Name
monadicCarrierTag (VCon name _)
    | name /= BC.pack ":"
    , name /= BC.pack "[]"
    , name /= BC.pack "(,)"
    , name /= BC.pack "(#,#)" = name
-- Leftover State# VFun (copyBytes after coerce) is IO, not ParsecT.
-- Parser actions arrive as VCon "ParsecT".
monadicCarrierTag (VFun _) = BC.pack "IO"
monadicCarrierTag (VFunIP _ _) = BC.pack "IO"
monadicCarrierTag (VIO _) = BC.pack "IO"
monadicCarrierTag _ = BC.pack "ParsecT"

-- | Peel lambdas / local signatures to a do-block, if any.
stripToDo :: Expr -> Maybe [Stmt]
stripToDo (EDo stmts) = Just stmts
stripToDo (ELocalSig _ e) = stripToDo e
stripToDo (ETyApp e _) = stripToDo e
stripToDo _ = Nothing

-- | Head constructor of a result type used as a monadic carrier.
-- Function types (BuildStep = BufferRange -> IO (BuildSignal a)) peel
-- to the result constructor.  Expand type synonyms first so a local
-- @type Parser = Parsec Void Text@ is the same carrier as @ParsecT@
-- (no name list of user synonyms).  @normalizeTyTag@ then maps
-- @Parsec@ → @ParsecT@.
monadicCarrierFromType :: ByteString -> IO Name
monadicCarrierFromType raw = do
    syns <- readIORef globalTypeSynonymsRef
    pure $ case Elab.parseRawTypeExpr raw of
        Just ty ->
            let (_, result) = tyArrowArgs (expandTypeSynonyms syns ty)
            in case resultHead result of
                Just n -> normalizeTyTag (lastComponent n)
                Nothing -> BC.pack "ParsecT"
        Nothing -> normalizeTyTag (lastComponent raw)
  where
    resultHead (TyCon n) = Just n
    resultHead (TyApp f _) = resultHead f
    resultHead (TyForall _ _ b) = resultHead b
    resultHead (TyArrow _ b) = resultHead b
    resultHead TyVar{} = Nothing
    resultHead _ = Nothing

    lastComponent n = case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
        Just idx -> BC.drop (idx + 1) n
        Nothing  -> n

-- | Publish a peeled carrier while an ascription / do runs so @>>=@ of
-- a leftover State# VFun (BuildStep fill) sees IO, not <function>.
-- Restore the previous tag (ST must survive nested IO dos).
withLastMonadicCarrier :: Name -> IO a -> IO a
withLastMonadicCarrier tag act = do
    prev <- peekLastMonadicCarrier
    let restore = case prev of
            Just t -> setLastMonadicCarrier t
            Nothing -> takeLastMonadicCarrier >> pure ()
    bracket_ (setLastMonadicCarrier tag) restore act

-- | Publish only a peeled monad head (the same constructors
-- annotateMonadicCarrier stamps).  An @e \@Int@ ascription must not
-- pin lastMonadicCarrier to Int and steal a later @>>=@.
shouldPublishPeeledCarrier :: Name -> Bool
shouldPublishPeeledCarrier c =
    c == BC.pack "IO" || c == BC.pack "ST"
 || c == BC.pack "STM" || c == BC.pack "Q"
 || BC.isSuffixOf (BC.pack ".IO") c
 || BC.isSuffixOf (BC.pack ".ST") c
 || BC.isSuffixOf (BC.pack ".STM") c

-- | @>>@ / @>>=@ / @*>@ — sequencing whose result type is the
-- continuation's carrier.  Not @pure@/@return@: those stay elaborable.
isSequencingBindName :: Name -> Bool
isSequencingBindName n =
    n == BC.pack ">>" || n == BC.pack ">>=" || n == BC.pack "*>"
    || n == BC.pack "(>>)" || n == BC.pack "(>>=)" || n == BC.pack "(*>)"

annotateCarrierResult :: Name -> [Stmt] -> [Stmt]
annotateCarrierResult _ [] = []
annotateCarrierResult carrier [SExpr e] = [SExpr (annotatePureLike carrier e)]
annotateCarrierResult carrier (s:ss) = s : annotateCarrierResult carrier ss

isPureLikeArg :: Expr -> Bool
isPureLikeArg (EApp f _) = isPureLikeHead f
isPureLikeArg (ETyApp inner _) = isPureLikeArg inner
isPureLikeArg (ELocalSig _ inner) = isPureLikeArg inner
isPureLikeArg (EVar n) = isPureLikeName n
isPureLikeArg _ = False

isPureLikeHead :: Expr -> Bool
isPureLikeHead (EVar n) = isPureLikeName n
isPureLikeHead (ETyApp inner _) = isPureLikeHead inner
isPureLikeHead _ = False

isPureLikeName :: Name -> Bool
isPureLikeName n =
    let b = bareName n
    in b == BC.pack "pure" || b == BC.pack "return"
        || b == BC.pack "empty"

-- Stamp a singleton do-statement with the carrier computed by
-- monadicCarrierFromType.  Prefer the same `pure`/`return` pin as the
-- multi-stmt tail; otherwise keep the whole statement as ETyApp so
-- tryElaborateTyAnn can drive any result-poly method.  No name list
-- of Parser/ParsecT.
pinSingleDoStmt :: Maybe Name -> Expr -> Expr
pinSingleDoStmt (Just c) e
    | not (BC.null c) =
        let pinned = annotatePureLike c e
        in if pinned /= e
              then pinned
              else case e of
                  ETyApp _ _ -> e
                  _          -> ETyApp e c
pinSingleDoStmt _ e = e

annotatePureLike :: Name -> Expr -> Expr
annotatePureLike carrier (EApp (EVar n) x)
    | bareName n == BC.pack "pure" || bareName n == BC.pack "return" =
        if isQCarrier carrier
          then EApp (EVar qWrapKey) (EApp (EVar n) x)
          else EApp (ETyApp (EVar n) carrier) (annotatePureLike carrier x)
annotatePureLike carrier (EVar n)
    | isPureLikeName n && not (isQCarrier carrier) = ETyApp (EVar n) carrier
annotatePureLike carrier e = case e of
    -- Only propagate through result-position constructs. Rewriting arbitrary
    -- application arguments or let RHSs could retag a genuinely nested
    -- Maybe/IO @pure@ as the surrounding carrier.
    ELam n b -> ELam n (annotatePureLike carrier b)
    ELet bs b -> ELet bs (annotatePureLike carrier b)
    ECase s as -> ECase (annotatePureLike carrier s)
        [Alt p (annotatePureLike carrier rhs) | Alt p rhs <- as]
    EIf c t f -> EIf (annotatePureLike carrier c) (annotatePureLike carrier t)
                    (annotatePureLike carrier f)
    EDo ss -> EDo (annotateCarrierResult carrier ss)
    ETyApp x ty -> ETyApp (annotatePureLike carrier x) ty
    ELocalSig ty x -> ELocalSig ty (annotatePureLike carrier x)
    _ -> e

bareName :: Name -> Name
bareName n = case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
    Just i  -> BC.drop (i + 1) n
    Nothing -> n

doSTSequence
    :: IHCHooks
    -> Env
    -> ImplicitParamMap
    -> Thunk
    -> Maybe (Name, Bool)
    -> [Stmt]
    -> IO Val
doSTSequence hooks env ipm stateFnT mBind rest = do
    stFuncT <- newWHNFThunk $ VFun $ \sT -> do
        -- Publish ST while this state function *runs* so source
        -- @>>@ / mapM_ sees lastMonadicCarrier ST.  Do not push
        -- do-carrier — that makes writeArray's @return $!@ ST and
        -- dies on unapplied classMethod return.  Restore afterwards
        -- so C8.unpack / later IO dos are not wrapped as ST.
        prev <- peekLastMonadicCarrier
        let restore = case prev of
                Just t -> setLastMonadicCarrier t
                Nothing -> takeLastMonadicCarrier >> pure ()
        bracket_ (setLastMonadicCarrier (BC.pack "ST")) restore $ do
          stepResult <- runSTStateFunction hooks stateFnT sT
          case stDoResultComponents stepResult of
            Just (newST, resultT) -> do
                env' <- case mBind of
                    Nothing -> pure env
                    Just (name, isBang) -> do
                        -- BangPatterns on a do-bind force the bound result
                        -- before the remaining statements execute.
                        if isBang then force hooks resultT >> pure () else pure ()
                        pure (extendEnv name resultT env)
                restV <- evalDo hooks env' ipm Nothing rest
                runSTContinuation hooks newST restV
            Nothing ->
                pure stepResult
    pure (VCon "ST" [stFuncT])

isSTCarrierTag :: Name -> Bool
isSTCarrierTag tag =
    tag == BC.pack "ST" || BC.isSuffixOf (BC.pack ".ST") tag

-- | Wrap a State# VFun / IO-shaped action as @VCon "ST"@ when an ST
-- do is running.  Value-directed @>>@ then sees tag ST instead of
-- @<function>@ / ParsecT.  Does not wrap every constructed do.
wrapStDoResult :: IHCHooks -> Val -> IO Val
wrapStDoResult hooks v = do
    mLast <- peekLastMonadicCarrier
    case mLast of
        Just t | isSTCarrierTag t ->
            case v of
                VCon n _ | isSTCarrierTag n -> pure v
                VFun _ -> do
                    fnT <- newWHNFThunk v
                    pure (VCon (BC.pack "ST") [fnT])
                VFunIP _ _ -> do
                    fnT <- newWHNFThunk v
                    pure (VCon (BC.pack "ST") [fnT])
                VIO io -> do
                    fnT <- newWHNFThunk $ VFun $ \sT -> do
                        _ <- force hooks sT
                        r <- io
                        rT <- newWHNFThunk r
                        pure (VCon (BC.pack "(#,#)") [sT, rT])
                    pure (VCon (BC.pack "ST") [fnT])
                VCon n [fnT]
                    | n == BC.pack "IO" || BC.isSuffixOf (BC.pack ".IO") n ->
                        pure (VCon (BC.pack "ST") [fnT])
                _ -> pure v
        _ -> pure v

runSTStateFunction :: IHCHooks -> Thunk -> Thunk -> IO Val
runSTStateFunction hooks stateFnT sT = do
    stateFn <- force hooks stateFnT
    raw <- apply hooks stateFn sT
    runIOVal hooks raw

runSTContinuation :: IHCHooks -> Thunk -> Val -> IO Val
runSTContinuation hooks newST restV =
    case restV of
        VCon "ST" [nextFnT] ->
            runSTStateFunction hooks nextFnT newST
        -- Result-polymorphic methods can still default to IO in optimistic
        -- mode when a surrounding ST result type is not visible at runtime.
        -- Treat that IO-shaped value as the continuation action and rewrap
        -- its result in the current ST state.
        VIO io -> do
            result <- io
            resultT <- newWHNFThunk result
            pure (VCon "(#,#)" [newST, resultT])
        other -> do
            result <- runIOVal hooks other
            case result of
                VCon "ST" [nextFnT] ->
                    runSTStateFunction hooks nextFnT newST
                _ -> do
                    resultT <- newWHNFThunk result
                    pure (VCon "(#,#)" [newST, resultT])

stDoResultComponents :: Val -> Maybe (Thunk, Thunk)
stDoResultComponents (VCon "(#,#)" [stateT, valueT]) = Just (stateT, valueT)
stDoResultComponents (VCon "(,)" [valueT, stateT])   = Just (stateT, valueT)
stDoResultComponents _                               = Nothing

evalDoMaybe :: IHCHooks -> Env -> ImplicitParamMap -> [Stmt] -> IO Val
evalDoMaybe _     _   _   []              = do
    unitT <- newWHNFThunk VUnit
    pure (VCon "Just" [unitT])
evalDoMaybe hooks env ipm [SExpr e]       = evalMaybeFinal hooks env ipm e
evalDoMaybe hooks env ipm [SBind _ e]     = evalMaybeAction hooks env ipm e
evalDoMaybe hooks _   _   [SLet _]        = do
    unitT <- newWHNFThunk VUnit
    pure (VCon "Just" [unitT])
evalDoMaybe hooks env ipm (SExpr e : rest) = do
    mv <- evalMaybeAction hooks env ipm e
    case mv of
        VCon "Just" _  -> evalDoMaybe hooks env ipm rest
        VCon "Nothing" [] -> pure mv
        _ -> pure mv
evalDoMaybe hooks env ipm (SBind name e : rest) = do
    mv <- evalMaybeAction hooks env ipm e
    case mv of
        VCon "Just" [vT] -> evalDoMaybe hooks (extendEnv name vT env) ipm rest
        VCon "Nothing" [] -> pure mv
        _ -> pure mv
evalDoMaybe hooks env ipm [SBangBind _ e] = evalMaybeAction hooks env ipm e
evalDoMaybe hooks env ipm (SBangBind name e : rest) = do
    mv <- evalMaybeAction hooks env ipm e
    case mv of
        VCon "Just" [vT] -> do
            _ <- force hooks vT
            evalDoMaybe hooks (extendEnv name vT env) ipm rest
        VCon "Nothing" [] -> pure mv
        _ -> pure mv
evalDoMaybe hooks env ipm (SLet bs : rest) = do
    slots <- mapM (\_ -> newIORef (BlackHole Nothing "<maybe-do-let-placeholder>" Nothing)) bs
    let names = map fst bs
        env'  = extendEnvMany (zip names slots) env
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env' ipm rhs)))
          (zip bs slots)
    forceStrictDoLetBinds hooks env' bs
    evalDoMaybe hooks env' ipm rest
evalDoMaybe hooks env ipm [SImplicitLet _] = do
    unitT <- newWHNFThunk VUnit
    pure (VCon "Just" [unitT])
evalDoMaybe hooks env ipm (SImplicitLet bs : rest) = do
    slots <- mapM (\_ -> newIORef (BlackHole Nothing "<maybe-do-implicit-let-placeholder>" Nothing)) bs
    let names = map fst bs
        ipm'  = foldr (\(n, sl) m -> extendIPMap n sl m) ipm
                      (zip names slots)
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env ipm rhs)))
          (zip bs slots)
    evalDoMaybe hooks env ipm' rest

evalMaybeAction :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Val
evalMaybeAction hooks env ipm e = eval hooks env ipm e

evalMaybeFinal :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Val
evalMaybeFinal hooks env ipm e =
    case stripTyApps e of
        EApp f arg
            | isMaybePureHead f -> do
                v <- eval hooks env ipm arg
                mkJust v
        _ -> evalMaybeAction hooks env ipm e
  where
    stripTyApps (ETyApp inner _) = stripTyApps inner
    stripTyApps other           = other

    isMaybePureHead (EVar n) =
        let bare = lastNameComponent n
        in bare == BC.pack "pure" || bare == BC.pack "return"
    isMaybePureHead (ETyApp inner _) = isMaybePureHead inner
    isMaybePureHead _                = False

    lastNameComponent n =
        case BC.elemIndexEnd '.' n of
            Just idx -> BC.drop (idx + 1) n
            Nothing  -> n

    mkJust v = do
        t <- newWHNFThunk v
        pure (VCon "Just" [t])

forceStrictDoLetBinds :: IHCHooks -> Env -> [Bind] -> IO ()
forceStrictDoLetBinds hooks env binds =
    mapM_ forceOne
        [ n
        | (n, _) <- binds
        , BC.pack "$doPatStrict" `BS.isPrefixOf` n
        ]
  where
    forceOne n = case lookupEnv n env of
        Just t  -> () <$ force hooks t
        Nothing -> pure ()

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
    unwrapIOStateResult hooks result
-- @ST s a@ has the same State#-passing runtime shape as source-built IO.
-- The direct EDo evaluator runs bind statements through 'runIOVal'; without
-- this case, an ST do-bind like @ref <- newSTRef 0@ binds @ref@ to the ST
-- action wrapper rather than to the STRef result.
runIOVal hooks (VCon "ST" [ft]) = do
    fv <- force hooks ft
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    result <- apply hooks fv rwT
    case result of
        -- Strict ST returns an unboxed state tuple shaped as
        -- (# State# s, a #), but lazy ST's state function returns a
        -- boxed pair `(a, State s)`.  Direct EDo uses runIOVal for both;
        -- preserve the lazy-ST field order or binds receive `S#`
        -- instead of the computation result.
        VCon "(,)" [resT, stT] -> do
            _ <- force hooks stT
            force hooks resT
        VCon _ [stT, resT] -> do
            _ <- force hooks stT
            force hooks resT
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
        VCon _ [stT, resT] -> do
            -- Side-effecting primops (e.g. @setAddrRange#@,
            -- @writeAddr#@) are wired into the *state* slot of the IO
            -- result tuple — @(# setAddrRange# dest# size# byte# s,
            -- () #)@.  The runtime semantics is "evaluate the new
            -- state to trigger the side effect, then return the
            -- value".  Forcing only @resT@ (which is the unit value)
            -- would leave the state thunk un-evaluated and the side
            -- effect would never fire — explains why source-loaded
            -- @fillBytes@ used to silently produce zero-filled
            -- buffers in e.g. @BSC.replicate 4 'a'@.
            _ <- force hooks stT
            force hooks resT
        other               -> pure other
-- Unwrapped State#-passing function: an @IO a@ that has been
-- pattern-matched via @IO f = ...@ to extract the underlying
-- @State# RealWorld -> (# State# RealWorld, a #)@ closure.  This
-- arrives as 'VFun' or 'VFunIP' depending on whether the closure
-- carries an ImplicitParamMap context.  The shape is operationally
-- identical to @VCon "IO" [ft]@ — apply with a state token and
-- extract the result from the unboxed tuple.  Without this case,
-- 'ioBind' (which threads its first action through 'runIOVal') would
-- silently treat the wrapped IO action as a value and pass it as-is
-- to the continuation, so e.g. warp's
--    src <- mkSource (...)
--    leftoverSource src bs0
-- ends up binding 'src' to the State# function rather than the
-- 'Source' value the do-bind was supposed to extract.
runIOVal hooks (VFun fv) = do
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    result <- fv rwT
    unwrapIOStateResult hooks result
runIOVal hooks (VFunIP _ipm fv) = do
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    result <- fv Map.empty rwT
    unwrapIOStateResult hooks result
runIOVal _     v        = pure v

-- | Leftover function at a @Q Exp@ boundary.  Source @Q@ is a newtype
-- around @Quasi m => m a@; after the constructor is stripped the
-- payload is often a leftover class-method / State# function.  The
-- splice / antiquotation boundary runs that via 'runIOVal'.  Raw TH
-- Exp constructors pass through.  Not a runQ wrap (that regresses
-- @$(pure [| 42 |])@).  Not a ParsecT-as-Q special case.
isLeftoverQAction :: Val -> Bool
isLeftoverQAction VFun{} = True
isLeftoverQAction VFunIP{} = True
isLeftoverQAction VClassMethod{} = True
isLeftoverQAction _ = False

runLeftoverQAction :: IHCHooks -> Val -> IO Val
runLeftoverQAction hooks val = go val
  where
    go (VIO action) = action
    go (VCon n [actionT])
        | n == BC.pack "Q" || n == BC.pack "IO"
          || BC.isSuffixOf (BC.pack ".Q") n
          || BC.isSuffixOf (BC.pack ".IO") n = do
            inner <- force hooks actionT
            go inner
    go (VClassMethod _ _ tags method) = do
        dummy <- newWHNFThunk VUnit
        let pinned
                | any isIOOrQTag tags = tags
                | otherwise           = tags ++ [BC.pack "IO"]
        resolved <- method pinned dummy
        case resolved of
            VClassMethod{} -> do
                resolvedQ <- method (tags ++ [BC.pack "Q"]) dummy
                case resolvedQ of
                    VClassMethod{} -> pure val
                    other          -> go other
            other -> go other
    go v
        | isLeftoverQAction v = do
            r <- try (runIOVal hooks v) :: IO (Either SomeException Val)
            case r of
                Right v' | not (isLeftoverQAction v') -> go v'
                _ -> pure v
        | otherwise = pure v

    isIOOrQTag t = t == BC.pack "IO" || t == BC.pack "Q"
                 || BC.isSuffixOf (BC.pack ".IO") t
                 || BC.isSuffixOf (BC.pack ".Q") t

-- Unwrap only an unboxed State# result @(# s, a #)@.  A boxed pair is
-- the IO *value* — @createFpUptoN'@ does @(len, res) <- action fp@
-- where the action is @IO (Int, a)@.  Treating every 2-field VCon as
-- a state tuple returned the leftover list / extra and PatternMatchFail'd
-- the do-bind (C8.pack / lazyByteString / responseLBS body).
unwrapIOStateResult :: IHCHooks -> Val -> IO Val
unwrapIOStateResult hooks result = case result of
    -- Leftover whole-result VIO / IO|ST|STM newtype from a State#
    -- application (createAndTrim else).  A VIO sitting in the *result
    -- slot* of a tuple is a legitimate IO (IO a) and is not matched
    -- here (those cases are 2-field constructors below).
    VIO io -> do
        inner <- io
        unwrapIOStateResult hooks inner
    VCon n [ft]
        | isStateTokenNewtypeCtor n
          || BC.isSuffixOf (BC.pack ".IO") n
          || BC.isSuffixOf (BC.pack ".ST") n
          || BC.isSuffixOf (BC.pack ".STM") n -> do
            fv <- force hooks ft
            rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
            inner <- apply hooks fv rwT
            unwrapIOStateResult hooks inner
    VCon n [stT, progT, fromT, toT]
        | isUnboxedStateTupleName n -> do
            _ <- force hooks stT
            prog <- force hooks progT
            if isCodingProgressVal prog
                then pure (VCon "(,)" [fromT, toT])
                else pure result
    VCon n [stT, resT]
        | isUnboxedStateTupleName n -> unwrapStateRes
        | otherwise -> do
            -- Boxed pair: only unwrap if the first field is a State#
            -- token (@(# s, a #)@ sometimes lands as @(,)`).  A
            -- genuine IO *value* @(Int, a)@ (createFpUptoN') must
            -- stay a pair.
            st <- force hooks stT
            if isStateTokenVal st
                then force hooks resT
                else do
                    res <- force hooks resT
                    stT' <- newWHNFThunk st
                    resT' <- newWHNFThunk res
                    pure (VCon n [stT', resT'])
      where
        unwrapStateRes = do
            _ <- force hooks stT
            force hooks resT
    other -> pure other

isStateTokenVal :: Val -> Bool
isStateTokenVal (VPrimObj PrimRealWorld) = True
isStateTokenVal (VCon n _) =
    n == BC.pack "S#" || n == BC.pack "State#"
    || BC.isSuffixOf (BC.pack ".S#") n
    || BC.isSuffixOf (BC.pack ".State#") n
isStateTokenVal _ = False

isUnboxedStateTupleName :: Name -> Bool
isUnboxedStateTupleName n =
    BC.pack "(#" `BS.isPrefixOf` n
    && BC.pack "#)" `BS.isSuffixOf` n

-- Leftover State# function / host VIO sitting where a case or function
-- clause expected an unboxed state tuple or a boxed primitive.  thenIO
-- (`(# new_s, _ #)`) and Num Int (`I# y`) both hit this.
-- Candidate only: every VFun matches, including a cons PAP (`:`).
-- Rematch must still check 'isLeftoverStateResult' after applying
-- RealWorld so `:` is not treated as State#.
isLeftoverStateFun :: Val -> Bool
isLeftoverStateFun (VFun _)      = True
isLeftoverStateFun (VFunIP _ _)  = True
isLeftoverStateFun (VIO _)       = True
isLeftoverStateFun _             = False

-- Apply-result of a leftover *State#* function.  Unboxed `(# s, a #)`,
-- a still-wrapped IO/ST/STM newtype, or host VIO.  A cons cell
-- (`<:...>`) is the saturation of `(:) x` with RealWorld — not State#.
isLeftoverStateResult :: Val -> Bool
isLeftoverStateResult (VIO _) = True
isLeftoverStateResult (VCon n _)
    | isUnboxedStateTupleName n = True
    | isStateTokenNewtypeCtor n = True
    | BC.isSuffixOf (BC.pack ".IO") n = True
    | BC.isSuffixOf (BC.pack ".ST") n = True
    | BC.isSuffixOf (BC.pack ".STM") n = True
isLeftoverStateResult _ = False

leftoverStateFunUnchanged :: Val -> Val -> Bool
leftoverStateFunUnchanged (VFun _)     (VFun _)     = True
leftoverStateFunUnchanged (VFunIP _ _) (VFunIP _ _) = True
leftoverStateFunUnchanged (VIO _)      (VIO _)      = True
leftoverStateFunUnchanged _            _            = False

-- Run leftover VIO / State# VFun / source IO newtype sitting where a
-- Bool is required (`if ptr == nullPtr`).  Recurse while the peel
-- actually unwraps; stop on an unchanged leftover so a non-Bool IO
-- result still reports the same error.
peelIfCondition :: IHCHooks -> Val -> IO Val
peelIfCondition hooks v
    | isLeftoverIfCondition v = do
        v1 <- runIOVal hooks v
        if leftoverIfUnchanged v v1
            then pure v1
            else peelIfCondition hooks v1
    | otherwise = pure v

isLeftoverIfCondition :: Val -> Bool
isLeftoverIfCondition v =
    isLeftoverStateFun v || isStateIONewtype v

isStateIONewtype :: Val -> Bool
isStateIONewtype (VCon n [_]) = isStateTokenNewtypeCtor n
isStateIONewtype _            = False

leftoverIfUnchanged :: Val -> Val -> Bool
leftoverIfUnchanged a b =
    leftoverStateFunUnchanged a b
    || (isStateIONewtype a && isStateIONewtype b)

-- leftover (# s, a #) applied as a function: force the state slot
-- (side-effecting primops live there) and return `a` only when `a`
-- is itself applicable.  Inverse of applyLeftoverStateFun (thenIO
-- rematch leftover VFun as (#,#)).  leftover `pure []` as IO leaves
-- a list in the value slot — do not apply that list.
peelLeftoverStateTuple :: IHCHooks -> Val -> IO (Maybe Val)
peelLeftoverStateTuple hooks (VCon n [stT, resT])
    | isUnboxedStateTupleName n = do
        _ <- force hooks stT
        a <- force hooks resT
        if isApplicableLeftover a
            then pure (Just a)
            else pure Nothing
peelLeftoverStateTuple _ _ = pure Nothing

-- Values that leftover (# s, a #) apply rematch may peel to.
-- No constructor name list — shape only.
isApplicableLeftover :: Val -> Bool
isApplicableLeftover (VFun _)            = True
isApplicableLeftover (VFunIP _ _)        = True
isApplicableLeftover (VFieldAccessor{})  = True
isApplicableLeftover (VClassMethod{})    = True
isApplicableLeftover (VIO _)             = True
isApplicableLeftover (VCon n [])         = isStateTokenNewtypeCtor n
isApplicableLeftover (VCon _ [_])        = True
isApplicableLeftover (VCon n [_, _])     = isUnboxedStateTupleName n
isApplicableLeftover _                   = False

-- Apply a leftover State# VFun / run a leftover VIO, keeping the
-- `(# s, a #)` shape thenIO's case wants.  Contrast runIOVal, which
-- unwraps to `a`.
applyLeftoverStateFun :: IHCHooks -> Val -> IO Val
applyLeftoverStateFun _ (VIO io) = do
    result <- io
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    resT <- newWHNFThunk result
    pure (VCon "(#,#)" [stT, resT])
applyLeftoverStateFun hooks fn@(VFun _) = do
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    apply hooks fn rwT
applyLeftoverStateFun hooks fn@(VFunIP _ _) = do
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    apply hooks fn rwT
applyLeftoverStateFun _ v = pure v

isNumericPrimPatName :: Name -> Bool
isNumericPrimPatName n =
    let b = bareConName n
    in b == BC.pack "I#" || b == BC.pack "Int#"
        || isWordPrimCon b
        || b `elem` intSizedPrimCons

isIntHashCtor :: Name -> Bool
isIntHashCtor c =
    let b = bareConName c
    in b == BC.pack "I#" || b == BC.pack "Int#" || b == BC.pack "IS"

isStateTokenNewtypeCtor :: Name -> Bool
isStateTokenNewtypeCtor n =
    let b = bareConName n
    in b == BC.pack "IO"
    || b == BC.pack "ST"
    || b == BC.pack "STM"

isCodingProgressVal :: Val -> Bool
isCodingProgressVal (VCon n []) =
    n == BC.pack "InputUnderflow"
    || n == BC.pack "OutputUnderflow"
    || n == BC.pack "InvalidSequence"
isCodingProgressVal _ = False

unboxedTupleCtorValue :: Name -> Maybe Val
unboxedTupleCtorValue name = do
    arity <- unboxedTupleArity name
    pure (mkCtor arity [])
  where
    mkCtor 0 args = VCon name (reverse args)
    mkCtor n args = VFun $ \arg -> pure (mkCtor (n - 1) (arg : args))

unboxedTupleArity :: Name -> Maybe Int
unboxedTupleArity name
    | BC.pack "(#" `BS.isPrefixOf` name
    , BC.pack "#)" `BS.isSuffixOf` name =
        let middle = BC.drop 2 (BC.take (BC.length name - 2) name)
        in if BC.null middle
              then Just 0
              else if all (== ',') (BC.unpack middle)
                      then Just (BC.length middle + 1)
                      else Nothing
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Phase 2.12: TemplateHaskellQuotes — [| expr |] evaluation
--
-- evalQuote converts an IHC AST Expr into a TH Exp-shaped Val without
-- evaluating the expression. This mirrors the encoding used by liftVal /
-- thExpToExpr in IHC.TH so that $( [| e |] ) round-trips correctly.
-- We define this here (not in IHC.TH) to avoid a module cycle since
-- IHC.TH already imports IHC.Eval.
--------------------------------------------------------------------------------

evalQuote :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> IO Val
evalQuote hooks env _ipm (EVar n) = do
    -- A name bound to a local *value* (the `s` in `\s -> [| s |]`, or
    -- `value` in HSX's `[| Html5.preEscapedText value |]`) will not exist
    -- at the splice site.  GHC Lifts those.  Functions and unknown names
    -- stay VarE/ConE so `[| h1 |]` still quotes the identifier.
    case lookupEnv n env of
        Just t -> do
            v <- force hooks t
            mLifted <- liftQuotedVal hooks v
            case mLifted of
                Just expVal -> pure expVal
                Nothing     -> quoteVarName n
        Nothing -> quoteVarName n
evalQuote _hooks _env _ipm (ELit (LInt n)) = do
    nT   <- newWHNFThunk (VInt n)
    litT <- newWHNFThunk (VCon "IntegerL" [nT])
    pure (VCon "LitE" [litT])
evalQuote _hooks _env _ipm (ELit (LFloat d)) = do
    -- Store as IntegerL(round) — RationalL needs more infra for MVP.
    nT   <- newWHNFThunk (VInt (round d))
    litT <- newWHNFThunk (VCon "IntegerL" [nT])
    pure (VCon "LitE" [litT])
evalQuote _hooks _env _ipm (ELit (LChar c)) = do
    cT   <- newWHNFThunk (VChar c)
    litT <- newWHNFThunk (VCon "CharL" [cT])
    pure (VCon "LitE" [litT])
evalQuote _hooks _env _ipm (ELit (LStr bs)) = do
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
evalQuote hooks env ipm (EApp f x) = do
    fV <- evalQuote hooks env ipm f
    xV <- evalQuote hooks env ipm x
    fT <- newWHNFThunk fV
    xT <- newWHNFThunk xV
    pure (VCon "AppE" [fT, xT])
evalQuote hooks env ipm (ENeg e) = do
    -- negate x  →  AppE (VarE "negate") (evalQuote x)
    negT <- newWHNFThunk =<< evalQuote hooks env ipm (EVar "negate")
    xV   <- evalQuote hooks env ipm e
    xT   <- newWHNFThunk xV
    pure (VCon "AppE" [negT, xT])
evalQuote hooks env ipm (ETuple es) = do
    elemVals <- mapM (evalQuote hooks env ipm) es
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
-- Antiquotation: evaluate the hole in the lexical environment of the
-- quotation.  A splice expression conventionally has type @Q Exp@, which
-- IHC represents as one VIO wrapper around the TH Exp tree.
evalQuote hooks env ipm (ESplice hole) = do
    value <- eval hooks env ipm hole
    unwrapOneQuoteSplice value
  where
    unwrapOneQuoteSplice (VIO action) = action
    unwrapOneQuoteSplice (VCon "Q" [actionT]) = do
        inner <- force hooks actionT
        unwrapOneQuoteSplice inner
    unwrapOneQuoteSplice value
        | isLeftoverQAction value = runLeftoverQAction hooks value
        | otherwise               = pure value
-- Expression / local type annotation `e :: T` → SigE e (ConT T).
-- The type bytes are opaque metadata (same as ETyApp); we do not
-- evaluate T.  Without this, evalQuote's catch-all emits VarE
-- "<unsupported>", which splices as an unbound name.
evalQuote hooks env ipm (ETyApp e ty) = quoteSigE hooks env ipm e ty
evalQuote hooks env ipm (ELocalSig ty e) = quoteSigE hooks env ipm e ty
evalQuote hooks env ipm (EConstrainedValue e _) = evalQuote hooks env ipm e
-- Unsupported forms: emit a VarE "<unsupported>" placeholder.
evalQuote _hooks _env _ipm _ = do
    nt <- newWHNFThunk (VStr "<unsupported>")
    pure (VCon "VarE" [nt])

quoteVarName :: Name -> IO Val
quoteVarName n = do
    nt <- newWHNFThunk (VStr n)
    pure (VCon (thQuotedName n) [nt])

-- | Encode a runtime value as a TH Exp the way GHC's @Lift@ does for
-- quotation free variables.  Functions / class methods / unknown
-- constructors stay 'Nothing' so the caller quotes the name instead.
liftQuotedVal :: IHCHooks -> Val -> IO (Maybe Val)
liftQuotedVal hooks v = case v of
    VInt n      -> Just <$> quoteLitInteger (fromIntegral n)
    VInteger n  -> Just <$> quoteLitInteger n
    VChar c     -> Just <$> quoteLitChar c
    VStr bs     -> Just <$> quoteLitString (BC.unpack bs)
    VCon ":" _  -> do
        mCs <- extractQuotedChars hooks v
        case mCs of
            Just cs -> Just <$> quoteLitString cs
            Nothing -> pure Nothing
    VCon "[]" [] ->
        -- Empty list is both @""@ and @[]@.  ListE [] splices as nil,
        -- which is the empty String at the use site.
        Just <$> quoteEmptyListE
    _ -> pure Nothing

quoteLitInteger :: Integer -> IO Val
quoteLitInteger n = do
    nT   <- newWHNFThunk (VInt (fromInteger n))
    litT <- newWHNFThunk (VCon "IntegerL" [nT])
    pure (VCon "LitE" [litT])

quoteLitChar :: Char -> IO Val
quoteLitChar c = do
    cT   <- newWHNFThunk (VChar c)
    litT <- newWHNFThunk (VCon "CharL" [cT])
    pure (VCon "LitE" [litT])

quoteLitString :: String -> IO Val
quoteLitString cs = do
    charListVal <- buildQuotedCharList cs
    lT   <- newWHNFThunk charListVal
    litT <- newWHNFThunk (VCon "StringL" [lT])
    pure (VCon "LitE" [litT])
  where
    buildQuotedCharList [] = pure (VCon "[]" [])
    buildQuotedCharList (c:rest) = do
        h <- newWHNFThunk (VChar c)
        t <- newWHNFThunk =<< buildQuotedCharList rest
        pure (VCon ":" [h, t])

quoteEmptyListE :: IO Val
quoteEmptyListE = do
    nilT <- newWHNFThunk (VCon "[]" [])
    pure (VCon "ListE" [nilT])

extractQuotedChars :: IHCHooks -> Val -> IO (Maybe String)
extractQuotedChars hooks = go []
  where
    go acc (VCon "[]" []) = pure (Just (reverse acc))
    go acc (VCon ":" [hT, tT]) = do
        h <- force hooks hT
        case h of
            VChar c -> do
                t <- force hooks tT
                go (c : acc) t
            _ -> pure Nothing
    go acc (VStr bs) = pure (Just (reverse acc ++ BC.unpack bs))
    go _ _ = pure Nothing

-- | Last identifier segment of a (possibly qualified) name.  Used so
-- @Html5.h1@ quotes as VarE and @Html5.Html@ as ConE.
thQuotedName :: Name -> Name
thQuotedName n =
    let bare = case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
            Just i  -> BC.drop (i + 1) n
            Nothing -> n
    in if not (BC.null bare) && BC.head bare >= 'A' && BC.head bare <= 'Z'
           then "ConE"
           else "VarE"

-- | Encode @e :: T@ as TH @SigE e (ConT T)@.  @T@ is stored as the
-- source type bytes (ConT of a simple name, or the raw annotation).
quoteSigE :: IHCHooks -> Env -> ImplicitParamMap -> Expr -> ByteString -> IO Val
quoteSigE hooks env ipm e ty = do
    eV  <- evalQuote hooks env ipm e
    eT  <- newWHNFThunk eV
    nt  <- newWHNFThunk (VStr ty)
    tyT <- newWHNFThunk (VCon "ConT" [nt])
    pure (VCon "SigE" [eT, tyT])
