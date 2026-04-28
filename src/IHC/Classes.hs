-- | Type class registry for Phase 2.3 dictionary-passing implementation.
--
-- Type classes are implemented via runtime dispatch: a global
-- 'ClassRegistry' maps (ClassName, TypeTag) -> method-name/value table.
--
-- 'typeTagOf' inspects a 'Val' and returns a stable string tag that
-- identifies its runtime type for dispatch purposes.
--
-- Phase 2.9.5: TypeRep infrastructure.
-- TypeRep is represented as VCon "TypeRep" [tyConThunk, argsThunk]
-- where tyConThunk = VCon "TyCon" [nameThunk] and argsThunk = list of
-- sub-TypeReps. This is structural, compositional, and equality is
-- just VCon equality.
module IHC.Classes
    ( ClassRegistry
    , newClassRegistry
    , registerInstance
    , registerInstanceMulti
    , lookupInstance
    , lookupInstanceMulti
    , lookupInstanceMethod
    , lookupInstanceMethodMulti
    , typeTagOf
    , normalizeTyTag
      -- * Phase 2.9.5: TypeRep helpers
    , mkTyCon
    , mkTypeRep
    , mkTypeRepApp
    , builtinTypeRep
    , typeRepEq
      -- * Lazy instance dispatch (Haskell 2010 §4.3.2)
    , InstanceScopeRef
    , ScanHook
    , instanceScopeRef
    , scanHookRef
    , sharedClassRegRef
    , setSharedClassReg
    , unionInstanceScope
    , currentInstanceScope
    , clearInstanceScope
      -- * Lazy instance catalogue (Stage 2 of lazy registration plan)
    , addCataloguedInstance
    , drainCataloguedInstancesForClass
    , resetInstanceCatalogue
    , catalogueHasClass
      -- * Demand-driven env fallback
    , EnvFallbackHook
    , envFallbackRef
    , setEnvFallback
    , lookupEnvFallback
      -- * Core-instance load hook
    , coreInstanceLoadHookRef
    , setCoreInstanceLoadHook
    , triggerCoreInstanceLoad
      -- * Class-method dispatcher fallback
    , classMethodFallbackRef
    , setClassMethodFallback
    , lookupClassMethodFallback
      -- * TH Exp -> Expr decoder hook
    , ThExpToExprHook
    , thExpToExprRef
    , setThExpToExpr
    , runThExpToExpr
      -- * B.1: superclass tracking
    , registerSuperclasses
    , lookupSuperclasses
    , clearSuperclasses
    , allSuperclasses
    , checkSuperclassCoverage
    ) where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

import IHC.AST (Expr)
import IHC.Val

--------------------------------------------------------------------------------
-- Phase 2.9.5: TypeRep construction helpers
--------------------------------------------------------------------------------

-- | Build a TyCon value: VCon "TyCon" [nameThunk].
-- The name is the type constructor name as a [Char] list.
mkTyCon :: ByteString -> IO Val
mkTyCon name = do
    nameT <- stringToThunk (BC.unpack name)
    pure (VCon "TyCon" [nameT])

-- | Build a TypeRep for a nullary type constructor (no applied args).
-- VCon "TypeRep" [tyConThunk, emptyListThunk]
mkTypeRep :: ByteString -> IO Val
mkTypeRep name = do
    tyConV <- mkTyCon name
    tyConT <- newWHNFThunk tyConV
    nilT   <- newWHNFThunk (VCon "[]" [])
    pure (VCon "TypeRep" [tyConT, nilT])

-- | Build a TypeRep for an applied type: f applied to args.
-- VCon "TypeRep" [tyConThunk, argsListThunk]
-- where args are sub-TypeReps for each applied argument.
mkTypeRepApp :: ByteString -> [Val] -> IO Val
mkTypeRepApp name argReps = do
    tyConV <- mkTyCon name
    tyConT <- newWHNFThunk tyConV
    argTs  <- mapM newWHNFThunk argReps
    -- Build a [TypeRep] list.
    listV  <- valListFromThunks argTs
    listT  <- newWHNFThunk listV
    pure (VCon "TypeRep" [tyConT, listT])

-- Build a Haskell list Val from a list of thunks.
valListFromThunks :: [Thunk] -> IO Val
valListFromThunks []     = pure (VCon "[]" [])
valListFromThunks (t:ts) = do
    rest  <- valListFromThunks ts
    restT <- newWHNFThunk rest
    pure (VCon ":" [t, restT])

-- | Build a [Char] list Val from a String.
stringToThunk :: String -> IO Thunk
stringToThunk s = do
    v <- stringToVal s
    newWHNFThunk v

stringToVal :: String -> IO Val
stringToVal []     = pure (VCon "[]" [])
stringToVal (c:cs) = do
    rest  <- stringToVal cs
    restT <- newWHNFThunk rest
    charT <- newWHNFThunk (VChar c)
    pure (VCon ":" [charT, restT])

-- | TypeRep for a built-in primitive type by name.
-- Convenience: builds a nullary TypeRep for Int, Char, Bool, etc.
builtinTypeRep :: ByteString -> IO Val
builtinTypeRep = mkTypeRep

-- | Structural equality on TypeRep values.
-- Two TypeReps are equal iff their TyCon names and all argument TypeReps
-- are equal. This mirrors Eq TypeRep in GHC's implementation.
typeRepEq :: Val -> Val -> IO Bool
typeRepEq (VCon "TypeRep" [tc1, args1]) (VCon "TypeRep" [tc2, args2]) = do
    b1 <- tyConEq tc1 tc2
    if not b1 then pure False
    else do
        v1 <- readIORef args1 >>= forceThunkState
        v2 <- readIORef args2 >>= forceThunkState
        typeRepListEq v1 v2
typeRepEq _ _ = pure False

tyConEq :: Thunk -> Thunk -> IO Bool
tyConEq t1 t2 = do
    v1 <- readIORef t1 >>= forceThunkState
    v2 <- readIORef t2 >>= forceThunkState
    case (v1, v2) of
        (VCon "TyCon" [n1], VCon "TyCon" [n2]) -> strEq n1 n2
        _ -> pure False

strEq :: Thunk -> Thunk -> IO Bool
strEq t1 t2 = do
    v1 <- readIORef t1 >>= forceThunkState
    v2 <- readIORef t2 >>= forceThunkState
    pure (valToString v1 == valToString v2)

valToString :: Val -> String
valToString (VCon "[]" [])    = []
valToString (VCon ":" [h, t]) =
    -- TypeRep name strings are always fully evaluated; safe to peek.
    case readThunkPure h of
        VChar c -> c : valToString (readThunkPure t)
        _       -> []
valToString (VStr bs) = BC.unpack bs
valToString _         = []

-- | Read a thunk's value without an IO context.
-- Safe only for WHNF-evaluated thunks (which TypeRep name strings always are).
readThunkPure :: Thunk -> Val
readThunkPure t =
    case unsafePerformIO (readIORef t) of
        Evaluated v  -> v
        _            -> VStr (BC.pack "<thunk>")

typeRepListEq :: Val -> Val -> IO Bool
typeRepListEq (VCon "[]" []) (VCon "[]" []) = pure True
typeRepListEq (VCon ":" [h1,t1]) (VCon ":" [h2,t2]) = do
    v1 <- readIORef h1 >>= forceThunkState
    v2 <- readIORef h2 >>= forceThunkState
    b  <- typeRepEq v1 v2
    if not b then pure False
    else do
        r1 <- readIORef t1 >>= forceThunkState
        r2 <- readIORef t2 >>= forceThunkState
        typeRepListEq r1 r2
typeRepListEq _ _ = pure False

forceThunkState :: ThunkState -> IO Val
forceThunkState (Evaluated v) = pure v
forceThunkState (Unevaluated _) = pure (VStr (BC.pack "<unevaluated>"))
forceThunkState (BlackHole _) = pure (VStr (BC.pack "<blackhole>"))
forceThunkState (LazyBuiltin _) = pure (VStr (BC.pack "<lazy-builtin>"))

type MethodTable = Map ByteString Val

-- | The global class registry. Maps @(ClassName, [TypeTag])@ to a
-- method-name/value table.
--
-- The tag list supports multi-parameter type classes (MPTC) where an
-- instance is identified by several types — e.g. @instance SetField
-- \"name\" User String where ...@ registers under
-- @(\"SetField\", [\"name\", \"User\", \"String\"])@. Single-parameter
-- classes use a 1-element list for their tag.
type ClassRegistry = IORef (Map (ByteString, [ByteString]) MethodTable)

newClassRegistry :: IO ClassRegistry
newClassRegistry = newIORef Map.empty

-- | Register a dict (method table) for a single-tag @(class, type-tag)@
-- pair. Overwrites any previously registered instance (last write wins).
--
-- Kept for backwards compatibility with the single-parameter class path;
-- equivalent to 'registerInstanceMulti' with a 1-element tag list.
registerInstance :: ClassRegistry -> ByteString -> ByteString -> MethodTable -> IO ()
registerInstance reg className typeTag methods =
    registerInstanceMulti reg className [typeTag] methods

-- | Register a dict (method table) for a @(class, [type-tag])@ composite
-- key. Overwrites any previously registered instance (last write wins).
registerInstanceMulti :: ClassRegistry -> ByteString -> [ByteString] -> MethodTable -> IO ()
registerInstanceMulti reg className typeTags methods =
    modifyIORef' reg (Map.insert (className, typeTags) methods)

-- | Look up the method table for a given @(class, type-tag)@ pair.
-- Single-tag convenience wrapper.
lookupInstance :: ClassRegistry -> ByteString -> ByteString -> IO (Maybe MethodTable)
lookupInstance reg className typeTag =
    lookupInstanceMulti reg className [typeTag]

-- | Look up the method table for a given @(class, [type-tag])@ composite
-- key. Used for multi-parameter class dispatch.
lookupInstanceMulti :: ClassRegistry -> ByteString -> [ByteString] -> IO (Maybe MethodTable)
lookupInstanceMulti reg className typeTags = do
    m <- readIORef reg
    pure (Map.lookup (className, typeTags) m)

-- | Look up @(class, type-tag) -> method-name -> Val@. On a miss, the
-- Stage-2 lazy-instance catalogue is drained for @class@ — closures
-- registered by 'registerInstancesFrom' run, materialising every
-- catalogued instance of that class — and the lookup retries. After
-- the drain the catalogue's per-class entry is gone so subsequent
-- misses are O(1) IORef reads.
--
-- This means /every/ call site (the dispatcher, @show@/@==@/@compare@
-- builtins, the elaborator's @resolveTypedMethod@, …) automatically
-- gets lazy-registration support without needing to call drain
-- explicitly. Callers that need a strictly-non-mutating lookup can
-- still use 'lookupInstanceMethodRaw'.
lookupInstanceMethod :: ClassRegistry -> ByteString -> ByteString -> ByteString -> IO (Maybe Val)
lookupInstanceMethod reg className typeTag methodName =
    lookupInstanceMethodMulti reg className [typeTag] methodName

-- | Multi-tag variant of 'lookupInstanceMethod'. Same drain-on-miss
-- semantics — see that function's note.
lookupInstanceMethodMulti :: ClassRegistry -> ByteString -> [ByteString] -> ByteString -> IO (Maybe Val)
lookupInstanceMethodMulti reg className typeTags methodName = do
    mMethods <- lookupInstanceMulti reg className typeTags
    case mMethods >>= Map.lookup methodName of
        Just v  -> pure (Just v)
        Nothing -> do
            drained <- drainCataloguedInstancesForClass className
            if drained
                then do
                    mMethods' <- lookupInstanceMulti reg className typeTags
                    pure (mMethods' >>= Map.lookup methodName)
                else pure Nothing

-- | Strictly-non-mutating variant of 'lookupInstanceMethod' — does NOT
-- drain the lazy-instance catalogue. Used by call sites that need to
-- distinguish "no instance is registered" from "an instance is
-- catalogued but not yet materialised" without triggering
-- materialisation.
lookupInstanceMethodRaw :: ClassRegistry -> ByteString -> ByteString -> ByteString -> IO (Maybe Val)
lookupInstanceMethodRaw reg className typeTag methodName = do
    m <- readIORef reg
    pure (Map.lookup (className, [typeTag]) m >>= Map.lookup methodName)

-- | Normalise a type-application source slice into a stable dispatch
-- tag. The parser's 'captureTypeArg' stores the raw bytes of a
-- type-application argument verbatim — this means quotes, parens, and
-- whitespace slip through. Collapse those so both registration (scanned
-- from an instance head) and dispatch (parsed from an 'ETyApp' in a
-- call site) agree on the same key.
--
-- Examples:
--
--   * @\"name\"@ (a @Symbol@ literal) → @name@
--   * @User@     (a type ctor)       → @User@
--   * @(Maybe Int)@                   → @Maybe Int@ (parens stripped)
--   * @42@       (a @Nat@ literal)   → @42@
--   * @\'x\'@    (a @Char@ literal)   → @x@
normalizeTyTag :: ByteString -> ByteString
normalizeTyTag bs0 = stripQuotes (trimSpace (stripParens bs0))
  where
    trimSpace s =
        BC.dropWhile isSpace (BC.reverse (BC.dropWhile isSpace (BC.reverse s)))
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    stripParens s
        | BC.length s >= 2
        , BC.head s == '('
        , BC.last s == ')'    = stripParens (trimSpace (BC.init (BC.tail s)))
        | otherwise           = s

    stripQuotes s
        | BC.length s >= 2
        , BC.head s == '"'
        , BC.last s == '"'    = BC.init (BC.tail s)
        | BC.length s >= 2
        , BC.head s == '\''
        , BC.last s == '\''   = BC.init (BC.tail s)
        | otherwise           = s

-- | Return a stable string tag for the runtime type of a value.
-- Used by dispatch builtins to find the right class instance.
typeTagOf :: Val -> ByteString
typeTagOf (VInt _)    = BC.pack "Int"
typeTagOf (VInteger _) = BC.pack "Integer"
typeTagOf (VFloat _)  = BC.pack "Double"
typeTagOf (VChar _)   = BC.pack "Char"
typeTagOf (VStr _)    = BC.pack "String"   -- transitional VStr
typeTagOf VUnit       = BC.pack "()"
typeTagOf (VCon "[]" _) = BC.pack "[]"
typeTagOf (VCon ":" _)  = BC.pack "[]"
typeTagOf (VCon "True"  _) = BC.pack "Bool"
typeTagOf (VCon "False" _) = BC.pack "Bool"
typeTagOf (VCon "(,)" _)   = BC.pack "(,)"
typeTagOf (VCon "(,,)" _)  = BC.pack "(,,)"
typeTagOf (VCon n _)    = n
typeTagOf (VFun _)      = BC.pack "<function>"
typeTagOf (VFunIP _ _)  = BC.pack "<function>"
typeTagOf (VClassMethod _ _ _ _) = BC.pack "<function>"
typeTagOf (VLazyMethod _) = BC.pack "<function>"
typeTagOf (VIO _)       = BC.pack "<IO>"
typeTagOf (VPrimObj (PrimIORef _))       = BC.pack "<IORef>"
typeTagOf (VPrimObj (PrimHandle _))      = BC.pack "<Handle>"
typeTagOf (VPrimObj (PrimForeignPtr _))  = BC.pack "<ForeignPtr>"
typeTagOf (VPrimObj (PrimPtr _))         = BC.pack "<Ptr>"
typeTagOf (VPrimObj (PrimByteArray _))   = BC.pack "<MutableByteArray>"
typeTagOf (VPrimObj (PrimArray _))       = BC.pack "<MutableArray#>"
typeTagOf (VPrimObj (PrimBoxedArray _ _)) = BC.pack "<BoxedArray#>"
typeTagOf (VPrimObj PrimRealWorld)       = BC.pack "<RealWorld#>"
typeTagOf (VPrimObj (PrimMVar _))        = BC.pack "<MVar>"
typeTagOf (VPrimObj (PrimTVar _))        = BC.pack "<TVar>"
typeTagOf (VPrimObj (PrimThreadId _))    = BC.pack "<ThreadId>"
typeTagOf (VLabel _)                     = BC.pack "Label"

--------------------------------------------------------------------------------
-- Lazy instance dispatch (Haskell 2010 §4.3.2)
--------------------------------------------------------------------------------

type InstanceScopeRef = IORef (Set ByteString)
type ScanHook = ByteString -> IO ()

{-# NOINLINE instanceScopeRef #-}
instanceScopeRef :: InstanceScopeRef
instanceScopeRef = unsafePerformIO (newIORef Set.empty)

{-# NOINLINE scanHookRef #-}
scanHookRef :: IORef (Maybe ScanHook)
scanHookRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE sharedClassRegRef #-}
sharedClassRegRef :: IORef (Maybe ClassRegistry)
sharedClassRegRef = unsafePerformIO (newIORef Nothing)

setSharedClassReg :: ClassRegistry -> IO ()
setSharedClassReg reg = writeIORef sharedClassRegRef (Just reg)

unionInstanceScope :: Set ByteString -> IO ()
unionInstanceScope ms = modifyIORef' instanceScopeRef (Set.union ms)

currentInstanceScope :: IO (Set ByteString)
currentInstanceScope = readIORef instanceScopeRef

-- | Reset the instance-scope set.  Called by the scheduler at the start
-- of every @loadProgramFromSource@ run so stale module names from a
-- prior load don't make a re-load see the wrong subset of modules as
-- "in scope" for instance dispatch.
clearInstanceScope :: IO ()
clearInstanceScope = writeIORef instanceScopeRef Set.empty

--------------------------------------------------------------------------------
-- Lazy instance catalogue (Stage 2 of the lazy-registration plan)
--
-- The eager 'registerInstancesFrom' walked every loaded module's
-- instance-method bodies and called 'evalMethodWithLazy' on each — even
-- for instances of classes that no user code dispatches into. On a
-- 155-module load that's ~0.45 s of wasted work.
--
-- Stage 2 replaces that with a /catalogue/: 'registerInstancesFrom'
-- becomes a cheap "for each instance found by 'scanInstanceDecls', stash
-- a closure under the class name." When the dispatcher misses a
-- @(class, tag)@ lookup, 'lazyInstanceRetry' drains every catalogued
-- closure for /that one class/ — running the existing 'registerOne'
-- body verbatim — and retries. Classes that never get dispatched into
-- pay zero method-body parse / FV / eval work.
--
-- Ordering: closures are appended to a per-class list and drained in
-- append order; with 'registerInstance''s last-write-wins semantics,
-- this matches what eager registration would produce as long as the
-- caller catalogues modules in the same iteration order.
--
-- Reset boundary: 'resetInstanceCatalogue' is called once per
-- 'loadProgramFromSource' run (alongside 'globalLoadedModulesRef'
-- reset), so per-run state doesn't leak across runFile boundaries.
--------------------------------------------------------------------------------

-- | The catalogue. Keys are class names; values are append-ordered
-- lists of "register me on demand" closures, each performing the work
-- the eager 'registerOne' would have done up front.
type InstanceCatalogue = Map ByteString [IO ()]

{-# NOINLINE instanceCatalogueRef #-}
instanceCatalogueRef :: IORef InstanceCatalogue
instanceCatalogueRef = unsafePerformIO (newIORef Map.empty)

-- | Stash a "register one instance of @cls@" closure into the catalogue.
-- Cheap: just an 'IORef' update. The expensive work (parsing instance
-- method bodies, free-var discovery, 'evalMethodWithLazy') is deferred
-- to the closure itself and only runs if 'drainCataloguedInstancesForClass'
-- is later called for @cls@.
addCataloguedInstance :: ByteString -> IO () -> IO ()
addCataloguedInstance cls action =
    -- Map.insertWith f new old:  f is called as f new old, so to append
    -- the new singleton AT THE END we want @old ++ new@.
    modifyIORef' instanceCatalogueRef
        (Map.insertWith (\new old -> old ++ new) cls [action])

-- | Drain every catalogued closure for @cls@, running them in append
-- order. Removes the entries from the catalogue so subsequent drains
-- for the same class are O(1) no-ops. Returns 'True' iff at least one
-- closure was drained (i.e. the caller should retry its lookup).
--
-- Individual closures that throw are swallowed: the dispatcher's
-- existing 'methodPlaceholder' machinery already handles per-method
-- evaluation failure, and a single bad instance must not poison the
-- whole class.
drainCataloguedInstancesForClass :: ByteString -> IO Bool
drainCataloguedInstancesForClass cls = do
    cat <- readIORef instanceCatalogueRef
    case Map.lookup cls cat of
        Nothing      -> pure False
        Just []      -> pure False
        Just actions -> do
            modifyIORef' instanceCatalogueRef (Map.delete cls)
            mapM_ runOne actions
            pure True
  where
    runOne action = action `catch` \(_ :: SomeException) -> pure ()

-- | Discard every catalogued closure. Called once per
-- 'loadProgramFromSource' run, alongside the 'globalLoadedModulesRef'
-- reset, so per-run state doesn't leak across consecutive runFile
-- calls.
resetInstanceCatalogue :: IO ()
resetInstanceCatalogue = writeIORef instanceCatalogueRef Map.empty

-- | True iff the catalogue currently holds any closures for @cls@.
-- Used by callers that need to materialise a class's instances eagerly
-- (e.g. derived-Functor synthesis must see explicit 'Functor T'
-- instances before deciding whether to skip a 'deriving' clause).
catalogueHasClass :: ByteString -> IO Bool
catalogueHasClass cls = do
    cat <- readIORef instanceCatalogueRef
    case Map.lookup cls cat of
        Just (_:_) -> pure True
        _          -> pure False

--------------------------------------------------------------------------------
-- Demand-driven env fallback
--
-- When a closure's frozen 'Env' is missing a key the body references
-- (typically a fully-qualified re-export target like
-- @Data.Text.Internal.empty@ that wasn't yet discovered at env-snapshot
-- time), 'IHC.Eval.eval' consults this hook before raising "unbound
-- variable".  The hook is installed by the scheduler/REPL and knows
-- how to look up a body in the module registry and produce a Thunk
-- on-demand.  Matches the lazy-scan hook pattern above (Phase-3
-- precedent, see 'scanHookRef').
--------------------------------------------------------------------------------

-- | The first parameter is the owning module of the closure whose body
-- is currently being evaluated, when known.  It scopes the unqualified-
-- name fallback to that module's actual import declarations (per
-- Haskell 2010 §5.5).  'Nothing' means "no owner context" — typically
-- transient lookups before the owner sentinel is installed (see
-- 'currentOwner' in 'IHC.Eval').
type EnvFallbackHook = Maybe ByteString -> ByteString -> IO (Maybe Thunk)

{-# NOINLINE envFallbackRef #-}
envFallbackRef :: IORef EnvFallbackHook
envFallbackRef = unsafePerformIO (newIORef (\_ _ -> pure Nothing))

setEnvFallback :: EnvFallbackHook -> IO ()
setEnvFallback = writeIORef envFallbackRef

lookupEnvFallback :: Maybe ByteString -> ByteString -> IO (Maybe Thunk)
lookupEnvFallback owner name = do
    hook <- readIORef envFallbackRef
    hook owner name

--------------------------------------------------------------------------------
-- Class-method dispatcher fallback
--
-- When the elaborator's 'IHC.Eval.resolveTypedMethod' can't find an
-- instance for its resolved tag — typically because the instance lives
-- in a module the REPL never demand-loaded (e.g. @instance Monad (ST s)@
-- lives in @GHC.Internal.ST@, which the core-instance-load hook doesn't
-- pull in) — we fall back to the existing value-directed dispatcher.
-- This hook returns a 'VClassMethod' that performs runtime tag-based
-- lookup against the shared registry, mirroring the behaviour classes
-- had before the elaborator existed.
--------------------------------------------------------------------------------

{-# NOINLINE classMethodFallbackRef #-}
classMethodFallbackRef :: IORef (ByteString -> ByteString -> IO (Maybe Val))
classMethodFallbackRef = unsafePerformIO (newIORef (\_ _ -> pure Nothing))

setClassMethodFallback :: (ByteString -> ByteString -> IO (Maybe Val)) -> IO ()
setClassMethodFallback = writeIORef classMethodFallbackRef

lookupClassMethodFallback :: ByteString -> ByteString -> IO (Maybe Val)
lookupClassMethodFallback cls method = do
    hook <- readIORef classMethodFallbackRef
    hook cls method

--------------------------------------------------------------------------------
-- Core-instance load hook
--
-- One-shot trigger for force-loading GHC.Internal.Base and friends so
-- their Applicative/Monad/Functor instance dicts are in the registry.
-- Bare REPL startup skips this (keeps prompt latency low); the elaborator's
-- 'resolveTypedMethod' fires it only when a type-annotation-driven
-- lookup misses (e.g. @pure 42 :: Maybe Int@ before any explicit import).
--
-- Installed by 'buildBaseEnv'; invoked by 'IHC.Eval.resolveTypedMethod'.
-- The hook itself maintains its own "already-loaded" flag so subsequent
-- calls are free (no re-scan).
--------------------------------------------------------------------------------

{-# NOINLINE coreInstanceLoadHookRef #-}
coreInstanceLoadHookRef :: IORef (IO ())
coreInstanceLoadHookRef = unsafePerformIO (newIORef (pure ()))

setCoreInstanceLoadHook :: IO () -> IO ()
setCoreInstanceLoadHook = writeIORef coreInstanceLoadHookRef

triggerCoreInstanceLoad :: IO ()
triggerCoreInstanceLoad = do
    hook <- readIORef coreInstanceLoadHookRef
    hook

--------------------------------------------------------------------------------
-- TH Exp -> Expr decoder hook
--
-- QuasiQuoter dispatch ('EQuasiQuote' in the AST) has to decode the TH
-- 'Exp' value returned by @quoteExp@ back into an 'Expr'.  That decoder
-- lives in 'IHC.TH' (which already imports 'IHC.Eval'), so we break the
-- cycle via this hook: TH installs it at module load time, 'IHC.Eval'
-- reads through it.
--------------------------------------------------------------------------------

type ThExpToExprHook = Val -> IO Expr

{-# NOINLINE thExpToExprRef #-}
thExpToExprRef :: IORef ThExpToExprHook
thExpToExprRef = unsafePerformIO (newIORef (\_ ->
    error "IHC.Classes: thExpToExpr hook not installed"))

setThExpToExpr :: ThExpToExprHook -> IO ()
setThExpToExpr = writeIORef thExpToExprRef

runThExpToExpr :: Val -> IO Expr
runThExpToExpr v = do
    hook <- readIORef thExpToExprRef
    hook v

--------------------------------------------------------------------------------
-- B.1: superclass tracking (Haskell Report §4.3.1)
--
-- @class C1 a, C2 a => D a where …@ declares C1 and C2 as superclasses
-- of D: every @instance D T@ implies the existence of @instance C1 T@
-- and @instance C2 T@.  We track that relation in a global IORef so
-- the scheduler can verify coherence after instance loading and the
-- dispatcher can (in B.2) reach class default-method bodies that
-- reference superclass methods.
--
-- The map is keyed by class name only — single-param classes vs.
-- multi-param classes share the same channel because dispatch in ihc
-- already lives entirely in tag space.  We record only the superclass
-- class names; tracking which type variable each superclass is applied
-- to would require a real type AST and is deferred.
--------------------------------------------------------------------------------

{-# NOINLINE superclassesRef #-}
superclassesRef :: IORef (Map ByteString [ByteString])
superclassesRef = unsafePerformIO (newIORef Map.empty)

-- | Register a class's superclass list.  Idempotent: subsequent
-- registrations for the same class name overwrite (later modules win,
-- matching the rest of the registry).
registerSuperclasses :: ByteString -> [ByteString] -> IO ()
registerSuperclasses cls supers
    | null supers = pure ()
    | otherwise   = modifyIORef' superclassesRef (Map.insert cls supers)

-- | Direct superclasses of a class (one hop).
lookupSuperclasses :: ByteString -> IO [ByteString]
lookupSuperclasses cls =
    Map.findWithDefault [] cls <$> readIORef superclassesRef

-- | Reset the global superclass map.  Useful for tests.
clearSuperclasses :: IO ()
clearSuperclasses = writeIORef superclassesRef Map.empty

-- | Transitive closure: every superclass reachable from @cls@ via the
-- direct-superclass relation, deduplicated.  Excludes @cls@ itself.
allSuperclasses :: ByteString -> IO [ByteString]
allSuperclasses cls = do
    m <- readIORef superclassesRef
    let go !seen [] = seen
        go !seen (c : cs)
            | Set.member c seen = go seen cs
            | otherwise =
                let seen'   = Set.insert c seen
                    parents = Map.findWithDefault [] c m
                in go seen' (parents ++ cs)
        roots = Map.findWithDefault [] cls m
    pure (Set.toList (go Set.empty roots))

-- | After all instances have been registered, walk the class registry
-- and report any @(D, T)@ entry whose required superclass instance
-- @(C, T)@ is missing.  Returns a list of @(D, T, C)@ triples that
-- callers can format into diagnostics.  Side-effect free.
checkSuperclassCoverage :: ClassRegistry -> IO [(ByteString, [ByteString], ByteString)]
checkSuperclassCoverage reg = do
    rmap <- readIORef reg
    superMap <- readIORef superclassesRef
    let problems =
            [ (cls, tags, super)
            | ((cls, tags), _methods) <- Map.toList rmap
            , super <- Map.findWithDefault [] cls superMap
            , not (Map.member (super, tags) rmap)
            ]
    pure problems
