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
    , setSharedClassReg
    , unionInstanceScope
    , currentInstanceScope
      -- * Demand-driven env fallback
    , EnvFallbackHook
    , setEnvFallback
    , lookupEnvFallback
      -- * Core-instance load hook
    , setCoreInstanceLoadHook
    , triggerCoreInstanceLoad
      -- * Class-method dispatcher fallback
    , setClassMethodFallback
    , lookupClassMethodFallback
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

import IHC.ClassesTypes
import IHC.Runtime (IHCRuntime(..))
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

lookupInstanceMethod :: ClassRegistry -> ByteString -> ByteString -> ByteString -> IO (Maybe Val)
lookupInstanceMethod reg className typeTag methodName =
    lookupInstanceMethodMulti reg className [typeTag] methodName

lookupInstanceMethodMulti :: ClassRegistry -> ByteString -> [ByteString] -> ByteString -> IO (Maybe Val)
lookupInstanceMethodMulti reg className typeTags methodName = do
    mMethods <- lookupInstanceMulti reg className typeTags
    pure (mMethods >>= Map.lookup methodName)

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
-- | Return a stable string tag for the runtime type of a value.
-- Used by dispatch builtins to find the right class instance.
typeTagOf :: Val -> ByteString
typeTagOf (VInt _)    = BC.pack "Int"
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
typeTagOf (VPrimObj PrimRealWorld)       = BC.pack "<RealWorld#>"
typeTagOf (VPrimObj (PrimMVar _))        = BC.pack "<MVar>"
typeTagOf (VPrimObj (PrimTVar _))        = BC.pack "<TVar>"
typeTagOf (VPrimObj (PrimThreadId _))    = BC.pack "<ThreadId>"
typeTagOf (VLabel _)                     = BC.pack "Label"

--------------------------------------------------------------------------------
-- Per-run class state
--
-- Previously six top-level 'unsafePerformIO' CAFs:
--   * instanceScopeRef, scanHookRef, sharedClassRegRef
--   * envFallbackRef, classMethodFallbackRef, coreInstanceLoadHookRef
-- Each has been moved into 'IHC.Runtime.IHCRuntime'; the helpers here
-- are now thin wrappers that take an 'IHCRuntime' and project the
-- matching 'IORef'.  This lets two interpreter instances in the same
-- process dispatch class methods without stomping on each other.
--------------------------------------------------------------------------------

-- Getter: return the per-run instance scope ref.  Exported for the
-- REPL / scheduler which want the bare ref (not the wrapped
-- 'Set ByteString' value).  Implemented as a function taking 'rt' so
-- that this module doesn't need to import 'IHC.Runtime' — see the
-- '.hs-boot' cycle note in 'IHC.Val'.
instanceScopeRef :: IHCRuntime -> IORef (Set ByteString)
instanceScopeRef = rtInstanceScope

scanHookRef :: IHCRuntime -> IORef (Maybe ScanHook)
scanHookRef = rtScanHook

setSharedClassReg :: IHCRuntime -> ClassRegistry -> IO ()
setSharedClassReg _ _ = pure ()
-- ^ No-op: 'IHCRuntime' always has a 'ClassRegistry' as 'rtClassReg',
-- so there's nothing to install.  Kept for backward compatibility with
-- existing call sites; will be removed once those migrate to
-- 'rtClassReg' directly.

unionInstanceScope :: IHCRuntime -> Set ByteString -> IO ()
unionInstanceScope rt ms = modifyIORef' (rtInstanceScope rt) (Set.union ms)

currentInstanceScope :: IHCRuntime -> IO (Set ByteString)
currentInstanceScope rt = readIORef (rtInstanceScope rt)

--------------------------------------------------------------------------------
-- Demand-driven env fallback
--
-- When a closure's frozen 'Env' is missing a key the body references
-- (typically a fully-qualified re-export target like
-- @Data.Text.Internal.empty@ that wasn't yet discovered at env-snapshot
-- time), 'IHC.Eval.eval' consults this hook before raising "unbound
-- variable".  The hook is installed by the scheduler/REPL and knows
-- how to look up a body in the module registry and produce a Thunk
-- on-demand.
--------------------------------------------------------------------------------

setEnvFallback :: IHCRuntime -> EnvFallbackHook -> IO ()
setEnvFallback rt = writeIORef (rtEnvFallback rt)

lookupEnvFallback :: IHCRuntime -> ByteString -> IO (Maybe Thunk)
lookupEnvFallback rt name = do
    hook <- readIORef (rtEnvFallback rt)
    hook name

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

setClassMethodFallback :: IHCRuntime -> (ByteString -> ByteString -> IO (Maybe Val)) -> IO ()
setClassMethodFallback rt = writeIORef (rtClassMethodFallback rt)

lookupClassMethodFallback :: IHCRuntime -> ByteString -> ByteString -> IO (Maybe Val)
lookupClassMethodFallback rt cls method = do
    hook <- readIORef (rtClassMethodFallback rt)
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

setCoreInstanceLoadHook :: IHCRuntime -> IO () -> IO ()
setCoreInstanceLoadHook rt = writeIORef (rtCoreInstanceLoad rt)

triggerCoreInstanceLoad :: IHCRuntime -> IO ()
triggerCoreInstanceLoad rt = do
    hook <- readIORef (rtCoreInstanceLoad rt)
    hook
