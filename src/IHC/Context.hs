-- | Explicit interpreter context.
--
-- This module owns the record types that will replace the ~24
-- module-level 'unsafePerformIO + IORef + NOINLINE' globals scattered
-- across 'IHC.Scheduler', 'IHC.Classes', 'IHC.TypeGlobals', and
-- 'IHC.TypeReduce'.  The plan is:
--
--   * 'IHCHooks' — write-once-per-session install points: fallback
--     hooks, the shared 'ClassRegistry', the per-load instance
--     registration hook, the TH-Exp decoder.  Currently mirrored by
--     the @\*Ref@ globals in 'IHC.Classes' and survive run boundaries.
--
--   * 'IHCRunState' — per-run state cleared by the moral equivalent of
--     'IHC.Scheduler.resetPerRunGlobals': the loaded-module catalogue,
--     type sigs/synonyms/method tables, the env-fallback caches, the
--     instance scope/catalogue/superclass tables, etc.  Replaced
--     atomically per 'loadProgramFromSource' so closures captured
--     against the previous run's run-state can never observe the new
--     run's state.
--
--   * 'IHCContext' — the bundle threaded through 'eval', 'force',
--     'apply', and the loader entry points.
--
-- == Lifecycle
--
-- One 'IHCContext' per IHC session: a 'runFile' invocation, a single
-- REPL session, or a test harness sequence.  'newIHCContext' builds
-- the empty shape; 'freshRunState' is called between runs in the same
-- session (REPL @:l@ between source loads, test harness between
-- fixtures) to atomically swap the per-run substate without touching
-- the hooks.  Closures captured during a run hold references to that
-- run's 'IHCRunState'; forcing them under a different run-state would
-- be a category error — the "one ctx per session, never thread two
-- contexts simultaneously" invariant rules that out.
--
-- == Status
--
-- This is PR 0 (scaffolding).  The records exist but nothing reads
-- from them yet; the legacy globals in 'IHC.Classes' / 'IHC.Scheduler'
-- still own the live state.  Subsequent PRs migrate the actual reads
-- and writes one theme at a time:
--
--   1. 'IHCHooks' — moves the seven hook IORefs.
--   2. 'TypeRunState' — moves the type-registry refs.
--   3. 'ClassRunState' — moves instance scope / catalogue /
--      superclass refs.
--   4. 'FallbackRunState' — moves the env-fallback caches.
--   5. 'ModuleRunState' — moves the loaded-module catalogue and
--      collapses 'resetPerRunGlobals' into one atomic swap.
--
-- Hot-path caches ('symbolCache', 'openLibs', 'ctorIndexRegistry',
-- 'globalScanCacheRegistry'), gensym counters, and lazy session memos
-- intentionally stay outside this context.
module IHC.Context
    ( -- * Top-level context
      IHCContext(..)
      -- * Sub-records
    , IHCHooks(..)
    , IHCRunState(..)
    , ClassRunState(..)
    , TypeRunState(..)
    , FallbackRunState(..)
    , ModuleRunState(..)
      -- * Construction
    , newIHCContext
    , freshHooks
    , freshRunState
    ) where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import IHC.Classes
    ( ClassRegistry
    , EnvFallbackHook
    , RegisterInstancesHook
    , ScanHook
    , ThExpToExprHook
    )
import IHC.Loader.Types (LoadedModule)
import IHC.ModuleHeader (ModuleName)
import qualified IHC.TypeAST
import qualified IHC.TypeReduce as TR
import IHC.Val (Env, Thunk, Val)

--------------------------------------------------------------------------------
-- Top-level context
--------------------------------------------------------------------------------

-- | The interpreter context threaded through evaluation and the
-- loader.  One per session; see the module header.
--
-- 'ctxRun' is itself an 'IORef' so a per-run reset collapses to one
-- atomic 'writeIORef (ctxRun ctx) =<< freshRunState' instead of the
-- current per-field clear sequence in 'resetPerRunGlobals'.
data IHCContext = IHCContext
    { ctxHooks :: !IHCHooks
    , ctxRun   :: !(IORef IHCRunState)
    }

--------------------------------------------------------------------------------
-- Hooks (write-once-per-session)
--------------------------------------------------------------------------------

-- | Install-once hooks that survive run boundaries.  All of these are
-- currently backed by module-level 'unsafePerformIO' IORefs in
-- 'IHC.Classes'; PR 1 moves the storage here without changing the
-- behavioural contract.
data IHCHooks = IHCHooks
    { hkEnvFallback         :: !(IORef EnvFallbackHook)
    , hkClassMethodFallback :: !(IORef (ByteString -> ByteString -> IO (Maybe Val)))
    , hkCoreInstanceLoad    :: !(IORef (IO ()))
    , hkRegisterInstances   :: !(IORef RegisterInstancesHook)
    , hkScan                :: !(IORef (Maybe ScanHook))
    , hkSharedClassReg      :: !(IORef (Maybe ClassRegistry))
    , hkThExpToExpr         :: !(IORef ThExpToExprHook)
    }

--------------------------------------------------------------------------------
-- Per-run state
--------------------------------------------------------------------------------

-- | Per-run state cleared by 'freshRunState' between successive
-- 'loadProgramFromSource' calls in the same session.
data IHCRunState = IHCRunState
    { rsClass    :: !ClassRunState
    , rsType     :: !TypeRunState
    , rsFallback :: !FallbackRunState
    , rsModule   :: !ModuleRunState
    }

-- | Class/instance dispatch state.  Mirrors 'instanceScopeRef',
-- 'instanceCatalogueRef', and 'superclassesRef' in 'IHC.Classes'.
--
-- Note: 'crsInstanceCatalogue' uses the inlined type
-- @Map ByteString [IO ()]@ rather than the (currently unexported)
-- 'IHC.Classes.InstanceCatalogue' alias, to keep PR 0 from touching
-- 'IHC.Classes' at all.  PR 3 unifies the two by exporting the alias.
data ClassRunState = ClassRunState
    { crsInstanceScope     :: !(IORef (Set ByteString))
    , crsInstanceCatalogue :: !(IORef (Map ByteString [IO ()]))
    , crsSuperclasses      :: !(IORef (Map ByteString [ByteString]))
    }

-- | Type-registry state.  Mirrors 'globalTypeSigsRef',
-- 'globalTypeSynonymsRef', 'globalClassMethodNamesRef',
-- 'globalMethodClassRef' in 'IHC.TypeGlobals' plus 'globalRegistry' in
-- 'IHC.TypeReduce'.
data TypeRunState = TypeRunState
    { trsTypeSigs           :: !(IORef (Map ByteString IHC.TypeAST.Scheme))
    , trsTypeSynonyms       :: !(IORef (Map ByteString (Int, IHC.TypeAST.Type)))
    , trsClassMethodNames   :: !(IORef (Set ByteString))
    , trsMethodClass        :: !(IORef (Map ByteString [ByteString]))
    , trsTypeFamilyRegistry :: !(IORef TR.TypeFamilyRegistry)
    }

-- | Env-fallback / discovery cache state.  Mirrors
-- 'envBaseForFallbackRef', 'envFallbackCache', 'envFallbackNegCacheRef',
-- 'envFallbackCacheGenRef', 'discoverNegCacheRef', and
-- 'resolveImportCacheRef' in 'IHC.Scheduler'.
data FallbackRunState = FallbackRunState
    { frsEnvBaseForFallback :: !(IORef Env)
    , frsEnvCache           :: !(IORef (Map ByteString Thunk))
    , frsEnvNegCache        :: !(IORef (Int, Set (Maybe ByteString, ByteString)))
    , frsEnvCacheGen        :: !(IORef Int)
    , frsDiscoverNegCache   :: !(IORef (Set (ByteString, ByteString)))
    , frsResolveImportCache :: !(IORef (Map (ByteString, ByteString) (Maybe ModuleName)))
    }

-- | Per-run module catalogue and search-path state.  Mirrors
-- 'globalLoadedModulesRef', 'globalSearchPathRef', and
-- 'globalIncludeMapRef' in 'IHC.Scheduler'.
data ModuleRunState = ModuleRunState
    { mrsLoadedModules :: !(IORef (Map ModuleName LoadedModule))
    , mrsSearchPath    :: !(IORef [FilePath])
    , mrsIncludeMap    :: !(IORef (Map FilePath [FilePath]))
    }

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Allocate a fresh context: empty hooks (no-op fallbacks installed)
-- + empty run-state.  Called once per IHC session.
newIHCContext :: IO IHCContext
newIHCContext = do
    hooks <- freshHooks
    rs    <- freshRunState
    runRef <- newIORef rs
    pure IHCContext
        { ctxHooks = hooks
        , ctxRun   = runRef
        }

-- | Hooks initialised with their no-op defaults.  Match the defaults
-- baked into the legacy 'unsafePerformIO' definitions in 'IHC.Classes'
-- so the behavioural contract carries over verbatim when PR 1 swaps
-- the storage:
--
--   * 'hkEnvFallback'         → @\\_ _ -> pure Nothing@
--   * 'hkClassMethodFallback' → @\\_ _ -> pure Nothing@
--   * 'hkCoreInstanceLoad'    → @pure ()@
--   * 'hkRegisterInstances'   → @\\_ -> pure ()@
--   * 'hkScan'                → 'Nothing'
--   * 'hkSharedClassReg'      → 'Nothing'
--   * 'hkThExpToExpr'         → 'error' (only fires before TH installs
--     the real decoder; matches the legacy @error \"…\"@ default).
freshHooks :: IO IHCHooks
freshHooks = do
    envFb       <- newIORef (\_ _ -> pure Nothing)
    classMethFb <- newIORef (\_ _ -> pure Nothing)
    coreLoad    <- newIORef (pure ())
    regInsts    <- newIORef (\_ -> pure ())
    scan        <- newIORef Nothing
    sharedReg   <- newIORef Nothing
    thExp       <- newIORef
        (\_ -> error "IHC.Context.freshHooks: thExpToExpr hook not installed")
    pure IHCHooks
        { hkEnvFallback         = envFb
        , hkClassMethodFallback = classMethFb
        , hkCoreInstanceLoad    = coreLoad
        , hkRegisterInstances   = regInsts
        , hkScan                = scan
        , hkSharedClassReg      = sharedReg
        , hkThExpToExpr         = thExp
        }

-- | An 'IHCRunState' with every IORef set to its natural empty value.
-- Used both at session start and on per-run reset.
freshRunState :: IO IHCRunState
freshRunState = do
    cls <- ClassRunState
        <$> newIORef Set.empty
        <*> newIORef Map.empty
        <*> newIORef Map.empty
    ty  <- TypeRunState
        <$> newIORef Map.empty
        <*> newIORef Map.empty
        <*> newIORef Set.empty
        <*> newIORef Map.empty
        <*> newIORef Map.empty
    fb  <- FallbackRunState
        <$> newIORef HashMap.empty
        <*> newIORef Map.empty
        <*> newIORef (0, Set.empty)
        <*> newIORef 0
        <*> newIORef Set.empty
        <*> newIORef Map.empty
    md  <- ModuleRunState
        <$> newIORef Map.empty
        <*> newIORef []
        <*> newIORef Map.empty
    pure IHCRunState
        { rsClass    = cls
        , rsType     = ty
        , rsFallback = fb
        , rsModule   = md
        }
