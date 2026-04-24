-- | Per-interpreter-run mutable state.
--
-- Historically the interpreter kept its state in ~22 top-level
-- @unsafePerformIO@ / @NOINLINE@ 'IORef' CAFs scattered across
-- 'IHC.Scheduler', 'IHC.Classes', 'IHC.TypeGlobals', 'IHC.Builtins',
-- 'IHC.TypeReduce', 'IHC.Cpp', 'IHC.CabalProject', 'IHC.TH'.  Those
-- CAFs made two things impossible: running two interpreters in one
-- process, and getting fresh state between sequential test cases.
--
-- This module defines the single record that replaces them.
-- 'newIHCRuntime' is the one-stop constructor; every API that
-- previously read or wrote a CAF takes an 'IHCRuntime' (or a narrow
-- projection of its fields) as argument.
--
-- The only intentional exceptions are 'IHC.FFI.openLibs' and
-- 'IHC.FFI.symbolCache', which genuinely reflect process-scoped
-- @dlopen@ / @dlsym@ state and so are correct to share across
-- multiple runtimes in the same process.
module IHC.Runtime
    ( -- * Per-module record (moved from 'IHC.Scheduler')
      LoadedModule(..)
      -- * Per-run state
    , IHCRuntime(..)
    , newIHCRuntime
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign.Ptr (IntPtr)

import IHC.AST (Expr)
import IHC.ClassesTypes
    ( ClassRegistry, EnvFallbackHook, ScanHook, newClassRegistry
    )
import IHC.FFITypes (ForeignDecl)
import IHC.ModuleHeader (ModuleHeader, ModuleName)
import qualified IHC.Parser as Parser
import qualified IHC.Scan as Scan
import IHC.Source (Source)
import qualified IHC.TypeAST as TypeAST
import qualified IHC.TypeReduce as TR
import IHC.Val (Thunk, Val)

--------------------------------------------------------------------------------
-- Per-module record
--
-- Moved here from 'IHC.Scheduler' so 'IHCRuntime' can hold an
-- @IORef (Map ModuleName LoadedModule)@ without creating a cycle
-- with modules that import 'IHC.Runtime'.
--------------------------------------------------------------------------------

data LoadedModule = LoadedModule
    { lmName        :: !ModuleName
    , lmHeader      :: !ModuleHeader
    , lmSource      :: !Source
    , lmKnown       :: !Scan.KnownSymbols
    , lmDataReg     :: !Scan.DataRegistry
    , lmFieldReg    :: !Scan.FieldRegistry
      -- | Map from type-constructor name to the data constructors it
      -- declares. Built by 'scanDataDecls' alongside 'lmDataReg'. Used by
      -- 'exportsName' so that @T(..)@ and @T(Ctor1, Ctor2)@ exports match
      -- the named constructors, not just the type head.
    , lmTypeCtorReg :: !Scan.TypeCtorRegistry
      -- | Accumulated (local-name, parsed body) pairs for this module.
    , lmBodies      :: !(IORef (Map ByteString Expr))
      -- | Whether this is the entry module (its bindings stay unqualified
      -- in the final env; foreign-module bindings are namespaced).
    , lmIsEntry     :: !Bool
      -- | Per-module fixity table: defaults + any @infixl/infixr/infix@
      -- declarations found at column 1 in this source.
    , lmFixity      :: !Parser.FixityTable
      -- | Whether this module opts out of top-level record-field
      -- accessor generation via @{-# LANGUAGE NoFieldSelectors #-}@.
      -- When true, fields from this module's 'lmFieldReg' are NOT bound
      -- under their bare names in the final env — only under the
      -- internal 'fieldProjName' alias that record-dot uses.
    , lmNoFieldSelectors :: !Bool
      -- | Per-module 'TR.TypeFamilyRegistry'. Built by
      -- 'scanTypeFamilyDecls' in the same pass that scans data decls.
      -- Unioned across all loaded modules at knot-tying time and
      -- installed into 'TR.globalRegistry' so the ETyApp path of the
      -- evaluator can reduce type-family applications at runtime.
    , lmTypeFamilies :: !TR.TypeFamilyRegistry
      -- | @foreign import ccall@ declarations scanned from this module's
      -- source.  Each entry becomes a host-backed 'Val' in the final env
      -- (see 'registerForeignImports') that dispatches the real C symbol
      -- via libffi at call time.  Populated by 'scanForeignImports'.
    , lmForeignDecls :: ![ForeignDecl]
      -- | Top-level type signatures scanned from this module's source.
      -- Used by 'IHC.Elaborate' for on-demand type inference when class
      -- dispatch hits ambiguity.  Populated by 'scanTypeSigs'.
    , lmTypeSigs    :: !(Map ByteString TypeAST.Scheme)
      -- | Top-level type synonyms (@type Name args = RHS@).  Used for
      -- one-hop expansion before unification.  Populated by
      -- 'scanTypeSynonyms'.
    , lmTypeSynonyms :: !(Map ByteString (Int, TypeAST.Type))
    }

--------------------------------------------------------------------------------
-- Per-run state
--------------------------------------------------------------------------------

data IHCRuntime = IHCRuntime
    { -- Module loading / search ------------------------------------------------
      rtLoadedModules     :: !(IORef (Map ModuleName LoadedModule))
    , rtSearchPath        :: !(IORef [FilePath])
    , rtIncludeMap        :: !(IORef (Map FilePath [FilePath]))
    , rtEnvFallbackCache  :: !(IORef (Map ByteString Thunk))
      -- | Base env used by the demand-driven fallback when it materialises
      -- a Thunk for a freshly-discovered FQN. Keeps the fallback
      -- self-consistent regardless of which import triggered the miss.
    , rtEnvFallbackBase   :: !(IORef (Map ByteString Thunk))

      -- Type inference --------------------------------------------------------
    , rtTypeSigs          :: !(IORef (Map ByteString TypeAST.Scheme))
    , rtTypeSynonyms      :: !(IORef (Map ByteString (Int, TypeAST.Type)))
    , rtClassMethodNames  :: !(IORef (Set ByteString))
    , rtTypeFamilyReg     :: !(IORef TR.TypeFamilyRegistry)

      -- Class dispatch --------------------------------------------------------
    , rtClassReg            :: !ClassRegistry
    , rtInstanceScope       :: !(IORef (Set ByteString))
    , rtScanHook            :: !(IORef (Maybe ScanHook))
    , rtEnvFallback         :: !(IORef EnvFallbackHook)
    , rtClassMethodFallback :: !(IORef (ByteString -> ByteString -> IO (Maybe Val)))
    , rtCoreInstanceLoad    :: !(IORef (IO ()))

      -- Runtime support (builtins) --------------------------------------------
    , rtCtorIndex         :: !(IORef (Map ByteString (ByteString, Int)))
    , rtUniqueCounter     :: !(IORef Int64)
    , rtForeignPtrRanges  :: !(IORef [(IntPtr, IntPtr)])
    , rtNewNameCounter    :: !(IORef Int)
    }

-- | Allocate a fresh 'IHCRuntime'.
--
-- The only side effects are 'newIORef' / 'newClassRegistry'.  Callers
-- (Driver, REPL) create exactly one 'IHCRuntime' per interpreter
-- session and thread it through every API that used to read globals.
newIHCRuntime :: IO IHCRuntime
newIHCRuntime = do
    classReg <- newClassRegistry
    IHCRuntime
        <$> newIORef Map.empty            -- rtLoadedModules
        <*> newIORef []                   -- rtSearchPath
        <*> newIORef Map.empty            -- rtIncludeMap
        <*> newIORef Map.empty            -- rtEnvFallbackCache
        <*> newIORef Map.empty            -- rtEnvFallbackBase
        <*> newIORef Map.empty            -- rtTypeSigs
        <*> newIORef Map.empty            -- rtTypeSynonyms
        <*> newIORef Set.empty            -- rtClassMethodNames
        <*> newIORef Map.empty            -- rtTypeFamilyReg
        <*> pure classReg                 -- rtClassReg
        <*> newIORef Set.empty            -- rtInstanceScope
        <*> newIORef Nothing              -- rtScanHook
        <*> newIORef (\_ -> pure Nothing) -- rtEnvFallback
        <*> newIORef (\_ _ -> pure Nothing) -- rtClassMethodFallback
        <*> newIORef (pure ())            -- rtCoreInstanceLoad
        <*> newIORef Map.empty            -- rtCtorIndex
        <*> newIORef 0                    -- rtUniqueCounter
        <*> newIORef []                   -- rtForeignPtrRanges
        <*> newIORef 0                    -- rtNewNameCounter
