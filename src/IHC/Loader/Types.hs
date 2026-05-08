-- | Loader-internal types shared between 'IHC.Scheduler' and 'IHC.Context'.
--
-- 'LoadedModule' originally lived inside 'IHC.Scheduler' but was
-- extracted here so 'IHC.Context' can reference it (via
-- @IORef (Map ModuleName LoadedModule)@) without forcing
-- @IHC.Context -> IHC.Scheduler@ and creating an import cycle (the
-- scheduler imports the context for the per-run state record).
--
-- The data type stays a loader-internal contract: it isn't part of
-- the documented IHC surface and shouldn't be imported from outside
-- the scheduler/context pair.  Tests drive the loader through
-- 'IHC.Scheduler.loadProgramFromSource' rather than constructing a
-- 'LoadedModule' directly.
module IHC.Loader.Types
    ( LoadedModule(..)
    ) where

import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)

import IHC.AST (Expr)
import qualified IHC.FFI as FFI
import IHC.ModuleHeader (ModuleHeader, ModuleName)
import IHC.Parser (FixityTable)
import IHC.Scan (DataRegistry, FieldRegistry, KnownSymbols, TypeCtorRegistry)
import IHC.Source (Source)
import qualified IHC.TypeAST
import qualified IHC.TypeReduce as TR

data LoadedModule = LoadedModule
    { lmName        :: !ModuleName
    , lmHeader      :: !ModuleHeader
    , lmSource      :: !Source
    , lmKnown       :: !KnownSymbols
    , lmDataReg     :: !DataRegistry
    , lmFieldReg    :: !FieldRegistry
      -- | Map from type-constructor name to the data constructors it
      -- declares. Built by 'scanDataDecls' alongside 'lmDataReg'. Used by
      -- 'exportsName' so that @T(..)@ and @T(Ctor1, Ctor2)@ exports match
      -- the named constructors, not just the type head.
    , lmTypeCtorReg :: !TypeCtorRegistry
      -- | Accumulated (local-name, parsed body) pairs for this module.
    , lmBodies      :: !(IORef (Map ByteString Expr))
      -- | Whether this is the entry module (its bindings stay unqualified
      -- in the final env; foreign-module bindings are namespaced).
    , lmIsEntry     :: !Bool
      -- | Per-module fixity table: defaults + any @infixl/infixr/infix@
      -- declarations found at column 1 in this source.
    , lmFixity      :: !FixityTable
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
    , lmForeignDecls :: ![FFI.ForeignDecl]
      -- | Top-level type signatures scanned from this module's source.
      -- Used by 'IHC.Elaborate' for on-demand type inference when class
      -- dispatch hits ambiguity.  Populated by 'scanTypeSigs'.
    , lmTypeSigs    :: !(Map ByteString IHC.TypeAST.Scheme)
      -- | Top-level type synonyms (@type Name args = RHS@).  Used for
      -- one-hop expansion before unification.  Populated by
      -- 'scanTypeSynonyms'.
    , lmTypeSynonyms :: !(Map ByteString (Int, IHC.TypeAST.Type))
    }
