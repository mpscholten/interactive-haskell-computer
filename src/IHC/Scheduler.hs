-- | Phase 2.5 scheduler: multi-module loading + demand-driven binding
-- discovery.
--
-- The heart of this module is still the same demand-driven loop as
-- Phase 2.0: given a free variable, find the file that defines it,
-- skim the LHS, parse the RHS, walk its free variables, recurse. The
-- new bit is that "the file" is now resolved via a tiny module loader
-- that reads module headers and import declarations to decide which
-- foreign module owns a name.
--
-- High-level algorithm:
--
--   1. Load the entry @Source@ and treat it as the entry module
--      (its name, if it declares one, otherwise @Main@).
--   2. Starting from @main@, demand-discover bindings. When a free var
--      isn't defined in the current module, walk that module's imports
--      in order; the first import whose spec accepts the name (and
--      whose source file actually defines it) is the one that owns it.
--      Load that module lazily and recurse.
--   3. Qualified references like @B.suffix@ are resolved by finding
--      the import whose alias (or name) matches @B@ and recursing
--      into that module with @suffix@.
--   4. Once every reachable binding has been parsed, build a single
--      recursive @ELet@ where foreign bindings are keyed by their
--      fully-qualified name (@\"Bar.suffix\"@) and the entry module's
--      bindings stay unqualified.
--
-- Constructor resolution: each module's @data@ declarations are
-- scanned into its own 'DataRegistry'. All registries are unioned at
-- the end.
module IHC.Scheduler
    ( -- * Entry points
      loadProgram
    , loadProgramFromSource
    , buildBaseEnv
    , loadImportIntoEnv
    , loadFileIntoEnv
      -- * Types exposed for testing
    , ModuleRegistry
    , freeVars
    , splitQualified
      -- * User-defined class dispatch (used by the REPL)
    , classMethodDispatcher
    , defaultTypeTag
      -- * Record-syntax desugaring (used by the REPL)
    , desugarRecordCons
    , desugarRecordPats
    ) where

import Control.Exception (throwIO, Exception, catch, SomeException, try)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString, isSuffixOf)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (isPrefixOf)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import qualified System.IO
import IHC.AST
import IHC.Builtins (builtinEnv, buildConEnv, buildFieldEnv)
import IHC.CabalProject (cachedPackageSearchPath, cachedPackageSearchPathWithIncludes)
import IHC.Diagnostics (warnStub)
import IHC.Classes
    ( ClassRegistry, newClassRegistry, registerInstance, lookupInstance
    , lookupInstanceMethod, typeTagOf
    )
import IHC.Cpp (cppPreprocessWithIncludes, defaultCppContext)
import IHC.Eval (force, apply)
import IHC.Lexer (startCursor)
import IHC.ModuleHeader
import qualified IHC.Parser as Parser
import IHC.Parser (FixityTable, defaultFixityTable, scanFixityDecls, ParseError)
import IHC.Scan
import IHC.Source
import IHC.TH (expandSplicesInExpr)
import qualified IHC.TypeReduce as TR
import IHC.Val

-- | Run our hand-rolled CPP over the source bytes, returning a new
-- 'Source' with the same filename and the preprocessed contents.
-- Uses no extra include-dirs; call 'cppSourceWithIncludes' when the
-- owning package's @include-dirs:@ is known.
cppSource :: Source -> IO Source
cppSource = cppSourceWithIncludes []

-- | Like 'cppSource' but also searches the given @includeDirs@ when
-- resolving @#include "file"@ directives (from the package's
-- @include-dirs:@ cabal stanza).
cppSourceWithIncludes :: [FilePath] -> Source -> IO Source
cppSourceWithIncludes includeDirs src = do
    bs' <- cppPreprocessWithIncludes includeDirs defaultCppContext (srcName src) (srcBytes src)
    pure (src { srcBytes = bs' })

--------------------------------------------------------------------------------
-- Module registry types
--------------------------------------------------------------------------------

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
    }

data ModuleState
    = Loading
    | Loaded !LoadedModule

type ModuleRegistry = IORef (Map ModuleName ModuleState)

-- | Raised when a module imports a module that is currently in the
-- middle of being loaded (mutual/circular import).
newtype ImportCycle = ImportCycle ModuleName deriving Show
instance Exception ImportCycle

-- | Raised when a module can't be located on disk.
newtype ModuleNotFound = ModuleNotFound ModuleName deriving Show
instance Exception ModuleNotFound

-- | Raised when a qualified reference can't be resolved to any import.
newtype UnresolvedName = UnresolvedName String deriving Show
instance Exception UnresolvedName

--------------------------------------------------------------------------------
-- Public entry points
--------------------------------------------------------------------------------

-- | Backwards-compatible single-source entry point: the same shape as
-- Phase 2.0's 'loadProgram' but routed through the multi-module loader
-- with an empty search path (so imports will fail unless the entry
-- module has none). Used by the existing test suite.
loadProgram :: Source -> IO (Env, Thunk)
loadProgram = loadProgramFromSource []

-- | Multi-module entry point: @searchPath@ is the list of directories
-- to look in when resolving @import Foo@ statements.
--
-- Computes 'cachedPackageSearchPath' once at startup and appends it
-- after the explicit @searchPath@, so all packages cached under
-- @~\/.cache\/ihc\/sources\/@ (including @mtl@, @transformers@,
-- @splitmix@, @random@, etc.) are available without the caller having
-- to enumerate them.
loadProgramFromSource :: [FilePath] -> Source -> IO (Env, Thunk)
loadProgramFromSource searchPath src0 = do
    -- Enumerate cached packages once; hs-source-dirs are respected via
    -- parseCabalFile inside cachedPackageSearchPath.
    -- Also collect include-dirs so CPP can find package headers.
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs      = map fst cacheWithIncludes
        includeMap     = Map.fromList cacheWithIncludes
        fullSearchPath = searchPath ++ cacheDirs

    registry <- newIORef Map.empty

    -- Phase 2.6: run CPP on the entry module's bytes before anything
    -- else touches them. Directive-free files short-circuit and are
    -- returned unchanged.
    src <- cppSource src0

    -- Phase 2.3: class registry for type-class dispatch.
    classReg <- newClassRegistry

    -- Pre-build the builtin name set so the discovery loop can short-
    -- circuit names that are provided by IHC.Builtins and never need to
    -- be walked through Prelude's re-export chain.  Without this, every
    -- use of a builtin (@putStrLn@, @print@, @+@, …) would trigger a
    -- Prelude walk that eagerly loads much of base/ghc-internal — slow
    -- and semantically pointless, because the evaluator resolves to the
    -- builtin anyway.
    earlyBuiltins <- builtinEnv classReg
    let earlyBuiltinNames = Map.keysSet earlyBuiltins

    -- Load the entry module. Its name is what the `module X where`
    -- header declares (or "Main" as a default). We always register it
    -- as the entry module so its bindings stay unqualified.
    entry <- loadEntryModule registry src
    let entryName = lmName entry

    -- Drive discovery from `main`.
    discoverInModuleWith earlyBuiltinNames registry fullSearchPath includeMap entry "main"

    -- Discover free variables of class default-method bodies and
    -- instance method bodies across every loaded module so those names
    -- are in the tied env before methods are evaluated. Without this,
    -- `class Foo a where m x = helper x` with `helper` at top-level
    -- would fail with 'unbound variable `helper`' at dispatch time.
    discoverClassAndInstanceFreeVars registry fullSearchPath includeMap

    -- Collect every loaded module.
    reg <- readIORef registry
    let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]

    -- Union data registries and field registries across all modules.
    let unionedData  = foldr Map.union Map.empty (map lmDataReg  loadedModules)
        (publicFields, unionedFields) = partitionFieldRegistries loadedModules
        unionedTypeCtors = foldr Map.union Map.empty (map lmTypeCtorReg loadedModules)
        -- Union type-family registries across all loaded modules and
        -- publish the merged result into the global 'TR.globalRegistry'
        -- so the ETyApp path in 'IHC.Eval' can look up reductions for
        -- 'symbolVal' / 'natVal' calls at runtime.  'Map.unionWith (++)'
        -- preserves every clause — multiple modules may extend the
        -- same open family with their own 'type instance' decls.
        unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                         (map lmTypeFamilies loadedModules)
    TR.setGlobalRegistry unionedTFReg
    conEnv   <- buildConEnv  unionedData
    fieldEnv <- buildFieldAccessorEnv publicFields unionedFields
    builtins <- builtinEnv classReg
    let baseNoClass = Map.union builtins (Map.union fieldEnv conEnv)
    -- User-defined class method dispatchers (Phase: Scan+Scheduler+Repl
    -- scanClassDecls). For every `class C a where m :: ...` declaration in
    -- any loaded module, bind `m` as a top-level dispatcher that looks up
    -- `(C, typeTagOf firstArg)` in the ClassRegistry and applies the
    -- selected instance method. Names that collide with built-in
    -- dispatchers (e.g. show/==/compare) are skipped.
    classMethodEnv <- buildClassMethodEnv classReg baseNoClass loadedModules
    let base = Map.union classMethodEnv baseNoClass

    -- Phase 2.11: expand TH splices in every loaded module's bodies.
    -- Run AFTER all modules are discovered (so imports are resolved) but
    -- BEFORE knot-tying. Use 'base' as the splice evaluation env — it
    -- contains all builtins including the 'lift' function.
    mapM_ (expandSplicesInModule base) loadedModules

    -- Build (fully-qualified-name, Expr) pairs for every loaded body.
    qualPairs <- concat <$> mapM (exportBodies registry (Map.keysSet builtins)) loadedModules

    -- Tie the knot for all bodies at once.
    slots <- mapM (\_ -> newIORef BlackHole) qualPairs
    let qualEnv = extendEnvMany (zip (map fst qualPairs) slots) base

    -- For FQN keys whose bare name (last dot-component) matches a builtin,
    -- point the FQN slot directly at the builtin thunk.  This ensures that
    -- rewritten references like "GHC.Internal.IO.Handle.Text.hPutBuf" resolve
    -- to the builtin rather than chasing through the source-level chain.
    -- Two cases:
    --   1. Sentinels (EVar): always forward if bare name matches builtin
    --   2. Real definitions: only forward for FFI/primop builtins (ffiBuiltinNames)
    let isSentinel (EVar _) = True
        isSentinel _        = False
    forM_ (zip qualPairs slots) $ \((fqn, rhs), slot) ->
        case BC.elemIndexEnd (toEnum (fromEnum '.')) fqn of
            Just idx -> do
                let bareName = BC.drop (idx + 1) fqn
                case Map.lookup bareName builtins of
                    Just builtinThunk
                        | isSentinel rhs || Set.member bareName ffiBuiltinNames -> do
                            builtinState <- readIORef builtinThunk
                            writeIORef slot builtinState
                    _ -> pure ()
            Nothing -> pure ()

    -- Add aliases: every binding imported into the entry module is
    -- visible there under its local name as well as the fully
    -- qualified one. Qualified imports (@import qualified Foo as B@)
    -- produce @B.name@ aliases via a different path (handled at
    -- parse / EVar-rewrite time — see splitQualified). For the
    -- simple-import case we expose the bare name.
    --
    -- Name collisions: last-writer-wins via Map.union right-bias.
    -- Entry-module bindings are inserted LAST so they always shadow
    -- imported aliases.
    aliases <- buildAliases registry fullSearchPath includeMap entry slots qualPairs
    let envWithAliases = Map.union aliases qualEnv
    let env = envWithAliases

    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure env emptyIPMap rhs)))
          (zip qualPairs slots)

    -- Phase 2.3: scan instance declarations from all loaded modules
    -- and register their method vals into the ClassRegistry. This must
    -- happen AFTER the env is fully tied so instance bodies can see all
    -- bindings (including recursive ones).
    do { classTable <- buildClassMethodTable loadedModules; mapM_ (registerInstancesFrom registry fullSearchPath includeMap classReg unionedTypeCtors classTable env) loadedModules }
    -- Register class-level default method bodies under the sentinel tag
    -- "<default>" so that the dispatcher can fall back to them when no
    -- instance-specific override exists.
    registerClassDefaults registry fullSearchPath includeMap classReg env loadedModules
    -- Synthesize user-derived Functor instances for every @deriving
    -- Functor@ annotated data/newtype decl. Runs after the explicit
    -- instance-registration pass so we can honour any hand-written
    -- @instance Functor T where ...@ already in the registry (the
    -- registrar skips types that already have a Functor dict).
    registerDerivedFunctorInstances classReg loadedModules

    case lookupEnv "main" env of
        Just t  -> pure (env, t)
        Nothing -> error ("IHC.Scheduler: no `main` binding in module "
                           <> BC.unpack entryName)

-- | Build a fresh base environment with all builtins and an empty
-- ClassRegistry. Used by the REPL to get a starting env without
-- requiring a @main@ binding.
--
-- In addition to builtins, we source-load a small set of
-- @GHC.Exception@ / @GHC.Internal.Exception@ helpers that source-loaded
-- @error@ and @throw@ reach into via @raise#@.  Concretely, when a
-- partial function like @GHC.Internal.List.head []@ bottoms out, the
-- source definition is:
--
-- @
--     head [] = error \"Prelude.head: empty list\"
-- @
--
-- and source-loaded @error@ is:
--
-- @
--     error s = raise# (errorCallWithCallStackException s ?callStack)
-- @
--
-- Without @errorCallWithCallStackException@ (and friends) reachable
-- in the evaluation env, the evaluator fails with
-- @IhcException: IHC.Eval: unbound variable errorCallWithCallStackException@
-- before @raise#@ ever sees the real message.  Pre-discovering the
-- helpers here lets the REPL's top-level handler report the real
-- @Prelude.head: empty list@ text instead.
buildBaseEnv :: IO (Env, ClassRegistry)
buildBaseEnv = do
    classReg <- newClassRegistry
    builtins <- builtinEnv classReg
    conEnv   <- buildConEnv Map.empty
    let env0 = Map.union builtins conEnv
    -- Pre-discover GHC.Exception / GHC.Internal.Exception helpers.
    -- Best-effort: swallow any exception so a cache miss (missing
    -- source, stale cache, etc.) never blocks REPL startup — the REPL
    -- still runs, just with the older fallback error message.
    env1 <- preDiscoverExceptionHelpers env0
                `catch` (\(_ :: SomeException) -> pure env0)
    -- Pre-discover implicit-Prelude constructors (Just/Nothing/Left/
    -- Right/ExitSuccess/ExitFailure) so that bare @Just 42@ at the
    -- prompt — and @:t Just 42@, which has no separate Prelude-loading
    -- hook — resolve without requiring an explicit @import Data.Maybe@.
    -- Best-effort: a cache miss or parse failure leaves the REPL
    -- running with whatever subset we managed to load.
    env2 <- preDiscoverPreludeConstructors env1
                `catch` (\(_ :: SomeException) -> pure env1)
    pure (env2, classReg)

-- | Source-load @errorCallWithCallStackException@, @errorCallException@,
-- @SomeException@, @displayException@ from @GHC.Internal.Exception@
-- and merge them into the base env unqualified.  Uses
-- 'loadImportIntoEnv' with an explicit @ImportOnly@ name list so only
-- the requested symbols and their transitive dependencies are
-- materialised — no bulk fan-out.
preDiscoverExceptionHelpers :: Env -> IO Env
preDiscoverExceptionHelpers env = do
    let names =
            [ BC.pack "errorCallWithCallStackException"
            , BC.pack "errorCallException"
            , BC.pack "SomeException"
            , BC.pack "displayException"
            ]
        imp = ImportDecl
                { impModule    = BC.pack "GHC.Internal.Exception"
                , impQualified = False
                , impAlias     = Nothing
                , impSpec      = ImportOnly names
                }
    (env', _) <- loadImportIntoEnv [] imp env
    pure env'

-- | Source-load the small set of ordinary-Hackage ADT constructors that
-- users expect to be reachable unqualified at the REPL prompt without
-- typing an explicit @import@. Concretely: @Just@ / @Nothing@ (from
-- @GHC.Internal.Maybe@), @Left@ / @Right@ (from
-- @GHC.Internal.Data.Either@), and @ExitSuccess@ / @ExitFailure@ (from
-- @GHC.Internal.IO.Exception@ — @ExitCode@'s canonical declaration
-- lives there, @System.Exit@ merely re-exports it).
--
-- Strategy: bypass 'loadImportIntoEnv' entirely — its preload phase
-- transitively walks the re-export chain into @GHC.Internal.Base@ and
-- friends, which is catastrophic for REPL startup latency. Instead we
-- call 'loadModule' on each target module, which only runs
-- 'scanDataDecls' over that single file to populate 'lmDataReg', and
-- feed the resulting 'DataRegistry' to 'buildConEnv' directly. The
-- constructor thunks returned by 'buildConEnv' are pure 'VCon'
-- builders that carry no references to the module's other exports, so
-- no import chasing is needed for them to work at the evaluator.
preDiscoverPreludeConstructors :: Env -> IO Env
preDiscoverPreludeConstructors env0 = do
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs  = map fst cacheWithIncludes
        includeMap = Map.fromList cacheWithIncludes
    registry <- newIORef Map.empty
    let targets =
            -- GHC.Internal.Maybe declares Maybe(Just, Nothing).
            [ BC.pack "GHC.Internal.Maybe"
            -- GHC.Internal.Data.Either declares Either(Left, Right).
            , BC.pack "GHC.Internal.Data.Either"
            -- ExitCode's real declaration lives in
            -- GHC.Internal.IO.Exception; System.Exit re-exports it.
            , BC.pack "GHC.Internal.IO.Exception"
            ]
    dataRegs <- mapM (scanDataRegFor registry cacheDirs includeMap) targets
    let unionedData = foldr Map.union Map.empty dataRegs
    conEnv <- buildConEnv unionedData
    -- Right-biased union means existing bindings in env0 (builtins,
    -- prior discoveries, etc.) take precedence; we only add constructor
    -- names that weren't already registered.
    pure (Map.union env0 conEnv)
  where
    scanDataRegFor registry cacheDirs includeMap modName = do
        r <- try (loadModule registry cacheDirs includeMap modName)
                :: IO (Either SomeException LoadedModule)
        case r of
            Right lm -> pure (lmDataReg lm)
            Left  _  -> pure Map.empty

-- | Load a .hs file for the REPL's @:l@ command and bring its exported
-- names into scope UNQUALIFIED, matching ghci semantics.
--
-- Algorithm:
--   1. Parse the module header to learn the export spec.
--   2. If the file declares @module Foo (exports) where@, only the
--      exported names are brought into scope; otherwise ALL top-level
--      names are exposed (bare file / @module Foo where@).
--   3. Every exported name is demand-discovered so its body lands in
--      the env.  The env keys for the entry module's own bindings are
--      already unqualified (see 'exportBodies' / lmIsEntry).
--   4. The result is the env keyed as bare names, plus the fully-
--      qualified @Module.name@ aliases for any imported libraries that
--      the file pulls in.
--
-- Returns @(updatedEnv, exportedNameCount)@.
loadFileIntoEnv
    :: [FilePath]   -- ^ search path (e.g. the file's own directory)
    -> FilePath     -- ^ the .hs file path
    -> Env          -- ^ existing REPL env (builtins etc.)
    -> IO (Env, Int)
loadFileIntoEnv searchPath path existingEnv = do
    src0 <- readSourceFile path
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs      = map fst cacheWithIncludes
        includeMap     = Map.fromList cacheWithIncludes
        fullSearchPath = searchPath ++ cacheDirs
    src <- cppSource src0
    registry <- newIORef Map.empty
    classReg <- newClassRegistry
    -- Pre-build builtin name set so discovery can short-circuit names
    -- resolved by IHC.Builtins (see 'discoverInModuleWith' for why this
    -- matters for the implicit-Prelude walk).
    earlyBuiltins <- builtinEnv classReg
    let earlyBuiltinNames = Map.keysSet earlyBuiltins
    -- Load the entry module.
    entry <- loadEntryModule registry src
    -- Determine which names to bring into scope.
    allNames <- scanAllTopLevelNames (lmSource entry)
    let header      = lmHeader entry
        -- Names the module declares it exports (or all names if ExportAll).
        exported = case mhExports header of
            ExportAll    -> allNames
            ExportList _ -> filter (exportsNameDirect entry) allNames
    -- Demand-discover every exported name.
    mapM_ (discoverInModuleWith earlyBuiltinNames registry fullSearchPath includeMap entry) exported
    -- Collect all loaded modules.
    reg <- readIORef registry
    let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]
    let unionedData   = foldr Map.union Map.empty (map lmDataReg  loadedModules)
        (publicFields, unionedFields) = partitionFieldRegistries loadedModules
        unionedTypeCtors = foldr Map.union Map.empty (map lmTypeCtorReg loadedModules)
        unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                         (map lmTypeFamilies loadedModules)
    TR.setGlobalRegistry unionedTFReg
    conEnv    <- buildConEnv  unionedData
    fieldEnv' <- buildFieldAccessorEnv publicFields unionedFields
    builtins  <- builtinEnv classReg
    let baseNoClass = Map.union builtins (Map.union fieldEnv' conEnv)
    classMethodEnv <- buildClassMethodEnv classReg baseNoClass loadedModules
    let base = Map.union classMethodEnv baseNoClass
    -- Phase 2.11: expand TH splices.
    mapM_ (expandSplicesInModule base) loadedModules
    -- Build (key, Expr) pairs.  Entry module bindings are keyed bare.
    qualPairs <- concat <$> mapM (exportBodies registry (Map.keysSet builtins)) loadedModules
    -- Tie the knot.
    slots <- mapM (\_ -> newIORef BlackHole) qualPairs
    let qualEnv = extendEnvMany (zip (map fst qualPairs) slots) base
    -- Aliases: imported libs get bare+qualified aliases in the entry scope.
    aliases <- buildAliases registry fullSearchPath includeMap entry slots qualPairs
    let innerEnv = Map.union aliases qualEnv
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
          (zip qualPairs slots)
    -- Register type-class instances.
    do { classTable <- buildClassMethodTable loadedModules; mapM_ (registerInstancesFrom registry fullSearchPath includeMap classReg unionedTypeCtors classTable innerEnv) loadedModules }
    registerClassDefaults registry fullSearchPath includeMap classReg innerEnv loadedModules
    registerDerivedFunctorInstances classReg loadedModules
    -- If the file has no `main` binding, inject `main = ()` so that the
    -- REPL user can type `main` without getting "unbound variable main".
    -- This matches the old :load behaviour and keeps the no_main regression
    -- test green.
    let hasMain = BC.pack "main" `elem` allNames
    mainFallback <- if hasMain
        then pure Map.empty
        else do
            slot <- newIORef (Evaluated VUnit)
            pure (Map.singleton (BC.pack "main") slot)
    -- Merge into the existing REPL env (existing wins on collision).
    let newBindings = Map.union aliases (Map.union qualEnv mainFallback)
        additions   = Map.difference newBindings existingEnv
        merged      = Map.union existingEnv additions
        -- Count exported names that are newly visible (bare unqualified keys).
        newExportedCount = length (filter (`Map.member` additions) exported)
    pure (merged, newExportedCount)

-- | Enumerate the effective exports of a loaded module as @(bare-name, Slot)@
-- pairs, walking transitive @ExportModule@ and named @ExportName@ re-exports
-- recursively.
--
-- PRECONDITION: all modules that may be needed have already been loaded and
-- discovered into @registry@.  @thunkByKey@ maps every @\"Module.name\"@ key
-- to its pre-allocated Thunk.  This function performs ONLY lookups — no new
-- modules are loaded, no new thunks are created.
--
-- Algorithm for each entry in the module's export list:
--   * @ExportName n@ — look up the slot for @n@ first in @lm@ itself, then via
--     a suffix search in @thunkByKey@ (for named re-exports where the slot is
--     keyed under the owning module).
--   * @ExportModule m@ — recursively call 'effectiveExports' on @m@, merge results.
--   * @ExportType n _@ — include the type constructor name @n@ via the same lookup.
--   * @ExportAll@ (no export list) — return every top-level body in the module.
--
-- @visited@ prevents cycles from circular @module Foo@ re-export chains.
-- Duplicate names deduplicate with last-entry-wins (Map.fromList keeps the last).
--
-- Memoization: modules like @IHP.Prelude@ re-export ~19 modules, each of which
-- transitively re-exports others that share common sub-modules (e.g. @GHC.Base@
-- appears in many chains).  Without memoization the walker has roughly cubic
-- cost in (#re-exports × #imports × #names) and hangs the loader for several
-- seconds or more.  We keep a per-call @IORef (Map ModuleName [(Name,Thunk)])@
-- so the result for each module is computed at most once.
--
-- Cycle interaction: when an @ExportModule m@ entry would recurse into a
-- module already in @visited@, we skip it and return @[]@ for that entry
-- WITHOUT poisoning the memo.  The memo stores a module's result once it's
-- been fully computed top-down (i.e. all its @ExportModule@ entries either
-- completed or were cycle-pruned).  Because cycles are pathological and we
-- always compute a module's exports the same way regardless of @visited@
-- (aside from the cycle-prune which only matters on the pathological path),
-- keying the memo by @ModuleName@ is safe in practice.
type ExportsMemo = IORef (Map ModuleName [(ByteString, Thunk)])

newExportsMemo :: IO ExportsMemo
newExportsMemo = newIORef Map.empty

effectiveExports
    :: ModuleRegistry
    -> Map ByteString Thunk     -- ^ thunkByKey: fully-qualified key → pre-built slot
    -> LoadedModule             -- ^ the module being interrogated
    -> [ModuleName]             -- ^ visited: cycle guard
    -> IO [(ByteString, Thunk)] -- ^ (bare-name, slot)
effectiveExports registry thunkByKey lm visited = do
    memo <- newExportsMemo
    effectiveExportsMemo memo registry thunkByKey lm visited

-- | Memoized variant of 'effectiveExports'.  Callers who make multiple
-- 'effectiveExports' calls in a single session (e.g. 'loadImportIntoEnv'
-- driving through a deeply re-exporting module) should build one memo via
-- 'newExportsMemo' and share it across calls to get the full benefit.
effectiveExportsMemo
    :: ExportsMemo
    -> ModuleRegistry
    -> Map ByteString Thunk
    -> LoadedModule
    -> [ModuleName]
    -> IO [(ByteString, Thunk)]
effectiveExportsMemo memo registry thunkByKey lm visited = do
    cached <- Map.lookup (lmName lm) <$> readIORef memo
    case cached of
        Just result -> pure result
        Nothing -> do
            result <- compute
            -- Only cache once fully computed.  We don't cache mid-recursion,
            -- so cycles (which return [] without recursing) don't poison the
            -- memo for modules still on the stack.
            modifyIORef' memo (Map.insert (lmName lm) result)
            pure result
  where
    compute = case mhExports (lmHeader lm) of
        ExportAll -> do
            -- No export list: all locally-discovered bodies are exported.
            bodies <- readIORef (lmBodies lm)
            let prefix = lmName lm <> BC.pack "."
            pure [ (n, t)
                 | n <- Map.keys bodies
                 , Just t <- [Map.lookup (prefix <> n) thunkByKey]
                 ]
        ExportList items -> do
            pairs <- concat <$> mapM (exportItem lm) items
            -- Deduplicate: last entry wins, matching Haskell's import shadowing.
            pure (Map.toList (Map.fromList pairs))

    exportItem lm' item = case item of
        ExportName n    -> lookupName lm' n
        ExportType n _  -> lookupName lm' n
        ExportModule m  -> do
            -- Cycle guard.
            if m `elem` visited
                then pure []
                else do
                    reg <- readIORef registry
                    case Map.lookup m reg of
                        Just (Loaded reLm) ->
                            effectiveExportsMemo memo registry thunkByKey reLm
                                (m : visited)
                        _ -> pure []

    -- Look up @n@ exported from @lm'@:
    --   1. First try @lm'@'s own direct qualified key (name defined
    --      locally in the module).
    --   2. Then walk @lm'@'s unqualified imports, respecting each
    --      import's filter (ImportOnly/ImportHiding). This is the
    --      multi-level re-export chain: e.g. @Data.List@ lists
    --      @sort@ in its export list but the definition lives in
    --      @Data.OldList@, reached via
    --      @import Data.OldList hiding (all, and, ...)@.  The chain
    --      can extend further (Data.OldList → GHC.OldList → GHC.List)
    --      so we recurse through 'lookupName' (cycle-guarded via
    --      @visitedImports@) until we find a module whose direct
    --      qualified key is in @thunkByKey@.
    --   3. As a last-resort fallback, a plain suffix search catches
    --      cases where a module was loaded outside the declared
    --      import graph (e.g. via 'preloadImportsForNamedReexports'
    --      but not reachable through the static import list).
    lookupName lm' n =
        lookupNameIn [lmName lm'] lm' n

    lookupNameIn visitedImports lm' n =
        let prefix = lmName lm' <> BC.pack "."
            suffix = BC.pack "." <> n
        in case Map.lookup (prefix <> n) thunkByKey of
            Just t  -> pure [(n, t)]
            Nothing -> do
                -- Walk the module's own unqualified imports, filtered
                -- by the import spec (ImportHiding / ImportOnly).
                let viaImports = filter (\i ->
                            not (impQualified i)
                         && impModule i /= BC.pack "Prelude"
                         && impModule i `notElem` visitedImports
                         && specAllows (impSpec i) n)
                        (mhImports (lmHeader lm'))
                reg <- readIORef registry
                foundViaImports <- goImports reg viaImports
                case foundViaImports of
                    (p:_) -> pure [p]
                    [] ->
                        -- Last-resort suffix search.
                        case [ t | (k, t) <- Map.toList thunkByKey
                                 , suffix `isSuffixOf` k ] of
                            (t:_) -> pure [(n, t)]
                            []    -> pure []
      where
        goImports _   []         = pure []
        goImports reg (imp:rest) =
            case Map.lookup (impModule imp) reg of
                Just (Loaded viaLm) -> do
                    ps <- lookupNameIn (impModule imp : visitedImports) viaLm n
                    case ps of
                        []  -> goImports reg rest
                        xs  -> pure xs
                _ -> goImports reg rest

-- | Memoization set for 'preloadForEffectiveExports'.  Once we've fully
-- processed a module (discovered its exported locals + walked its
-- @ExportModule@ / @ExportName@ chains), there is no benefit to doing it
-- again: the registry and @lmBodies@ IORefs already carry the results.
--
-- Without this set, the @O(re-exports × chain-depth)@ walk for modules like
-- @IHP.Prelude@ revisits the same dozen modules (@GHC.Base@, @GHC.Show@,
-- @GHC.Types@, …) on every re-export arm and hangs the loader.
type PreloadMemo = IORef (Set ModuleName)

newPreloadMemo :: IO PreloadMemo
newPreloadMemo = newIORef Set.empty

-- | Pre-load and discover all modules and bindings that will be needed by
-- 'effectiveExports' for @lm@.  Unlike 'discoverInModule' / 'resolveImport',
-- this helper explicitly loads cache modules because the user explicitly asked
-- to import them.
--
-- Callers driving multiple preloads over a shared set of modules (e.g.
-- @loadImportIntoEnv@'s fixed-point loop) share a single 'PreloadMemo' via
-- 'newPreloadMemo' so each module's preload runs at most once regardless of
-- how many re-export chains reach it.  Before memoization, IHP.Prelude's ~19
-- re-export arms caused the loader to hang for many seconds because the same
-- dozen modules (@GHC.Base@, @GHC.Show@, @GHC.Types@, …) were visited once
-- per arm.
--
-- Steps (idempotent):
--   1. Discover all locally-defined top-level names in @lm@.
--   2. For each missing @ExportName@ in @lm@, preload candidate unqualified
--      import modules once so their local bodies and transitive re-exports
--      are available to 'effectiveExports'.
--   3. For each @ExportModule m@ in @lm@'s export list, recursively call
--      'preloadForEffectiveExportsMemo' on @m@ (cycle-guarded by @visited@).
preloadForEffectiveExportsMemo
    :: PreloadMemo
    -> ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]
    -> IO ()
preloadForEffectiveExportsMemo memo registry searchPath includeMap lm visited = do
    done <- readIORef memo
    if Set.member (lmName lm) done
        then pure ()
        else do
            -- Mark BEFORE recursing so cycles that reach back into @lm@
            -- don't re-enter.  The body below is idempotent (scanAllTop... +
            -- discoverSafe skip on missing names; loadReexportMod already
            -- tolerates re-entry through the registry's Loaded state).
            modifyIORef' memo (Set.insert (lmName lm))
            -- 1. Discover only the locally-defined names that can actually
            --    contribute to this module's export surface. Private helpers
            --    do not need to be preloaded just to answer a REPL import.
            allLocalNames <- scanAllTopLevelNames (lmSource lm)
            let localSet = Set.fromList allLocalNames
                exportedLocalNames = case mhExports (lmHeader lm) of
                    ExportAll    -> allLocalNames
                    ExportList xs ->
                        [ n
                        | item <- xs
                        , n <- case item of
                            ExportName n'   -> [n']
                            ExportType n' _ -> [n']
                            ExportModule _  -> []
                        , n `Set.member` localSet
                        ]
            mapM_ (discoverSafe lm) exportedLocalNames
            -- 2. Named re-exports not locally defined.
            let namedRe = case mhExports (lmHeader lm) of
                    ExportAll    -> []
                    ExportList xs ->
                        [ n | ExportName n <- xs
                            , n `Set.notMember` localSet
                            , not (BC.null n) && BC.head n >= 'a' && BC.head n <= 'z'
                        ]
            preloadImportsForNamedReexportsMemo memo registry searchPath includeMap
                lm visited namedRe
            -- 3. ExportModule re-exports — recurse.
            let reexportMods = filter (`notElem` visited)
                                   (moduleReexports (lmHeader lm))
            mapM_ loadReexportMod reexportMods
  where
    -- Discover a single name in the given module, ignoring any parse error.
    discoverSafe m' n = do
        r <- try (discoverInModule registry searchPath includeMap m' n)
                 :: IO (Either SomeException ())
        case r of
            Right () -> pure ()
            Left  _  -> pure ()   -- skip unparseable bindings

    loadReexportMod m = do
        reLm <- loadModule registry searchPath includeMap m
                   `catch` (\(_ :: ModuleNotFound) ->
                       warnMissingStub m searchPath
                         >> buildEmptyStubModule m)
        -- Wrap entire sub-load in try so a broken re-exported module
        -- doesn't abort preloading of other modules.
        r <- try (preloadForEffectiveExportsMemo memo registry searchPath
                     includeMap reLm (m : visited))
                 :: IO (Either SomeException ())
        case r of
            Right () -> pure ()
            Left  _  -> pure ()

-- | Preload the unqualified import modules of @lm@ that could provide any of
-- the named re-exports in @missingNames@. Each candidate import module is
-- visited at most once per call (both because @viable@ filters against the
-- cycle @visited@ list and because 'preloadForEffectiveExportsMemo' skips
-- already-preloaded modules).
--
-- Parse errors and other exceptions from individual module loads or
-- recursive preloads are silently swallowed: if a candidate module can't be
-- parsed, we skip it and continue.
preloadImportsForNamedReexportsMemo
    :: PreloadMemo
    -> ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]
    -> [ByteString]
    -> IO ()
preloadImportsForNamedReexportsMemo memo registry searchPath includeMap lm visited missingNames = do
    let unqualImports = filter (not . impQualified) (mhImports (lmHeader lm))
        viable = filter (\i ->
            impModule i /= BC.pack "Prelude" &&
            impModule i `notElem` visited &&
            any (specAllows (impSpec i)) missingNames) unqualImports
    mapM_ tryLoad viable
  where
    tryLoad imp = do
        -- Wrap the entire load+preload in a try; if parsing fails for this
        -- module we silently skip it (best-effort).
        result <- try $ do
            srcLm <- loadModule registry searchPath includeMap (impModule imp)
                       `catch` (\(_ :: ModuleNotFound) ->
                           warnMissingStub (impModule imp) searchPath
                             >> buildEmptyStubModule (impModule imp))
            preloadForEffectiveExportsMemo memo registry searchPath includeMap srcLm
                (impModule imp : visited)
        case (result :: Either SomeException ()) of
            Right () -> pure ()
            Left  _  -> pure ()   -- parse error or other failure: skip silently

-- | Load a single import declaration into an existing 'Env', as if the
-- REPL user typed @import Foo@ at the prompt.
--
-- For builtin-backed modules (Prelude, Data.List, etc.) the names are
-- already present in the base env — we return the env unchanged with
-- count 0.
--
-- For source-backed modules we:
--   1. Load the target module.
--   2. Call 'preloadForEffectiveExports' to force-load all transitively
--      re-exported modules (ExportModule and ExportName chains).
--   3. Build thunks for every loaded module.
--   4. Call 'effectiveExports' to enumerate the full visible name set.
--   5. Merge into @existingEnv@.
--
-- Qualified imports (@import qualified Foo as F@) expose names under the
-- alias prefix (@F.name@) only; unqualified imports also add bare names.
--
-- Returns the updated env and the count of NEW names added.
loadImportIntoEnv
    :: [FilePath]   -- ^ directories to search for module source files
    -> ImportDecl   -- ^ the parsed import declaration
    -> Env          -- ^ current REPL env
    -> IO (Env, Int)
loadImportIntoEnv searchPath imp existingEnv
    | isBuiltinBackedModule (impModule imp) = pure (existingEnv, 0)
    | ImportOnly names <- impSpec imp = loadImportOnlyIntoEnv searchPath imp names existingEnv
    | otherwise = do
        -- Append cached package search dirs so mtl, transformers, etc. resolve.
        cacheWithIncludes <- cachedPackageSearchPathWithIncludes
        let cacheDirs      = map fst cacheWithIncludes
            includeMap     = Map.fromList cacheWithIncludes
            fullSearchPath = searchPath ++ cacheDirs
        registry <- newIORef Map.empty
        -- Step 1: load the target module.
        targetLm <- loadModule registry fullSearchPath includeMap (impModule imp)
        -- Step 2: pre-load every module that will be needed by effectiveExports
        -- (ExportName re-exports + ExportModule transitive closures).
        -- This populates the registry and lmBodies IORefs so that Step 4
        -- can do pure lookups.
        --
        -- We run preloadForEffectiveExports to a FIXED POINT: after each pass,
        -- check whether any new modules were loaded (e.g. forceLoadForReexport
        -- may have loaded GHC.List while processing Data.List → Data.OldList).
        -- Those newly loaded modules also need their local names discovered so
        -- they end up in thunkByKey.  Repeat until the registry stabilises.
        --
        -- Share a single preload memo across ALL passes so modules reachable
        -- via many re-export chains (common case: everything in base reaches
        -- @GHC.Base@) are preloaded exactly once.  Without sharing, IHP.Prelude
        -- (19 re-export arms) hangs the loader for many seconds.
        preloadMemo <- newPreloadMemo
        preloadForEffectiveExportsMemo preloadMemo registry fullSearchPath
            includeMap targetLm [lmName targetLm]
        let runUntilStable processed = do
                regBefore <- readIORef registry
                let knownNames = Map.keysSet regBefore
                -- Only preload modules that were added since the previous pass.
                -- Re-running every loaded module on every iteration causes
                -- quadratic work on import-heavy modules and can make REPL
                -- imports appear hung.
                let newlyLoaded =
                        [ lm
                        | (m, Loaded lm) <- Map.toList regBefore
                        , m `Set.notMember` processed
                        ]
                mapM_ (\lm -> do
                    r <- try (preloadForEffectiveExportsMemo preloadMemo registry
                                  fullSearchPath includeMap lm [lmName lm])
                             :: IO (Either SomeException ())
                    case r of { Right () -> pure (); Left _ -> pure () }
                    ) newlyLoaded
                regAfter <- readIORef registry
                let newNames = Map.keysSet regAfter
                if newNames == knownNames
                    then pure ()   -- stable: no new modules loaded
                    else runUntilStable (Set.union processed newNames)
        runUntilStable (Set.singleton (lmName targetLm))
        -- Step 3: collect all loaded modules and build the combined env.
        reg0 <- readIORef registry
        let loadedModules0 = [ lm | (_, Loaded lm) <- Map.toList reg0 ]
        let unionedData   = foldr Map.union Map.empty (map lmDataReg  loadedModules0)
            (publicFields, unionedFields) = partitionFieldRegistries loadedModules0
            unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                             (map lmTypeFamilies loadedModules0)
        TR.setGlobalRegistry unionedTFReg
        conEnv    <- buildConEnv  unionedData
        fieldEnv' <- buildFieldAccessorEnv publicFields unionedFields
        builtins <- builtinEnv =<< newClassRegistry
        let baseForImport = Map.union builtins (Map.union fieldEnv' conEnv)
        -- Build (qualified-key, Expr) pairs for each loaded module.
        qualPairs <- concat <$> mapM (exportBodies registry (Map.keysSet builtins)) loadedModules0
        -- Tie the knot.
        slots <- mapM (\_ -> newIORef BlackHole) qualPairs
        let qualEnv    = extendEnvMany (zip (map fst qualPairs) slots) baseForImport
            thunkByKey = Map.fromList (zip (map fst qualPairs) slots)
            modPrefix  = lmName targetLm <> BC.pack "."
            qualPrefix = case impAlias imp of
                Just a  -> a <> BC.pack "."
                Nothing
                    | impQualified imp -> lmName targetLm <> BC.pack "."
                    | otherwise        -> BC.empty
        -- Step 4: enumerate effective exports via transitive re-export walking.
        effectivePairs0 <- effectiveExports registry thunkByKey targetLm
                              [lmName targetLm]
        -- Filter by the import spec (e.g. ImportOnly, ImportHiding).
        let effectivePairs = [ (n, t) | (n, t) <- effectivePairs0
                                      , specAllows (impSpec imp) n ]
        -- Step 5: build bare and qualified alias lists.
        let bareAliases
                | impQualified imp = []
                | otherwise        = effectivePairs
            qualAliases
                | BC.null qualPrefix = []
                | otherwise =
                    [ (qualPrefix <> n, t) | (n, t) <- effectivePairs ]
        let aliasEnv = Map.fromList (bareAliases ++ qualAliases)
        -- For intra-module self-references (recursive calls, mutual recursion),
        -- ALL discovered names must be reachable by their bare name inside the
        -- closure env, regardless of whether the import is qualified.
        --
        -- Example: GHC.Base.map contains `map f (x:xs) = f x : map f xs`.
        -- The recursive `map` must resolve to the same slot.  Without bare
        -- aliases in innerEnv, qualified imports (where bareAliases = []) would
        -- leave `map` unbound inside its own body and cause infinite recursion
        -- or "unbound variable `map`" at runtime.
        --
        -- We use effectivePairs (all effective exports of the target) as the
        -- minimal set.  These cover every name that could appear as a recursive
        -- call site inside the imported bindings.
        let selfAliases =
                [ (n, slot)
                | (qualKey, slot) <- Map.toList thunkByKey
                , BC.isPrefixOf modPrefix qualKey
                , let n = BC.drop (BC.length modPrefix) qualKey
                ]
        -- The final env visible to imported bindings: qualEnv + aliases +
        -- bare-name aliases for ALL effective exports (for recursive closures).
        -- We also fall back to @existingEnv@ at the bottom of the lookup
        -- chain so that REPL-level pre-discoveries (e.g. the GHC.Exception
        -- helpers primed by 'buildBaseEnv') remain reachable from inside
        -- imported source bindings.  This matters for source-loaded
        -- @error@: its body references @errorCallWithCallStackException@,
        -- which @loadImportIntoEnv@'s fresh registry does not resolve on
        -- its own, but the REPL's pre-primed slot is still valid and
        -- must remain visible under the fresh import's innerEnv.
        let innerEnv = Map.union (Map.fromList selfAliases)
                     $ Map.union (Map.fromList effectivePairs)
                     $ Map.union aliasEnv
                     $ Map.union qualEnv existingEnv
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
              (zip qualPairs slots)
        -- Merge into the REPL env: prefer existing REPL bindings (shadow).
        let newBindings = Map.union qualEnv aliasEnv
            additions   = Map.difference newBindings existingEnv
            merged      = Map.union existingEnv additions
            newAliases  = aliasEnv `Map.difference` existingEnv
        pure (merged, Map.size newAliases)

loadImportOnlyIntoEnv
    :: [FilePath]
    -> ImportDecl
    -> [ByteString]
    -> Env
    -> IO (Env, Int)
loadImportOnlyIntoEnv searchPath imp requested0 existingEnv = do
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs      = map fst cacheWithIncludes
        includeMap     = Map.fromList cacheWithIncludes
        fullSearchPath = searchPath ++ cacheDirs
        requested      = nubBS requested0
    registry <- newIORef Map.empty
    classReg <- newClassRegistry
    targetLm <- loadModule registry fullSearchPath includeMap (impModule imp)
    mapM_ (discoverInModule registry fullSearchPath includeMap targetLm) requested
    -- ImportOnly is the REPL's deferred-name path: keep it targeted.
    -- Preloading every discovered dependency's full export surface defeats
    -- the point and makes requests like Prelude.map bulk-load GHC.Base's
    -- entire ExportAll set before the prompt can return.
    reg0 <- readIORef registry
    let loadedModules0 = [ lm | (_, Loaded lm) <- Map.toList reg0 ]
        unionedData    = foldr Map.union Map.empty (map lmDataReg loadedModules0)
        (publicFields, unionedFields) = partitionFieldRegistries loadedModules0
        unionedTypeCtors0 = foldr Map.union Map.empty (map lmTypeCtorReg loadedModules0)
        unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                         (map lmTypeFamilies loadedModules0)
    TR.setGlobalRegistry unionedTFReg
    conEnv    <- buildConEnv unionedData
    fieldEnv' <- buildFieldAccessorEnv publicFields unionedFields
    builtins  <- builtinEnv classReg
    let baseNoClass = Map.union builtins (Map.union fieldEnv' conEnv)
    classMethodEnv <- buildClassMethodEnv classReg baseNoClass loadedModules0
    let baseForImport = Map.union classMethodEnv baseNoClass
    mapM_ (expandSplicesInModule baseForImport) loadedModules0
    qualPairs <- concat <$> mapM (exportBodies registry (Map.keysSet builtins)) loadedModules0
    slots <- mapM (\_ -> newIORef BlackHole) qualPairs
    requestedPairs <- mapMaybe id <$> mapM (resolveRequestedPair targetLm qualPairs slots) requested
    let qualEnv    = extendEnvMany (zip (map fst qualPairs) slots) baseForImport
        thunkByKey = Map.fromList (zip (map fst qualPairs) slots)
        modPrefix  = lmName targetLm <> BC.pack "."
        qualPrefix = case impAlias imp of
            Just a  -> a <> BC.pack "."
            Nothing
                | impQualified imp -> lmName targetLm <> BC.pack "."
                | otherwise        -> BC.empty
        bareAliases
            | impQualified imp = []
            | otherwise        = requestedPairs
        qualAliases
            | BC.null qualPrefix = []
            | otherwise =
                [ (qualPrefix <> n, t) | (n, t) <- requestedPairs ]
        aliasEnv = Map.fromList (bareAliases ++ qualAliases)
    aliases <- buildAliases registry fullSearchPath includeMap targetLm slots qualPairs
    rewriteAliasPairs <- concat <$> mapM (rewriteAliases registry thunkByKey (Map.keysSet builtins)) loadedModules0
    let selfAliases =
            [ (n, slot)
            | (qualKey, slot) <- Map.toList thunkByKey
            , BC.isPrefixOf modPrefix qualKey
            , let n = BC.drop (BC.length modPrefix) qualKey
            ]
        -- innerEnv (see the parallel note in 'loadImportIntoEnv'): we
        -- include @existingEnv@ as the lowest-priority layer so that
        -- REPL-level pre-discoveries (e.g. the GHC.Exception helpers
        -- primed by 'buildBaseEnv') remain reachable from inside the
        -- imported bindings.
        innerEnv = Map.union (Map.fromList selfAliases)
                 $ Map.union (Map.fromList requestedPairs)
                 $ Map.union (Map.fromList rewriteAliasPairs)
                 $ Map.union aliases
                 $ Map.union qualEnv existingEnv
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
          (zip qualPairs slots)
    do { classTable <- buildClassMethodTable loadedModules0; mapM_ (registerInstancesFrom registry fullSearchPath includeMap classReg unionedTypeCtors0 classTable innerEnv) loadedModules0 }
    registerClassDefaults registry fullSearchPath includeMap classReg innerEnv loadedModules0
    registerDerivedFunctorInstances classReg loadedModules0
    let additions  = Map.difference aliasEnv existingEnv
        merged     = Map.union existingEnv additions
    pure (merged, Map.size additions)
  where
    resolveRequestedPair lm qualPairs slots n = do
        bodies <- readIORef (lmBodies lm)
        let thunkByKey   = Map.fromList (zip (map fst qualPairs) slots)
            ownKey       = lmName lm <> BC.pack "." <> n
            ownIsLocal   = case Map.lookup n bodies of
                Just expr -> expr /= EVar n
                Nothing   -> False
            suffix       = BC.pack "." <> n
            fallbackSlot =
                case [ t | (k, t) <- Map.toList thunkByKey
                         , suffix `isSuffixOf` k ] of
                    (t:_) -> Just t
                    []    -> Nothing
            slot
                | ownIsLocal = Map.lookup ownKey thunkByKey
                | otherwise  = fallbackSlot <|> Map.lookup ownKey thunkByKey
        pure ((n,) <$> slot)

    rewriteAliases registry thunkByKey builtinNames lm = do
        rw <- buildImportRewrites registry lm builtinNames
        pure
            [ (alias, slot)
            | (alias, targetKey) <- Map.toList rw
            , BC.elem '.' alias
            , Just slot <- [Map.lookup targetKey thunkByKey]
            ]

-- | Phase 2.11: expand TH splices in all bodies of a loaded module.
-- Mutates the @lmBodies@ IORef in place.
expandSplicesInModule :: Env -> LoadedModule -> IO ()
expandSplicesInModule spliceEnv lm = do
    bodies <- readIORef (lmBodies lm)
    expanded <- mapM (expandSplicesInExpr spliceEnv emptyIPMap 0) bodies
    writeIORef (lmBodies lm) expanded

-- | Scan @instance C T where ...@ declarations in a module's source,
-- parse each method body, evaluate it to a Val, and register the
-- resulting dict in the ClassRegistry.
--
-- @typeCtors@ maps every user-declared type constructor to the list of
-- its data-constructor names (e.g. @\"Color\" -> [\"Red\", \"Green\", \"Blue\"]@).
-- Since 'typeTagOf' returns the data-constructor name at runtime, we
-- register each instance under the type's head name AND under every
-- data-constructor name so dispatch can find it either way.
-- | Per-class method-name table: for each class, the alphabetical list of
-- method names declared in that class. Used to align instance method Vals
-- to the correct slot when the instance only defines a subset of the
-- class's methods (or when some methods fail to parse/evaluate).
type ClassMethodTable = Map ByteString [ByteString]

-- | Build a 'ClassMethodTable' by scanning every loaded module's class
-- declarations. If the same class name appears in multiple modules (rare —
-- usually a re-export), the first one wins.
buildClassMethodTable :: [LoadedModule] -> IO ClassMethodTable
buildClassMethodTable loadedModules = do
    tables <- mapM (\lm -> do
                        decls <- scanClassDecls (lmSource lm)
                        pure [ (classClassName d, classMethodNames d) | d <- decls ])
                   loadedModules
    pure (Map.fromList (concat tables))

registerInstancesFrom :: ModuleRegistry -> [FilePath] -> Map FilePath [FilePath] -> ClassRegistry -> TypeCtorRegistry -> ClassMethodTable -> Env -> LoadedModule -> IO ()
registerInstancesFrom registry searchPath includeMap classReg typeCtors classTable env lm = do
    decls <- scanInstanceDecls (lmSource lm)
    mapM_ (registerOne registry searchPath includeMap classReg typeCtors classTable env lm) decls

-- | Sentinel used as a placeholder when an instance method can't be
-- evaluated (parse error, unbound helper, etc.). The dispatcher detects
-- this sentinel and falls through to the class's default method body,
-- preserving partial-instance semantics.
methodPlaceholder :: Val
methodPlaceholder = VCon (BC.pack "<ihc-method-placeholder>") []

isMethodPlaceholder :: Val -> Bool
isMethodPlaceholder (VCon n []) = n == BC.pack "<ihc-method-placeholder>"
isMethodPlaceholder _           = False

registerOne
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> ClassRegistry
    -> TypeCtorRegistry
    -> ClassMethodTable
    -> Env
    -> LoadedModule
    -> InstanceDecl
    -> IO ()
registerOne registry searchPath includeMap classReg typeCtors classTable env lm (InstanceDecl cls typ typeNames methods) = do
    -- Pre-build the import-rewrite map so we can rewrite references
    -- inside the instance method bodies before evaluation.  We use
    -- 'buildImportRewritesForNames' which resolves each imported name
    -- (qualified or unqualified) to the fully-qualified key stored in
    -- the flat env, including walking named-reexport chains (so
    -- @List.foldr@ → @GHC.Internal.Base.foldr@ when Base defines it
    -- and List re-exports).  Without this, an instance body that uses
    -- a qualified alias defined in its own module would fail with
    -- "unbound variable" even though the target binding exists.
    instMethodFVs <- collectInstanceMethodFVs lm methods
    -- Demand-load each free var in the owning module so the flat env
    -- (and 'buildImportRewritesForNames' that reads 'lmBodies') sees
    -- the rewrite targets.  Without this, instance methods that use
    -- qualified aliases whose bare names haven't been demanded yet
    -- (e.g. @Foldable []@ uses @List.foldl@ but no user call triggers
    -- @foldl@ discovery) can't be rewritten to the owner's FQN.
    mapM_ (\fv -> do
              r <- try (discoverInModule registry searchPath includeMap lm fv)
                        :: IO (Either SomeException ())
              case r of
                  Right () -> pure ()
                  Left  _  -> pure ())
          (Set.toList instMethodFVs)
    rewrites <- buildImportRewritesForNames registry lm instMethodFVs
    -- Build a name-keyed method table. When the class declaration is
    -- known, the class's declared method names are canonical for
    -- dispatch. Extra instance bindings are preserved under their own
    -- names so non-standard extensions don't crash registration, but
    -- dispatch only consults the class-declared names. If the class
    -- declaration isn't available, keep every method the instance
    -- provided so the legacy fallback still works.
    let methodMap = Map.fromList methods
    methodVals <- case Map.lookup cls classTable of
        Just classMethods -> do
            let classMethodSet = Set.fromList classMethods
                extraMethods =
                    [ (mn, lhs)
                    | (mn, lhs) <- methods
                    , not (Set.member mn classMethodSet)
                    ]
            classEntries <- mapM (\mn -> do
                    v <- evalOneMethod rewrites mn (Map.lookup mn methodMap)
                    pure (mn, v))
                classMethods
            extraEntries <- mapM (\(mn, lhs) -> do
                    v <- evalOneMethod rewrites mn (Just lhs)
                    pure (mn, v))
                extraMethods
            pure (Map.fromList (classEntries ++ extraEntries))
        Nothing ->
            Map.fromList <$>
                mapM (\(mn, lhs) -> do
                    v <- evalOneMethod rewrites mn (Just lhs)
                    pure (mn, v))
                methods
    -- Register under the head type name (used by Bool/Int/Char/String
    -- dispatch via 'typeTagOf' specializations).
    registerInstance classReg cls typ methodVals
    -- Also register under every data constructor of that type so
    -- that 'typeTagOf (VCon n _) = n' lookups succeed for user
    -- ADTs like @data Color = Red | Green | Blue@.
    case Map.lookup typ typeCtors of
        Just ctors ->
            mapM_ (\ctor ->
                      registerInstance classReg cls ctor methodVals)
                  ctors
        Nothing -> pure ()
  where
    evalOneMethod _rw _mn Nothing = pure methodPlaceholder
    evalOneMethod rw _mn (Just lhs) = do
        r <- try (evalMethodWith env lm rw (_mn, lhs)) :: IO (Either SomeException Val)
        case r of
            Right v -> pure v
            Left  _ -> pure methodPlaceholder

evalMethod :: Env -> LoadedModule -> (ByteString, BindingLhs) -> IO Val
evalMethod env lm lhs = evalMethodWith env lm Map.empty lhs

-- | Like 'evalMethod' but first applies an import-rewrite map to the
-- parsed method expression.  The rewrite resolves names that are only
-- visible through the owning module's qualified imports (e.g. @List.foldr@
-- when the module has @import qualified GHC.Internal.List as List@) and
-- walks named re-exports so the final key points at the module that
-- actually defines the binding.
evalMethodWith :: Env -> LoadedModule -> Map ByteString ByteString -> (ByteString, BindingLhs) -> IO Val
evalMethodWith env lm rewrites (_, lhs) = do
    expr0 <- Parser.parseBodyExprWithFixity
                (lmSource lm) (lmFixity lm) (lhsClauses lhs)
    let expr1 = desugarRecordPats (lmFieldReg lm)
                 (desugarRecordCons (lmFieldReg lm) expr0)
        expr  = if Map.null rewrites then expr1 else rewriteExpr rewrites expr1
    -- Evaluate the method expression to a Val in the global env.
    -- We need Eval here but can't import it (cycle). Use a thunk trick:
    -- make a thunk and force it immediately.
    t <- newThunk env expr
    force t

-- | Collect the union of free variables across all method bodies of an
-- instance.  Used to seed 'buildImportRewritesForNames' with the exact
-- set of names we need to resolve; restricting to actually-referenced
-- names keeps the rewrite walk bounded.
collectInstanceMethodFVs :: LoadedModule -> [(ByteString, BindingLhs)] -> IO (Set ByteString)
collectInstanceMethodFVs lm methods = do
    fvs <- mapM oneMethod methods
    pure (Set.fromList (concat fvs))
  where
    oneMethod (_, lhs) = do
        r <- try (Parser.parseBodyExprWithFixity
                    (lmSource lm) (lmFixity lm) (lhsClauses lhs))
                :: IO (Either SomeException Expr)
        case r of
            Right e -> pure (freeVars e)
            Left  _ -> pure []

-- | Variant of 'buildImportRewrites' that operates on a pre-computed
-- set of needed names (free vars of instance method bodies) instead of
-- the module's tied-knot bodies.  Same re-export chain walking logic.
buildImportRewritesForNames :: ModuleRegistry -> LoadedModule -> Set ByteString -> IO (Map ByteString ByteString)
buildImportRewritesForNames registry lm needed = do
    reg <- readIORef registry
    let imports = mhImports (lmHeader lm)
    importPairs <- concat <$> mapM (rewritesForImport reg needed) imports
    let filteredImportPairs = filter (\(n, _) -> not (Set.member n ffiBuiltinNames)) importPairs
    pure (Map.fromList filteredImportPairs)
  where
    rewritesForImport reg needed' imp
        | impModule imp == BC.pack "Prelude" = pure []
        | otherwise = case Map.lookup (impModule imp) reg of
            Just (Loaded tm) -> do
                let qualRef = case impAlias imp of
                        Just a  -> Just (a <> BC.pack ".")
                        Nothing
                            | impQualified imp -> Just (lmName tm <> BC.pack ".")
                            | otherwise        -> Nothing
                    requestedNames = instanceRequestedNames needed' imp qualRef
                if null requestedNames
                    then pure []
                    else do
                        directPairs <- instanceDirectPairs reg tm requestedNames
                        reexportPairs <- concat <$>
                            mapM (\m -> instanceReexportPairs reg m requestedNames)
                                 (moduleReexports (lmHeader tm))
                        let allPairs = directPairs ++ reexportPairs
                            visible  = filter (specAllows (impSpec imp) . fst) allPairs
                            bare | impQualified imp = []
                                 | otherwise = filter (\(n, _) -> Set.member n needed') visible
                            qual = case qualRef of
                                Just p  -> [ (p <> n, q)
                                           | (n, q) <- visible
                                           , Set.member (p <> n) needed'
                                           ]
                                Nothing -> []
                        pure (bare ++ qual)
            _ -> pure []

    instanceRequestedNames :: Set ByteString -> ImportDecl -> Maybe ByteString -> [ByteString]
    instanceRequestedNames needed' imp qualRef =
        nubBS
            [ bare
            | key <- Set.toList needed'
            , Just bare <- [bareForKey imp qualRef key]
            ]

    bareForKey imp qualRef key
        | not (impQualified imp)
        , specAllows (impSpec imp) key
        = Just key
        | otherwise =
            case (qualRef, splitQualified key) of
                (Just p, Just (qual, bare))
                    | p == qual <> BC.pack "."
                    , specAllows (impSpec imp) bare
                    -> Just bare
                _ -> Nothing

    instanceDirectPairs reg tm names = do
        bodiesMap <- readIORef (lmBodies tm)
        let prefix = lmName tm <> BC.pack "."
            localExported =
                [ n
                | n <- names
                , Just expr <- [Map.lookup n bodiesMap]
                , expr /= EVar n
                , exportsNameDirect tm n
                ]
            localPairs = [(n, prefix <> n) | n <- localExported]
        namedPairs <- instanceNamedReexportPairs reg tm bodiesMap names
        pure (localPairs ++ namedPairs)

    instanceNamedReexportPairs reg tm bodiesMap names =
        case mhExports (lmHeader tm) of
            ExportAll     -> pure []
            ExportList xs -> do
                let exportedNames = [ n | ExportName n <- xs, n `elem` names ]
                    missingNames  = filter (\n ->
                        case Map.lookup n bodiesMap of
                            Just expr -> expr == EVar n
                            Nothing   -> True
                        ) exportedNames
                concat <$> mapM (instanceFindInImports reg tm [lmName tm]) missingNames

    instanceFindInImports reg tm visited n = do
        let viaImports = mhImports (lmHeader tm)
            viable = filter (\i ->
                impModule i /= BC.pack "Prelude" &&
                impModule i `notElem` visited &&
                specAllows (impSpec i) n) viaImports
        go viable
      where
        go []         = pure []
        go (imp:rest) =
            case Map.lookup (impModule imp) reg of
                Just (Loaded srcLm) -> do
                    srcBodies <- readIORef (lmBodies srcLm)
                    if Map.member n srcBodies
                        then do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            pure [(n, srcPrefix <> n)]
                        else do
                            deeper <- instanceFindInImports reg srcLm (impModule imp : visited) n
                            case deeper of
                                [] -> go rest
                                ps -> pure ps
                _ -> go rest

    instanceReexportPairs reg modName names =
        case Map.lookup modName reg of
            Just (Loaded reLm) -> instanceDirectPairs reg reLm names
            _                  -> pure []

--------------------------------------------------------------------------------
-- Deriving Functor synthesis
--
-- For each @data T tv1 ... tvN ... deriving (... Functor ...)@ declaration
-- we synthesize a dictionary containing one method — @fmap@ — and
-- register it in the 'ClassRegistry' under class name @"Functor"@ and
-- under every constructor name of the type. The method's shape is:
--
-- @
--     fmap f x = case x of
--         C1 v1 v2 ...     -> C1 [role-apply roles v1 v2 ...]
--         C2 v1 v2 ...     -> C2 [role-apply roles v1 v2 ...]
--         ...
-- @
--
-- where @role-apply@ is determined by 'FunctorFieldRole' per position:
--   * 'FRVar' — field type is exactly the functor tyvar, apply @f@
--   * 'FRRec' — field type mentions the tyvar, apply @fmap f@
--             (recursive dispatch via the 'ClassRegistry' at runtime)
--   * 'FRNone' — untouched
--
-- Constructors whose tag doesn't match (e.g. sharing class registration
-- with another constructor of the same type) can still be fmap'd because
-- we register the full dispatch method under every ctor name of @T@.
--------------------------------------------------------------------------------

-- | Synthesise and register @Functor@ instances for every @deriving
-- Functor@ declaration found in the loaded modules. The instance Val is
-- keyed under the class name @"Functor"@ and under every constructor
-- name of the owning type (since 'typeTagOf' yields the ctor name at
-- runtime).
registerDerivedFunctorInstances :: ClassRegistry -> [LoadedModule] -> IO ()
registerDerivedFunctorInstances classReg loadedModules = do
    mapM_ oneModule loadedModules
  where
    oneModule lm = do
        decls <- scanFunctorDerivings (lmSource lm)
        mapM_ (registerOneFunctor classReg) decls

-- | Register one 'FunctorDerivDecl' as a Functor instance in the
-- registry. The instance's only method is @fmap@, built by
-- 'synthFmapForDecl' below. If the user wrote an explicit @instance
-- Functor T where ...@ it will have been registered first and this
-- derived synthesis is a no-op for that type (we don't overwrite).
registerOneFunctor :: ClassRegistry -> FunctorDerivDecl -> IO ()
registerOneFunctor classReg decl = do
    let fmapVal = synthFmapForDecl classReg decl
        methods = Map.singleton (BC.pack "fmap") fmapVal
        functorCls = BC.pack "Functor"
    existing <- lookupInstance classReg functorCls (fdTyName decl)
    case existing of
        Just _  -> pure ()    -- user-written instance already registered
        Nothing -> do
            registerInstance classReg functorCls (fdTyName decl) methods
            mapM_ (\c -> registerInstance classReg functorCls (fcName c) methods)
                  (fdCtors decl)

-- | Build the @fmap@ method Val for one derived-Functor type. The Val
-- takes @f@ (the mapping function) and @x@ (the container value), and
-- returns a new VCon whose fields are either untouched, the result of
-- @f field@, or the result of a recursive fmap dispatch.
synthFmapForDecl :: ClassRegistry -> FunctorDerivDecl -> Val
synthFmapForDecl classReg decl = VFun $ \fT -> pure $ VFun $ \xT -> do
    xv <- force xT
    case xv of
        VCon ctorName oldThunks -> do
            let roles = lookupCtorRoles ctorName decl oldThunks
            newThunks <- mapM (applyRoleOne classReg fT) (zip roles oldThunks)
            pure (VCon ctorName newThunks)
        other -> error
            ( "derived-Functor fmap: expected constructor value for "
              <> BC.unpack (fdTyName decl)
              <> ", got " <> shortShow other )

-- | Look up the roles for @ctorName@ in @decl@. If the ctor isn't listed
-- (e.g. arity mismatch), default to all-FRNone of the right length.
lookupCtorRoles :: ByteString -> FunctorDerivDecl -> [Thunk] -> [FunctorFieldRole]
lookupCtorRoles ctorName decl oldThunks =
    case [ fcRoles c | c <- fdCtors decl, fcName c == ctorName ] of
        (rs : _)
            | length rs == length oldThunks -> rs
            | otherwise ->
                -- Arity recorded in the scan differs from the runtime
                -- VCon's arity (shouldn't normally happen; play safe).
                replicate (length oldThunks) FRNone
        [] -> replicate (length oldThunks) FRNone

-- | Apply a field's role to one thunk, producing a new thunk.
applyRoleOne :: ClassRegistry -> Thunk -> (FunctorFieldRole, Thunk) -> IO Thunk
applyRoleOne _classReg _fT (FRNone, t) = pure t
applyRoleOne _classReg fT  (FRVar,  t) = do
    -- f field
    fv <- force fT
    v  <- apply fv t
    newWHNFThunk v
applyRoleOne classReg fT (FRRec, t) = do
    v <- force t
    case v of
        VCon innerTag _ -> do
            mInnerFmap <- lookupInstanceMethod classReg (BC.pack "Functor") innerTag (BC.pack "fmap")
            case mInnerFmap of
                Just innerFmap -> do
                    stepT <- newWHNFThunk v
                    r1 <- apply innerFmap fT
                    r2 <- apply r1 stepT
                    newWHNFThunk r2
                _ -> pure t   -- no Functor instance; leave field untouched
        _ -> pure t

shortShow :: Val -> String
shortShow (VCon n _) = "VCon " <> BC.unpack n
shortShow (VInt _)   = "VInt"
shortShow (VFloat _) = "VFloat"
shortShow (VChar _)  = "VChar"
shortShow (VStr _)   = "VStr"
shortShow _          = "<other>"

--------------------------------------------------------------------------------
-- User-defined class method dispatchers
--
-- For every @class C a where m1 :: ..., m2 :: ..., ...@ declaration we
-- synthesize a top-level binding @m_i = dispatcher C "m_i"@. The
-- dispatcher forces its first argument, looks up the
-- @(C, typeTagOf firstArg)@ entry in the ClassRegistry, and applies the
-- instance's method value to the original thunk (currying through any
-- remaining arguments). If no instance is registered for that type, it
-- falls back to the class-level default body (stored in the registry
-- under the sentinel type-tag "<default>").
--
-- Method names that collide with existing names in the base env
-- (e.g. the pre-registered 'show', '==' builtins, or an instance
-- method name already added through another class) are skipped so the
-- original behaviour is preserved.
--------------------------------------------------------------------------------

-- | Sentinel type tag used to store class-level default method bodies in
-- the ClassRegistry. A dispatcher falls back to this slot when no
-- instance is registered for the argument's type tag.
defaultTypeTag :: ByteString
defaultTypeTag = BC.pack "<default>"

-- | Build a dispatcher Val for a single class method.
--
-- @classMethodDispatcher reg cls methodName@ returns a VFun that,
-- when applied to its first argument, looks up the instance dict for
-- the argument's type tag and re-applies the named instance method.
-- Remaining arguments (if any) flow through naturally via
-- the returned VFun's own arity.
classMethodDispatcher :: ClassRegistry -> ByteString -> ByteString -> Val
classMethodDispatcher reg cls methodName = dispatch 4 []
  where
    -- @dispatch remaining accArgs@: try looking up an instance for the
    -- next argument.  If the argument isn't dispatchable (it's a
    -- function or unit), walk past it and retry on the next argument,
    -- up to @arityBudget@ arguments.  This handles Foldable methods
    -- like @foldr :: (a -> b -> b) -> b -> t a -> b@ whose container
    -- argument isn't the first one.
    dispatch :: Int -> [Thunk] -> Val
    dispatch remaining accArgs
        | remaining <= 0 = fallback accArgs
        | otherwise = VFun $ \argT -> do
            av <- force argT
            let tag = typeTagOf av
            if isDispatchableTag tag
                then do
                    mMethod <- lookupInstanceMethod reg cls tag methodName
                    case mMethod of
                        Just methodVal
                          | not (isMethodPlaceholder methodVal) ->
                                applyAll methodVal (reverse (argT : accArgs))
                        _ -> do
                            -- Dispatchable arg but no matching instance (or the
                            -- method is a placeholder) — fall back to the
                            -- class's default body for this method.
                            mDef <- lookupInstanceMethod reg cls defaultTypeTag methodName
                            case mDef of
                                Just defVal ->
                                    applyAll defVal (reverse (argT : accArgs))
                                _ -> error
                                    ( "class-method dispatch: no instance of `"
                                     <> BC.unpack cls
                                     <> "` for type `" <> BC.unpack tag
                                     <> "` (method `" <> BC.unpack methodName <> "`)" )
                else do
                    -- Non-dispatchable (function / unit / primitive
                    -- object): stash and wait for the next arg so the
                    -- dispatcher can look at a dispatchable argument
                    -- later in the application chain.
                    let v = dispatch (remaining - 1) (argT : accArgs)
                    pure v

    -- All args consumed without finding an instance; fall back to
    -- the class's default body, or error if there is none.
    fallback accArgs = VFun $ \finalArgT -> do
        mDef <- lookupInstanceMethod reg cls defaultTypeTag methodName
        case mDef of
            Just defVal ->
                applyAll defVal (reverse (finalArgT : accArgs))
            _ -> error
                ( "class-method dispatch: no dispatchable instance of `"
                 <> BC.unpack cls
                 <> "` for method `" <> BC.unpack methodName
                 <> "` (after trying " <> show (length accArgs + 1) <> " arguments)" )

    -- Apply a method Val to a list of pre-collected thunks, left-to-right.
    applyAll :: Val -> [Thunk] -> IO Val
    applyAll v []     = pure v
    applyAll v (t:ts) = do
        v' <- apply v t
        applyAll v' ts

    -- A tag like "<function>", "<IO>", "()" is not dispatchable: no
    -- type-class instance is registered under it.  Try later args.
    isDispatchableTag :: ByteString -> Bool
    isDispatchableTag t = t /= BC.pack "<function>"
                       && t /= BC.pack "<IO>"
                       && t /= BC.pack "()"
                       && not (BC.pack "<" `BC.isPrefixOf` t)

-- | Build an Env containing a dispatcher thunk for every method of every
-- user-defined class visible in any loaded module. Method-name collisions
-- with 'existing' (builtins, constructors, real bindings) are skipped —
-- those names already have a correct definition.
buildClassMethodEnv
    :: ClassRegistry
    -> Env            -- ^ env of names we should NOT shadow (e.g. builtins)
    -> [LoadedModule]
    -> IO Env
buildClassMethodEnv classReg existing loadedModules = do
    decls <- concat <$> mapM (\lm -> scanClassDecls (lmSource lm)) loadedModules
    -- Build each method as (name, thunk). Later entries overwrite earlier
    -- so a later class with the same method name "wins", but this only
    -- happens in pathological source; typical modules don't clash.
    pairs <- concat <$> mapM buildOne decls
    let filtered = [ p | p@(n, _) <- pairs, not (Map.member n existing) ]
    pure (Map.fromList filtered)
  where
    buildOne (ClassDecl cls methodNames _defaults) =
        mapM (mkMethodEntry cls) methodNames
    mkMethodEntry cls methodName = do
        let v = classMethodDispatcher classReg cls methodName
        t <- newWHNFThunk v
        pure (methodName, t)

-- | After the environment is fully tied, evaluate each class's default
-- method bodies in that env and register them in the ClassRegistry under
-- the sentinel type tag '<default>'. The dispatcher falls back to these
-- when no real instance is registered for a given type.
--
-- The default entry stores one name-keyed method table. Methods without a
-- default body get a placeholder Val that errors only if dispatched to.
registerClassDefaults :: ModuleRegistry -> [FilePath] -> Map FilePath [FilePath] -> ClassRegistry -> Env -> [LoadedModule] -> IO ()
registerClassDefaults registry searchPath includeMap classReg env loadedModules =
    mapM_ oneModule loadedModules
  where
    oneModule lm = do
        decls <- scanClassDecls (lmSource lm)
        mapM_ (oneClass lm) decls

    oneClass lm (ClassDecl cls methodNames defaults)
        | Map.null defaults = pure ()
        | otherwise = do
            -- Pre-demand-load each free var referenced in the class's
            -- default bodies so 'buildImportRewritesForNames' can
            -- rewrite them to their fully-qualified form before we try
            -- to evaluate.  Class defaults like @foldr f z t = appEndo
            -- (foldMap (Endo #. f) t) z@ rely on qualified imports
            -- (e.g. @Endo@ from @Data.Semigroup.Internal@, @#.@ from
            -- @Data.Functor.Utils@) that may not yet be demanded via
            -- the usual flow.
            fvs <- fmap (Set.fromList . concat) $
                mapM (\(_, lhs) -> do
                         r <- try (Parser.parseBodyExprWithFixity
                                     (lmSource lm) (lmFixity lm) (lhsClauses lhs))
                                 :: IO (Either SomeException Expr)
                         case r of
                             Right e -> pure (freeVars e)
                             Left  _ -> pure [])
                     (Map.toList defaults)
            mapM_ (\fv -> do
                       r <- try (discoverInModule registry searchPath includeMap lm fv)
                                :: IO (Either SomeException ())
                       case r of
                           Right () -> pure ()
                           Left  _  -> pure ())
                  (Set.toList fvs)
            rewrites <- buildImportRewritesForNames registry lm fvs
            vals <- Map.fromList <$> mapM (\methodName -> do
                        v <- slotVal lm cls defaults rewrites methodName
                        pure (methodName, v))
                    methodNames
            registerInstance classReg cls defaultTypeTag vals

    slotVal lm cls defaults rewrites methodName =
        case Map.lookup methodName defaults of
            Just lhs -> do
                r <- try (evalDefaultMethodWith env lm rewrites lhs)
                        :: IO (Either SomeException Val)
                case r of
                    Right v -> pure v
                    Left  _ -> pure (placeholder cls methodName)
            Nothing -> pure (placeholder cls methodName)

    placeholder cls methodName = VFun $ \_ -> error
        ( "class-method `" <> BC.unpack methodName
       <> "` of class `"   <> BC.unpack cls
       <> "`: no instance and no default implementation" )

evalDefaultMethodWith :: Env -> LoadedModule -> Map ByteString ByteString -> BindingLhs -> IO Val
evalDefaultMethodWith env lm rewrites lhs = do
    expr0 <- Parser.parseBodyExprWithFixity
                (lmSource lm) (lmFixity lm) (lhsClauses lhs)
    let expr1 = desugarRecordPats (lmFieldReg lm)
                 (desugarRecordCons (lmFieldReg lm) expr0)
        expr  = if Map.null rewrites then expr1 else rewriteExpr rewrites expr1
    t <- newThunk env expr
    force t

evalDefaultMethod :: Env -> LoadedModule -> BindingLhs -> IO Val
evalDefaultMethod env lm = evalDefaultMethodWith env lm Map.empty

-- | For each loaded module, read its collected bodies out of the
-- IORef and key them by either the unqualified local name (entry
-- module) or the fully-qualified @\"Module.name\"@ form (imported
-- modules). Expression references inside a non-entry module get
-- rewritten: any free-var mention of an imported name is replaced
-- with its fully-qualified form so the final flat environment can
-- resolve it without relying on per-module scopes.
exportBodies :: ModuleRegistry -> Set ByteString -> LoadedModule -> IO [(ByteString, Expr)]
exportBodies registry builtinNames lm = do
    bs <- readIORef (lmBodies lm)
    let keyPrefix | lmIsEntry lm = BC.empty
                  | otherwise    = lmName lm <> BC.pack "."
    rewrites <- buildImportRewrites registry lm builtinNames
    let transform e
            | lmIsEntry lm = e     -- entry keeps bare names; env has aliases
            | otherwise    = rewriteExpr rewrites e
    pure
        -- Filter out foreign-alias sentinels inserted by discoverInModule.
        -- A sentinel is an entry (n, EVar n) meaning "n resolved via import;
        -- no local body to export".
        -- For the ENTRY module, sentinels would create self-referential
        -- thunks (no rewrite applied), so we skip them.
        -- For NON-ENTRY modules, sentinels are useful: the rewrite pass
        -- transforms (EVar n) into (EVar "Fully.Qualified.n"), producing
        -- valid re-export aliases like "Data.List.length" -> "GHC.Internal.Data.List.length".
        [ (keyPrefix <> n, transform e)
        | (n, e) <- Map.toList bs
        , not (lmIsEntry lm && e == EVar n)   -- skip sentinels only for entry module
        ]

-- | Names of builtins that are FFI/primop-backed and should ALWAYS resolve
-- to the host builtin, never to source definitions. These are excluded from
-- import rewrites so that bare references hit the builtin in the flat env.
-- Only includes names that wrap C FFI calls or primops with no interpretable
-- Haskell source path.
ffiBuiltinNames :: Set ByteString
ffiBuiltinNames = Set.fromList
    [ "hPutBuf", "hGetBuf", "hPutBufNonBlocking", "hGetBufNonBlocking"
    , "withForeignPtr", "unsafeWithForeignPtr"
    , "mallocPlainForeignPtrBytes", "mallocForeignPtrBytes"
    , "peek", "poke", "peekByteOff", "pokeByteOff", "peekElemOff", "pokeElemOff"
    , "memcpy", "copyBytes"
    , "plusForeignPtr", "plusPtr", "minusPtr", "castPtr"
    , "stdout", "stdin", "stderr"  -- RTS pre-built handles
    ]

-- | Build a map from each locally-visible imported name to its
-- fully-qualified target key (as stored in the flat env).
buildImportRewrites :: ModuleRegistry -> LoadedModule -> Set ByteString -> IO (Map ByteString ByteString)
buildImportRewrites registry lm _builtinNames = do
    reg <- readIORef registry
    bodiesNow <- readIORef (lmBodies lm)
    let imports = mhImports (lmHeader lm)
        neededNames = Set.fromList
            [ fv
            | expr <- Map.elems bodiesNow
            , fv <- freeVars expr
            ]
    -- Self-rewrites: names defined in the current module itself need to be
    -- rewritten to "Module.name" so that cross-body references within the
    -- same non-entry module resolve correctly in the flat env.
    -- Without this, `packChars = unsafePackLenChars ...` would leave
    -- `unsafePackLenChars` as a bare name in the body, but the flat env
    -- only has it as "Data.ByteString.Internal.Type.unsafePackLenChars".
    selfPairs <- if lmIsEntry lm
        then pure []   -- entry module bodies keep bare names
        else do
            let prefix = lmName lm <> BC.pack "."
            -- Only include REAL local definitions (not foreign-alias sentinels).
            -- A sentinel is (n, EVar n) inserted by discoverInModule when a
            -- name was resolved via import. Such names should NOT be
            -- self-rewritten to "Module.n" — they live under their owning
            -- module's prefix in the flat env, not under this module's prefix.
            pure [ (n, prefix <> n)
                 | (n, expr) <- Map.toList bodiesNow
                 , BC.elem '.' n == False
                 , expr /= EVar n   -- skip foreign-alias sentinels
                 ]
    importPairs <- concat <$> mapM (rewritesForImport reg neededNames) imports
    -- Exclude FFI/primop builtins from import rewrites so bare references
    -- resolve to the host builtin rather than chasing source sentinel chains.
    let filteredImportPairs = filter (\(n, _) -> not (Set.member n ffiBuiltinNames)) importPairs
    -- Self-rewrites take lower priority than import-rewrites (an import
    -- that brings in the same name shadows the local self-rewrite).
    -- Data.Map.fromList keeps the LAST occurrence for duplicate keys, so
    -- selfPairs must come first and importPairs second for imports to win.
    pure (Map.fromList (selfPairs ++ filteredImportPairs))
  where
    rewritesForImport reg needed imp
        | impModule imp == BC.pack "Prelude" = pure []
        | otherwise = case Map.lookup (impModule imp) reg of
            Just (Loaded tm) -> do
                let qualRef = case impAlias imp of
                        Just a  -> Just (a <> BC.pack ".")
                        Nothing
                            | impQualified imp -> Just (lmName tm <> BC.pack ".")
                            | otherwise        -> Nothing
                    requestedNames = requestedNamesForImport needed imp qualRef
                if null requestedNames
                    then pure []
                    else do
                        -- Gather only the names this module body actually mentions.
                        directPairs <- directRewritePairs reg tm requestedNames
                        reexportPairs <- concat <$>
                            mapM (\m -> rewritePairsFromReexport reg m requestedNames)
                                 (moduleReexports (lmHeader tm))
                        let allPairs = directPairs ++ reexportPairs
                            visible  = filter (specAllows (impSpec imp) . fst) allPairs
                            bare | impQualified imp = []
                                 | otherwise = filter (needsBare needed) visible
                            qual = case qualRef of
                                Just p  -> [ (p <> n, q)
                                           | (n, q) <- visible
                                           , Set.member (p <> n) needed
                                           ]
                                Nothing -> []
                        pure (bare ++ qual)
            _ -> pure []

    requestedNamesForImport :: Set ByteString -> ImportDecl -> Maybe ByteString -> [ByteString]
    requestedNamesForImport needed imp qualRef =
        nubBS
            [ bare
            | key <- Set.toList needed
            , Just bare <- [neededBareName key]
            ]
      where
        neededBareName key
            | not (impQualified imp)
            , specAllows (impSpec imp) key
            = Just key
            | otherwise =
                case (qualRef, splitQualified key) of
                    (Just p, Just (qual, bare))
                        | p == qual <> BC.pack "."
                        , specAllows (impSpec imp) bare
                        -> Just bare
                    _ -> Nothing

    needsBare :: Set ByteString -> (ByteString, ByteString) -> Bool
    needsBare needed (n, _) = Set.member n needed

    -- | @(bare-name, fully-qualified-key)@ pairs for names exported by a
    -- module — including names re-exported from its own imports via
    -- @ExportName@ entries (the named re-export chain).
    directRewritePairs reg tm requestedNames = do
        bodiesMap <- readIORef (lmBodies tm)
        let prefix   = lmName tm <> BC.pack "."
            -- Names defined directly in tm and exported.
            localExported =
                [ n
                | n <- requestedNames
                , Just expr <- [Map.lookup n bodiesMap]
                , expr /= EVar n
                , exportsNameDirect tm n
                ]
            localPairs = [(n, prefix <> n) | n <- localExported]
        -- For ExportName entries not covered by local bodies, follow
        -- tm's own unqualified imports (named re-export chain).
        namedPairs <- namedReexportPairs reg tm bodiesMap requestedNames
        pure (localPairs ++ namedPairs)

    -- | Collect (name, qualified-key) pairs for ExportName entries that
    -- are not locally defined in @tm@ but are re-exported via @tm@'s
    -- own unqualified imports.
    namedReexportPairs reg tm bodiesMap requestedNames = do
        case mhExports (lmHeader tm) of
            ExportAll    -> pure []   -- ExportAll: no explicit name list to iterate
            ExportList xs -> do
                let exportedNames = [ n | ExportName n <- xs, n `elem` requestedNames ]
                    -- Names that appear in the export list but are NOT locally defined
                    -- must come from an import (named re-export chain).
                    missingNames  = filter (\n ->
                        case Map.lookup n bodiesMap of
                            Just expr -> expr == EVar n
                            Nothing   -> True
                        ) exportedNames
                concat <$> mapM (findNameInImports reg tm [lmName tm]) missingNames

    -- | Find which of @tm@'s unqualified imports provides @n@, returning
    -- a @(n, qualified-key)@ pair if found.
    findNameInImports reg tm visited n = do
        -- Walk all imports (qualified + unqualified).  The module's
        -- export list may re-export a qualified name (e.g. Prelude's
        -- @List.words@ where @List@ is @import qualified … as List@).
        -- The qualifier is stripped at ModuleHeader parse time so we
        -- must consider qualified imports as potential providers when
        -- building rewrite pairs.
        let viaImports = mhImports (lmHeader tm)
            viable = filter (\i ->
                impModule i /= BC.pack "Prelude" &&
                impModule i `notElem` visited &&
                specAllows (impSpec i) n) viaImports
        go viable
      where
        go []         = pure []
        go (imp:rest) =
            case Map.lookup (impModule imp) reg of
                Just (Loaded srcLm) -> do
                    srcBodies <- readIORef (lmBodies srcLm)
                    if Map.member n srcBodies
                        then do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            pure [(n, srcPrefix <> n)]
                        else do
                            -- Try one level deeper (srcLm might also re-export).
                            deeper <- findNameInImports reg srcLm (impModule imp : visited) n
                            case deeper of
                                [] -> go rest
                                ps -> pure ps
                _ -> go rest

    -- | @(bare-name, fully-qualified-key)@ pairs from a re-exported
    -- module (@module Foo@ in the export list).
    rewritePairsFromReexport reg modName requestedNames =
        case Map.lookup modName reg of
            Just (Loaded reLm) -> directRewritePairs reg reLm requestedNames
            _                  -> pure []

-- | Rewrite every free 'EVar' in @expr@ whose name appears in the
-- rewrite table (and isn't shadowed by an inner binder) to its
-- fully-qualified form. Entry-module bodies bypass this — they keep
-- their bare names and rely on the alias map in 'buildAliases'.
rewriteExpr :: Map ByteString ByteString -> Expr -> Expr
rewriteExpr rw = go []
  where
    go bound = \case
        EVar n
            | n `elem` bound -> EVar n
            | Just q <- Map.lookup n rw -> EVar q
            | otherwise -> EVar n
        e@(ELit _)  -> e
        EApp f x    -> EApp (go bound f) (go bound x)
        ELam n e    -> ELam n (go (n : bound) e)
        ELet bs e   ->
            let names  = map fst bs
                bound' = names ++ bound
                bs'    = [(n, go bound' b) | (n, b) <- bs]
            in ELet bs' (go bound' e)
        ECase s as  -> ECase (go bound s) (map (goAlt bound) as)
        EIf c t e   -> EIf (go bound c) (go bound t) (go bound e)
        EDo stmts   -> EDo (goStmts bound stmts)
        ENeg e      -> ENeg (go bound e)
        ETuple es   -> ETuple (map (go bound) es)
        ERecordCon n fields ->
            ERecordCon n [(fname, go bound e) | (fname, e) <- fields]
        ERecordWild n   -> ERecordWild n
        ERecordUpdate e fields ->
            ERecordUpdate (go bound e) [(fname, go bound fe) | (fname, fe) <- fields]
        EImplicitRef n  -> EImplicitRef n
        EImplicitLet bs e ->
            let names  = map fst bs
                bound' = names ++ bound
                bs'    = [(n, go bound' b) | (n, b) <- bs]
            in EImplicitLet bs' (go bound' e)
        ESplice inner   -> ESplice (go bound inner)
        EQuote inner    -> EQuote inner   -- Phase 2.12: body is not evaluated; no free vars to rename
        e@(ELabel _)    -> e   -- Phase 3.5: labels are self-contained
        ETyApp inner ty -> ETyApp (go bound inner) ty   -- value-level @T: recurse into inner expr

    goAlt bound (Alt p e) = Alt p (go (patBound p ++ bound) e)

    goStmts _     []                 = []
    goStmts bound (SExpr e   : rest) = SExpr (go bound e)
                                       : goStmts bound rest
    goStmts bound (SBind n e : rest) = SBind n (go bound e)
                                       : goStmts (n : bound) rest
    goStmts bound (SLet bs   : rest) =
        let names = map fst bs
            bound' = names ++ bound
            bs'   = [(n, go bound' b) | (n, b) <- bs]
        in SLet bs' : goStmts bound' rest

    patBound (PVar n)        = [n]
    patBound (PCon _ ps)     = concatMap patBound ps
    patBound (PAs n p)       = n : patBound p
    patBound (PBang p)       = patBound p
    patBound (PTuple ps)     = concatMap patBound ps
    patBound (PRecord _ fps) = concatMap (patBound . snd) fps
    patBound (PRecordWild _) = []
    patBound (PView _ p)     = patBound p
    patBound _               = []

-- | Build the alias environment for the entry module: every unqualified
-- import makes its bindings available under the local (bare) name in
-- the entry scope. Qualified imports contribute nothing here — their
-- references flow through 'splitQualified' + the fully-qualified key.
--
-- The returned env is unioned UNDER the full @qualEnv@ so that entry
-- bindings with the same local name (e.g. redefining a Prelude-style
-- function) shadow the import.
buildAliases
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]  -- ^ include-dirs map (unused here, threaded for API consistency)
    -> LoadedModule
    -> [Thunk]
    -> [(ByteString, Expr)]
    -> IO Env
buildAliases registry _searchPath _includeMap entry slots qualPairs = do
    -- Index qualPairs so we can look up "Module.name" -> Thunk fast.
    let thunkByKey = Map.fromList (zip (map fst qualPairs) slots)
    -- Build aliases for the entry module's imports.
    let entryImports = mhImports (lmHeader entry)
    entryPairs <- concat <$> mapM (aliasesForImport thunkByKey) entryImports
    -- Also build aliases for ALL loaded modules' qualified imports.
    -- Without this, qualified references like `List.length` inside
    -- Data.ByteString.Internal.Type (which does `import qualified Data.List as List`)
    -- would fail: the env contains `Data.List.length` but the code
    -- references `List.length`.
    reg <- readIORef registry
    let allModules = [ lm | (_, Loaded lm) <- Map.toList reg ]
    internalPairs <- concat <$> mapM (internalAliases thunkByKey) allModules
    pure (Map.fromList (internalPairs ++ entryPairs))
  where
    aliasesForImport thunkByKey imp = do
            -- We need to know which names the target module actually
            -- exports. We only have the loaded registry, so look it up.
            reg <- readIORef registry
            case Map.lookup (impModule imp) reg of
                Just (Loaded tm) -> do
                    -- Collect (owning-module-prefix, name) pairs from both
                    -- the target module's own bodies and any `module Foo`
                    -- re-exports in its export list.
                    directPairs <- namesFromModule thunkByKey tm
                    reexportPairs <- concat <$>
                        mapM (namesFromReexport reg thunkByKey)
                             (moduleReexports (lmHeader tm))
                    let allPairs = directPairs ++ reexportPairs
                        qualPrefix = case impAlias imp of
                            Just a  -> a <> BC.pack "."
                            Nothing
                                | impQualified imp -> lmName tm <> BC.pack "."
                                | otherwise        -> BC.empty
                        bareAliases
                            | impQualified imp = []
                            | otherwise =
                                [ (n, t) | (n, t) <- allPairs ]
                        qualAliases
                            | BC.null qualPrefix = []
                            | otherwise =
                                [ (qualPrefix <> n, t) | (n, t) <- allPairs ]
                    pure (bareAliases ++ qualAliases)
                _ -> pure []

    -- | Build qualified aliases for a single (non-entry) module's imports.
    -- E.g. if Data.ByteString.Internal.Type has `import qualified Data.List as List`,
    -- this produces `List.length -> <thunk for Data.List.length>`, etc.
    -- IMPORTANT: Only process qualified imports here. Non-qualified imports
    -- from non-entry modules would leak bare names into the global env,
    -- overriding builtins (e.g. Data.ByteString.Char8's `import Data.ByteString (length)`
    -- would override the builtin list `length`).
    internalAliases thunkByKey lm = do
        let imports = filter impQualified (mhImports (lmHeader lm))
        concat <$> mapM (aliasesForImport thunkByKey) imports

    -- | Return @(bare-name, Thunk)@ pairs for names exported by a loaded
    -- module — including names re-exported via ExportName from its imports.
    namesFromModule thunkByKey tm = do
        reg <- readIORef registry
        bodiesMap <- readIORef (lmBodies tm)
        let prefix   = lmName tm <> BC.pack "."
            allN     = Map.keys bodiesMap
            -- Names defined directly in tm and exported.
            localExported =
                [ n
                | n <- allN
                , Just expr <- [Map.lookup n bodiesMap]
                , expr /= EVar n
                , exportsNameDirect tm n
                ]
            localPairs = [ (n, t)
                         | n <- localExported
                         , Just t <- [Map.lookup (prefix <> n) thunkByKey]
                         ]
        -- Also follow ExportName re-exports via tm's imports.
        namedPairs <- namesFromNamedReexports reg thunkByKey tm bodiesMap
        pure (localPairs ++ namedPairs)

    -- | Collect (name, Thunk) pairs for ExportName entries that are not
    -- locally defined in @tm@ but are re-exported from @tm@'s imports.
    namesFromNamedReexports reg thunkByKey tm bodiesMap =
        case mhExports (lmHeader tm) of
            ExportAll    -> pure []
            ExportList xs -> do
                let exportedNames = [ n | ExportName n <- xs ]
                    missingNames  = filter (\n ->
                        case Map.lookup n bodiesMap of
                            Just expr -> expr == EVar n
                            Nothing   -> True
                        ) exportedNames
                pairs <- concat <$> mapM (findThunkInImports reg thunkByKey tm [lmName tm]) missingNames
                pure pairs

    -- | Find the Thunk for @n@ by walking @tm@'s unqualified imports.
    findThunkInImports reg thunkByKey tm visited n = do
        -- Walk all imports (qualified + unqualified) for the same reason
        -- as 'findNameInImports': a module may re-export a qualified
        -- name (@List.words@ in Prelude) whose definition lives behind
        -- a qualified import; the qualifier is already stripped by the
        -- ExportList parser so we treat qualified imports as potential
        -- providers too.
        let viaImports = mhImports (lmHeader tm)
            viable = filter (\i ->
                impModule i /= BC.pack "Prelude" &&
                impModule i `notElem` visited &&
                specAllows (impSpec i) n) viaImports
        go viable
      where
        go [] = pure []
        go (imp:rest) =
            case Map.lookup (impModule imp) reg of
                Just (Loaded srcLm) -> do
                    srcBodies <- readIORef (lmBodies srcLm)
                    if Map.member n srcBodies
                        then do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            case Map.lookup (srcPrefix <> n) thunkByKey of
                                Just t  -> pure [(n, t)]
                                Nothing -> go rest
                        else do
                            deeper <- findThunkInImports reg thunkByKey srcLm
                                (impModule imp : visited) n
                            case deeper of
                                [] -> go rest
                                ps -> pure ps
                _ -> go rest

    -- | Collect exported name-thunk pairs from a re-exported module
    -- (a @module Foo@ entry in an export list).
    namesFromReexport reg thunkByKey modName =
        case Map.lookup modName reg of
            Just (Loaded reLm) -> namesFromModule thunkByKey reLm
            _                  -> pure []

--------------------------------------------------------------------------------
-- Loading modules
--------------------------------------------------------------------------------

loadEntryModule :: ModuleRegistry -> Source -> IO LoadedModule
loadEntryModule registry src = do
    (mHeader, _) <- parseModuleHeader src startCursor
    let header0 = fromMaybe emptyHeader mHeader
        name    = fromMaybe (BC.pack "Main") (mhName header0)
        header  = addImplicitPrelude name src header0
    writeIORef registry (Map.singleton name Loading)
    lm <- buildLoadedModule name True header src
    modifyIORef' registry (Map.insert name (Loaded lm))
    pure lm

-- | Inject an implicit @import Prelude@ at the head of @mhImports@ unless:
--
--   * the module IS Prelude itself (the base Prelude module),
--   * the source already contains a @{-# LANGUAGE NoImplicitPrelude #-}@
--     pragma (opt-out), OR
--   * the user wrote an explicit @import Prelude@ / @import qualified Prelude@
--     in their source (any form of explicit import disables the implicit one,
--     matching GHC).
--
-- This mirrors Haskell 2010 / GHC behaviour: every module gets an implicit
-- @import Prelude@ unless it opts out.  The implicit import is prepended so
-- that later explicit imports can shadow/override it.
addImplicitPrelude :: ModuleName -> Source -> ModuleHeader -> ModuleHeader
addImplicitPrelude modName src hdr
    | modName == BC.pack "Prelude"        = hdr
    | hasNoImplicitPrelude src            = hdr
    | any isPreludeImport (mhImports hdr) = hdr
    | otherwise = hdr { mhImports = implicitPreludeImport : mhImports hdr }
  where
    isPreludeImport imp = impModule imp == BC.pack "Prelude"

implicitPreludeImport :: ImportDecl
implicitPreludeImport = ImportDecl
    { impModule    = BC.pack "Prelude"
    , impQualified = False
    , impAlias     = Nothing
    , impSpec      = ImportAll
    }

-- | Check whether the source bytes contain a @{-# LANGUAGE NoImplicitPrelude #-}@
-- pragma.  Used to opt out of the implicit Prelude injection.  This is a cheap
-- substring test; it does not try to parse pragmas properly (a LANGUAGE pragma
-- with multiple extensions including NoImplicitPrelude still matches).
hasNoImplicitPrelude :: Source -> Bool
hasNoImplicitPrelude src =
    BC.pack "NoImplicitPrelude" `BC.isInfixOf` srcBytes src

-- | Check whether the source bytes contain a @{-# LANGUAGE NoFieldSelectors #-}@
-- pragma.  Used to opt out of auto-synthesised top-level record-field
-- accessor functions.  Cheap substring test — same caveats as
-- 'hasNoImplicitPrelude'.  When this is on for a module, the field
-- accessors for record types declared in that module are NOT bound
-- under their bare names in the environment, so user-defined top-level
-- names can reuse them without collision.  Record-dot (@x.field@) still
-- works because the parser desugars it to a reference under the
-- internal 'fieldProjName' key, which is populated for every field
-- regardless of the pragma.
hasNoFieldSelectors :: Source -> Bool
hasNoFieldSelectors src =
    BC.pack "NoFieldSelectors" `BC.isInfixOf` srcBytes src

-- | Internal synthetic name used as the env key for record-dot field
-- access.  The parser's @x.field@ desugaring emits
-- @EApp (EVar (fieldProjName "field")) x@ so that record-dot continues
-- to work even when @{-# LANGUAGE NoFieldSelectors #-}@ suppresses the
-- bare-name accessor.  The @$fldProj$@ prefix is non-parseable as a
-- user identifier, so collision with user code is impossible.
fieldProjName :: ByteString -> ByteString
fieldProjName fname = BC.pack "$fldProj$" <> fname

-- | Partition a list of loaded modules' field registries into:
--
--   * @publicFields@: the union of field registries from modules whose
--     'lmNoFieldSelectors' is 'False' — these get bare-name accessor
--     bindings (the normal Haskell record-selector behaviour).
--   * @allFields@: the union of every module's field registry — this
--     is used both for record-dot desugaring (always populated under
--     the 'fieldProjName' prefix) and for the record-con / record-pat
--     desugarers.
--
-- The two maps differ only when at least one module opts out via
-- @NoFieldSelectors@.
partitionFieldRegistries
    :: [LoadedModule]
    -> (FieldRegistry, FieldRegistry)
partitionFieldRegistries lms =
    let allFields    = foldr Map.union Map.empty (map lmFieldReg lms)
        publicFields = foldr Map.union Map.empty
                         [ lmFieldReg lm | lm <- lms, not (lmNoFieldSelectors lm) ]
    in (publicFields, allFields)

-- | Build the combined field-accessor env: every field is bound under
-- its 'fieldProjName' prefix (for record-dot), and fields from modules
-- that do NOT have 'NoFieldSelectors' are also bound under their bare
-- name (so legacy @fname record@ application works).
buildFieldAccessorEnv :: FieldRegistry -> FieldRegistry -> IO Env
buildFieldAccessorEnv publicFields allFields = do
    -- Bare-name accessors only for non-NoFieldSelectors modules.
    bareEnv <- buildFieldEnv publicFields
    -- Internal record-dot accessors for every field.
    projEnv <- buildFieldEnv allFields
    let projKeyed = Map.mapKeys fieldProjName projEnv
    pure (Map.union bareEnv projKeyed)

-- | Locate, read, parse, and register a module. Returns the
-- 'LoadedModule' (and reuses a cached one on subsequent calls).
-- Cycles raise 'ImportCycle'.
loadModule
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]  -- ^ include-dirs map: srcDir -> includeDirs
    -> ModuleName
    -> IO LoadedModule
loadModule registry searchPath includeMap name = do
    reg <- readIORef registry
    case Map.lookup name reg of
        Just (Loaded lm) -> pure lm
        Just Loading     -> throwIO (ImportCycle name)
        Nothing -> do
            -- GHC.*, System.IO.Unsafe, Foreign.*, Data.Bits, etc. are
            -- intercepted as empty stubs because their names are provided
            -- directly by the builtin environment. Trying to parse their
            -- GHC-internal source would fail.
            if isBuiltinBackedModule name
                then do
                    lm <- buildEmptyStubModule name
                    modifyIORef' registry (Map.insert name (Loaded lm))
                    pure lm
                else do
                    path <- locateModule searchPath name
                    src0 <- readSourceFile path
                    -- Look up the package's include-dirs by matching the
                    -- file's directory against the includeMap.
                    let fileDir    = takeDirectory path
                        incDirs    = lookupIncludeDirs includeMap fileDir
                    src  <- cppSourceWithIncludes incDirs src0
                    (mHeader, _) <- parseModuleHeader src startCursor
                    let header = fromMaybe emptyHeader mHeader
                        declared = fromMaybe name (mhName header)
                    modifyIORef' registry (Map.insert name Loading)
                    lm <- buildLoadedModule declared False header src
                    modifyIORef' registry (Map.insert name (Loaded lm))
                    pure lm

-- | Look up the include-dirs for a source file by matching its directory
-- against the keys of the includeMap.  The file may live inside a
-- subdirectory of the recorded source root (e.g. the source root is
-- @bytestring-0.12.2.0/@ but the file is in @.../Data/ByteString/@), so
-- we check every key as a prefix of @fileDir@.
lookupIncludeDirs :: Map FilePath [FilePath] -> FilePath -> [FilePath]
lookupIncludeDirs includeMap fileDir =
    -- Walk all keys; return the first set of include-dirs whose source-dir
    -- is a prefix of (or equal to) fileDir.
    case [ dirs | (srcDir, dirs) <- Map.toList includeMap
                , srcDir `isPrefixOf` fileDir
                , not (null dirs)
                ] of
        (dirs : _) -> dirs
        []         -> []

-- | Phase 2.17: Minimal compiler-builtins-only whitelist.
--
-- ONLY modules with NO .hs source file in base-4.19.0.0 stay here.
-- These are generated or wired-in by the GHC build system itself;
-- they cannot be source-loaded because they literally do not exist
-- as Haskell text on disk.
--
-- Verification: checked against ~/.cache/ihc/sources/base-4.19.0.0/
-- for each entry below.  Every module with a .hs file has been
-- REMOVED from this list and will be source-loaded instead.
--
-- The 17-clause original whitelist covered modules that DO have
-- source (GHC.Base, GHC.IO, Prelude, System.IO, Foreign.*, etc.).
-- Those are now source-loaded, exposing gaps in the interpreter that
-- the Phase 2.17 punchlist documents.
isBuiltinBackedModule :: ModuleName -> Bool
isBuiltinBackedModule n =
    -- GHC.Prim: no source; all primops are wired-in by the GHC
    -- compiler itself (primops.txt.pp → GHC.Prim at build time).
       n == "GHC.Prim"
    -- GHC.Types: wired-in kinds, Constraint, RuntimeRep, Int#, etc.
    -- The compiler synthesises this module; base-4.19 has no GHC/Types.hs.
    || n == "GHC.Types"
    -- GHC.Magic: inline/noinline/lazy/oneShot etc. — compiler magic.
    || n == "GHC.Magic"
    -- GHC.Magic.Dict: withDict — compiler magic, no source.
    || n == "GHC.Magic.Dict"
    -- GHC.CString: unpackCString# and friends — wired-in string literals.
    || n == "GHC.CString"
    -- GHC.Classes: Eq/Ord/Bool/not etc. — wired-in class hierarchy.
    -- GHC generates instances for (->), tuples, etc. internally.
    || n == "GHC.Classes"
    -- GHC.Tuple: wired-in tuple types ((), (,), (,,), …).
    || n == "GHC.Tuple"
    -- GHC.Prim.PrimOpWrappers: auto-generated by GHC build from primops.txt.
    || n == "GHC.Prim.PrimOpWrappers"
    -- GHC.Prim.Ext: extra primops not in primops.txt; generated by GHC build.
    || n == "GHC.Prim.Ext"
    -- GHC.Prim.Exception: raiseIO# etc. — generated primop wrappers.
    || n == "GHC.Prim.Exception"
    -- GHC.RTS.Flags: RTS runtime flags; generated from RtsFlags.c, no .hs source.
    || n == "GHC.RTS.Flags"
    -- GHC.Integer.Type: GMP integer internals; generated by GHC/integer-gmp build.
    || n == "GHC.Integer.Type"
    -- Unsafe.Coerce: has .hs source, but the source defines unsafeCoerce in
    -- terms of `unsafeEqualityProof`, which GHC rewrites at CoreToStg.Prep
    -- time — the untransformed source body `case unsafeEqualityProof of
    -- UnsafeRefl -> UnsafeRefl` diverges (see Note [Implementing
    -- unsafeCoerce] (U5) in base's Unsafe/Coerce.hs). unsafeCoerce is
    -- therefore compiler-intrinsic and must be host-backed; the builtin
    -- env provides it as identity-on-Val.
    || n == "Unsafe.Coerce"
    -- Language.Haskell.TH.*: template-haskell package; IHC.TH provides synthetic
    -- builtins for splice execution.  The package is not in the base cache.
    || "Language.Haskell.TH" `BC.isPrefixOf` n

-- | Emit a diagnostic to stderr when a missing module is being
-- substituted with an empty stub. Keeps the first 3 search-path
-- entries for context (the full list is usually dozens of cached
-- package dirs). Silenceable via @IHC_WARN_STUBS=0@.
warnMissingStub :: ModuleName -> [FilePath] -> IO ()
warnMissingStub name searchPath =
    let shown = take 3 searchPath
        suffix
            | length searchPath > 3 =
                " (+" <> show (length searchPath - 3) <> " more)"
            | otherwise = ""
        pathStr = case shown of
            [] -> "<empty>"
            _  -> show shown <> suffix
    in warnStub
        ("module " <> BC.unpack name
         <> " could not be located under " <> pathStr
         <> "; substituting empty stub")

buildEmptyStubModule :: ModuleName -> IO LoadedModule
buildEmptyStubModule name = do
    known  <- emptyKnownSymbols
    bodies <- newIORef Map.empty
    let src = mkSource (BC.unpack name) ""
    pure LoadedModule
        { lmName        = name
        , lmHeader      = ModuleHeader (Just name) ExportAll []
        , lmSource      = src
        , lmKnown       = known
        , lmDataReg     = Map.empty
        , lmFieldReg    = Map.empty
        , lmTypeCtorReg = Map.empty
        , lmBodies      = bodies
        , lmIsEntry     = False
        , lmFixity      = defaultFixityTable
        , lmNoFieldSelectors = False
        , lmTypeFamilies     = TR.emptyRegistry
        }

buildLoadedModule :: ModuleName -> Bool -> ModuleHeader -> Source -> IO LoadedModule
buildLoadedModule name isEntry header src = do
    known               <- emptyKnownSymbols
    (dataR, fldR, tCtR) <- scanDataDecls src
    tfReg               <- scanTypeFamilyDecls src
    bodies              <- newIORef Map.empty
    fixity              <- scanFixityDecls src defaultFixityTable
    pure LoadedModule
        { lmName        = name
        , lmHeader      = header
        , lmSource      = src
        , lmKnown       = known
        , lmDataReg     = dataR
        , lmFieldReg    = fldR
        , lmTypeCtorReg = tCtR
        , lmBodies      = bodies
        , lmIsEntry     = isEntry
        , lmFixity      = fixity
        , lmNoFieldSelectors = hasNoFieldSelectors src
        , lmTypeFamilies     = tfReg
        }

emptyHeader :: ModuleHeader
emptyHeader = ModuleHeader Nothing ExportAll []

-- | Given a dotted module name, search each entry in @searchPath@ for
-- a matching file.  The @searchPath@ should already include the cached
-- package source directories (see 'cachedPackageSearchPath' from
-- 'IHC.CabalProject', which is prepended by the public entry points
-- 'loadProgramFromSource' and 'loadImportIntoEnv').  Raises
-- 'ModuleNotFound' only when the entire search path misses.
locateModule :: [FilePath] -> ModuleName -> IO FilePath
locateModule searchPath name = go searchPath
  where
    candidates = modulePathCandidates name
    go []     = throwIO (ModuleNotFound name)
    go (d:ds) = tryCands d candidates ds
    tryCands _ []     rest = go rest
    tryCands d (c:cs) rest = do
        let p = d </> c
        exists <- doesFileExist p
        if exists then pure p else tryCands d cs rest

-- | Return 'True' if @name@ is safe to load: either it's a local (non-cache)
-- file OR it's a cache file that is NOT inside the GHC-internal subdirectory
-- of base (@GHC.*@).  GHC.* modules form a huge transitive web that causes
-- OOM when eagerly explored; all other cache modules (HUnit, hspec, standard
-- base modules like Control.Monad, Data.List, etc.) are small enough that
-- demand-driven discovery stays bounded.
--
-- Modules that ARE blocked by this guard can still be loaded when they are
-- already in the registry (a previous load put them there) or when the
-- import has an explicit name list (ImportOnly).
isLocalCacheModule :: [FilePath] -> ModuleName -> IO Bool
isLocalCacheModule searchPath name = do
    -- Bare GHC.* modules (GHC.Base, GHC.Num, etc.) are blocked: they form a
    -- huge transitive web that can cause OOM when eagerly explored.
    -- GHC.Internal.* are allowed: they contain the real definitions for standard
    -- library types (ST, STRef, etc.) and we load them demand-driven.
    if ("GHC." `BC.isPrefixOf` name || name == "GHC")
        && not ("GHC.Internal." `BC.isPrefixOf` name)
        then pure False
        else do
            -- Locate the module to check if it's a local file.
            mPath <- (Just <$> locateModule searchPath name)
                        `catch` (\(_ :: ModuleNotFound) -> pure Nothing)
            case mPath of
                Nothing -> pure False
                Just _  -> pure True   -- found anywhere is OK (we blocked bare GHC.* above)

--------------------------------------------------------------------------------
-- Class / instance body free-var discovery
--------------------------------------------------------------------------------

-- | Walk every loaded module's class default-method bodies and instance
-- method bodies, parse each one, collect free variables, and drive
-- 'discoverInModule' for every free var so the names are pre-loaded
-- before 'registerInstancesFrom' / 'registerClassDefaults' evaluate the
-- bodies against the tied env.
--
-- This fills the gap left by demand-driven discovery, which only walks
-- free vars reachable from @main@. Class/instance method bodies are not
-- reachable from @main@ via the expression-tree walk — they live in
-- metadata the scheduler only inspects at registration time.
discoverClassAndInstanceFreeVars
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> IO ()
discoverClassAndInstanceFreeVars registry searchPath includeMap = do
    reg <- readIORef registry
    let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]
    mapM_ discoverOne loadedModules
  where
    discoverOne lm = do
        -- Instance method bodies.
        instDecls  <- scanInstanceDecls (lmSource lm)
        mapM_ (discoverMethods lm)
              [ (n, lhs) | InstanceDecl _ _ _ ms <- instDecls, (n, lhs) <- ms ]
        -- Class default-method bodies.
        classDecls <- scanClassDecls (lmSource lm)
        mapM_ (discoverMethods lm)
              [ (n, lhs)
              | ClassDecl _ _ defs <- classDecls
              , (n, lhs) <- Map.toList defs
              ]

    discoverMethods lm (_, lhs) = do
        r <- try (Parser.parseBodyExprWithFixity (lmSource lm) (lmFixity lm)
                     (lhsClauses lhs))
                :: IO (Either SomeException Expr)
        case r of
            Left _     -> pure ()
            Right expr -> mapM_ (discoverFree lm) (freeVars expr)

    discoverFree lm name = do
        r <- try (discoverInModule registry searchPath includeMap lm name)
                :: IO (Either SomeException ())
        case r of
            Right () -> pure ()
            Left  _  -> pure ()   -- tolerate unresolved names

--------------------------------------------------------------------------------
-- Demand-driven discovery
--------------------------------------------------------------------------------

-- | Recursively discover bindings reachable from @name@ inside the
-- given module, following imports whenever the name isn't local.
discoverInModule
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]  -- ^ include-dirs map: srcDir -> includeDirs
    -> LoadedModule
    -> ByteString
    -> IO ()
discoverInModule = discoverInModuleWith Set.empty

-- | Variant of 'discoverInModule' that accepts a set of known builtin names
-- so that the demand-driven resolver can skip the Prelude source walk for
-- names that the evaluator can already resolve via the host builtin env.
-- This is the hot path: without it, every mention of @putStrLn@, @print@,
-- @+@, etc. would trigger a cascading load of @Prelude -> GHC.Internal.*@
-- subgraphs, hurting startup latency (sometimes catastrophically).
discoverInModuleWith
    :: Set ByteString          -- ^ builtin names to short-circuit on
    -> ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> ByteString
    -> IO ()
discoverInModuleWith builtins registry searchPath includeMap lm name
    -- Qualified name (contains a dot and the prefix looks like a module
    -- alias)? Route to the target module directly.
    | Just (qual, bareName) <- splitQualified name = do
        mTarget <- resolveQualified registry searchPath includeMap lm qual
        case mTarget of
            Just targetLm -> do
                discoverInModuleWith builtins registry searchPath includeMap targetLm bareName
                -- Check if the target module actually resolved the name.
                -- If its bodies contain bareName, the standard qualified key
                -- (e.g. "Data.List.length") will exist in the flat env.
                -- If not (e.g. class methods like 'length' from Foldable that
                -- can't be traced through re-exports), fall back to the bare
                -- name which may be resolved as a builtin/Prelude binding.
                targetBodies <- readIORef (lmBodies targetLm)
                let fqn = lmName targetLm <> BC.pack "." <> bareName
                    rhs = if Map.member bareName targetBodies
                            then EVar fqn
                            else EVar bareName  -- fallback to bare name (builtin/Prelude)
                modifyIORef' (lmBodies lm) (Map.insert name rhs)
            Nothing ->
                throwIO (UnresolvedName
                    ("qualified name " <> BC.unpack name
                     <> " — no matching import in module "
                     <> BC.unpack (lmName lm)))
    | otherwise = do
        bodies <- readIORef (lmBodies lm)
        if Map.member name bodies
            then pure ()
            else do
                mLhs <- findOrResolveLhs (lmSource lm) (lmKnown lm) name
                case mLhs of
                    Just lhs -> do
                        -- ParseError: primop-pattern bindings (e.g. `ord (C# c#)`)
                        -- are not yet supported.  Skip them and fall through to
                        -- import resolution so the evaluator can still find the
                        -- name via a re-export.
                        mExpr <- (Just <$> Parser.parseBodyExprWithFixity
                                            (lmSource lm)
                                            (lmFixity lm)
                                            (lhsClauses lhs))
                                    `catch` (\(_ :: ParseError) -> pure Nothing)
                        case mExpr of
                            Nothing
                                | Set.member name builtins ->
                                    pure ()
                                | otherwise -> do
                                    mForeign <- resolveImport registry searchPath includeMap lm name
                                    case mForeign of
                                        Just () ->
                                            modifyIORef' (lmBodies lm) (Map.insert name (EVar name))
                                        Nothing ->
                                            pure ()
                            Just expr0 -> do
                                let expr = desugarRecordPats (lmFieldReg lm)
                                             (desugarRecordCons (lmFieldReg lm) expr0)
                                modifyIORef' (lmBodies lm) (Map.insert name expr)
                                -- Recurse into every free var. Qualified ones
                                -- will be routed on the next call.
                                -- Deduplicate to avoid O(n^2) re-traversal when a
                                -- name appears many times in one binding.
                                -- ModuleNotFound for transitive deps (e.g. a missing
                                -- package like `array`) is silently swallowed: the
                                -- missing name is treated as a builtin and the
                                -- evaluator will complain if it is actually used.
                                let discoverFreeVar fv =
                                      discoverInModuleWith builtins registry searchPath includeMap lm fv
                                        `catch` (\(_ :: ModuleNotFound) -> pure ())
                                        `catch` (\(_ :: ParseError)     -> pure ())
                                mapM_ discoverFreeVar (nubBS (freeVars expr))
                    Nothing
                        -- Names provided by IHC.Builtins resolve to the host
                        -- builtin env — no need to walk the source re-export
                        -- chain through Prelude.  Skipping this load is what
                        -- makes implicit Prelude tractable for programs that
                        -- only use builtin names (the common case).
                        | Set.member name builtins ->
                            pure ()
                        | otherwise -> do
                            -- Not local. Try imports.
                            mForeign <- resolveImport registry searchPath includeMap lm name
                            case mForeign of
                                Just () ->
                                    -- Memoize: mark this name as handled in the
                                    -- current module's bodies so that repeated
                                    -- calls to discoverInModule for the same
                                    -- (module, name) pair short-circuit
                                    -- immediately without re-traversing imports.
                                    -- EVar name acts as a "foreign alias" sentinel.
                                    modifyIORef' (lmBodies lm) (Map.insert name (EVar name))
                                Nothing ->
                                    -- Assume a builtin; let the evaluator
                                    -- complain if truly missing.
                                    pure ()

-- | Look up @B@ in the module's imports and return the module it refers
-- to. Matches on alias when qualified is declared, otherwise on the
-- module name itself.
resolveQualified
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]  -- ^ include-dirs map
    -> LoadedModule
    -> ByteString
    -> IO (Maybe LoadedModule)
resolveQualified registry searchPath includeMap lm qual = do
    let imports = mhImports (lmHeader lm)
    case filter (importMatchesQual qual) imports of
        (imp:_) -> Just <$> loadModule registry searchPath includeMap (impModule imp)
        []      -> pure Nothing

importMatchesQual :: ByteString -> ImportDecl -> Bool
importMatchesQual qual imp =
    case impAlias imp of
        Just a  -> a == qual
        Nothing -> impModule imp == qual

-- | Try to satisfy an unqualified free var via one of the module's
-- imports. Returns 'Just ()' if one of the imports claims the name,
-- otherwise 'Nothing'. When a match is found, we recurse into that
-- foreign module to pull the binding's RHS in.
resolveImport
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]  -- ^ include-dirs map
    -> LoadedModule
    -> ByteString
    -> IO (Maybe ())
resolveImport registry searchPath includeMap lm name = do
    -- Only unqualified (non-qualified-import) imports can provide
    -- unqualified names.
    let imports = filter (not . impQualified) (mhImports (lmHeader lm))
    tryImports imports
  where
    tryImports [] = pure Nothing
    tryImports (imp:rest)
        | not (specAllows (impSpec imp) name) = tryImports rest
        | otherwise = do
            -- Load-guard: don't eagerly load cache modules that could pull in
            -- large transitive dependency subgraphs (GHC.Base and friends).
            -- We allow loading when:
            --  1. The module is already in the registry (previously loaded).
            --  2. The import is explicitly targeted: `import Foo (bar)` —
            --     ImportOnly means the user asked for exactly these names.
            --  3. The module is a local (non-cache) file: small and safe.
            --
            -- For ImportAll cache imports like `import Control.Monad`, we
            -- only allow loading if already in the registry.  This prevents
            -- GHC.Base and other huge base modules from being loaded
            -- transitively and causing OOM.
            reg <- readIORef registry
            let alreadyLoaded = Map.member (impModule imp) reg
            let importIsTargeted = case impSpec imp of
                    ImportOnly _ -> True
                    _            -> False
            shouldLoad <- if alreadyLoaded || importIsTargeted
                then pure True
                else isLocalCacheModule searchPath (impModule imp)
            if not shouldLoad
                then do
                    tryImports rest
                else do
                    mTargetLm <- (Just <$> loadModule registry searchPath includeMap (impModule imp))
                                    `catch` (\(_ :: ModuleNotFound) -> pure Nothing)
                    case mTargetLm of
                        Nothing       -> tryImports rest
                        Just targetLm -> do
                            mLhs <- findOrResolveLhs (lmSource targetLm)
                                                     (lmKnown targetLm) name
                            case mLhs of
                                Just _ ->
                                    if exportsName targetLm name
                                        then do
                                            discoverInModule registry searchPath includeMap targetLm name
                                            pure (Just ())
                                        else tryImports rest
                                Nothing ->
                                    -- The name isn't defined locally in targetLm.
                                    -- Two ways it might still be exported:
                                    --
                                    -- 1. `module Foo` re-export entry — follow those.
                                    -- 2. `ExportName n` re-export via unqualified import:
                                    --    e.g. `Data.Map.Strict` lists `fromList` in its
                                    --    export list but the definition lives in
                                    --    `Data.Map.Internal`.  Follow all unqualified
                                    --    imports of @targetLm@ to find the definition.
                                    if exportsName targetLm name
                                        then followNamedReexport targetLm rest
                                        else followModuleReexports targetLm rest

    -- | @targetLm@ exports @name@ by name (ExportName entry) but doesn't
    -- define it locally.  Walk @targetLm@'s own unqualified imports and
    -- recurse into each until we find the definition.
    --
    -- This is the key piece of the named re-export chain: when a module
    -- @M@ has @ExportName n@ in its export list but no local definition
    -- of @n@, the name must come from one of @M@'s unqualified imports.
    -- We walk those imports, recursively following further named
    -- re-exports, until we find the module that actually defines @n@.
    --
    -- Real-world example (aeson):
    --
    -- @
    --   module Data.Aeson.Encoding (encodingToLazyByteString, ...) where
    --   import Data.Aeson.Encoding.Internal   -- defines encodingToLazyByteString
    -- @
    --
    -- The export list uses @ExportName "encodingToLazyByteString"@, not a
    -- @module Data.Aeson.Encoding.Internal@ re-export, so the
    -- 'followModuleReexports' path alone would miss it.
    --
    -- @depth@ limits how many import-levels we traverse.  Depth 3 covers
    -- three-hop chains (A → B → C → D where D defines the name), which
    -- is enough for typical base/aeson/bytestring style gateway modules
    -- without blowing up loading entire dependency subgraphs when a name
    -- simply isn't there.
    followNamedReexport via rest = do
        followNamedReexportD (3 :: Int) via rest

    followNamedReexportD depth via rest
        | depth <= (0 :: Int) = do
            tryImports rest
        | otherwise = do
            -- First try module-form re-exports (they may also apply).
            let reexportedMods = filter (/= lmName via)
                               $ moduleReexports (lmHeader via)
            r <- tryReexports [lmName via] reexportedMods rest
            case r of
                Just () -> pure (Just ())
                Nothing -> do
                    -- Follow the via module's imports.  We include
                    -- QUALIFIED imports as well as unqualified ones because
                    -- the export list may re-export a qualified name:
                    -- e.g. Prelude has @List.words@ in its export list where
                    -- @List@ is @import qualified GHC.Internal.Data.List as
                    -- List@.  The qualifier is stripped by the ExportList
                    -- parser (we only record @ExportName "words"@), so at
                    -- resolve time we must be willing to walk qualified
                    -- imports too — discovery is already demand-driven per
                    -- name, so there's no broad-load cost.
                    let viaImports = mhImports (lmHeader via)
                        filteredImports = filter (\i ->
                            impModule i /= BC.pack "Prelude" &&
                            specAllows (impSpec i) name) viaImports
                    tryViaImports filteredImports depth rest

    tryViaImports [] _depth rest = do
        tryImports rest
    tryViaImports (imp:moreImps) depth rest = do
        -- Only load the via-module if it is already in the registry.
        -- This prevents eagerly loading large library dependency subgraphs
        -- (e.g., GHC.Base) just to search for a re-exported name.
        -- If the module is not yet loaded, we fall through to the next import.
        --
        -- GHC.Internal.* modules are allowed when doing targeted searches
        -- because they hold the real definitions for standard library types
        -- (e.g. GHC.Internal.ST contains runST, GHC.Internal.STRef contains
        -- newSTRef etc.) and discoverInModule is demand-driven — it only
        -- resolves the specific binding we're looking for, not the whole file.
        -- Bare GHC.* modules (GHC.Base, GHC.Num, etc.) are still blocked to
        -- avoid pulling in unimplemented primops at bulk load time.
        reg <- readIORef registry
        let isBlockedGhc = ("GHC." `BC.isPrefixOf` impModule imp || impModule imp == "GHC")
                        && not ("GHC.Internal." `BC.isPrefixOf` impModule imp)
        case Map.lookup (impModule imp) reg of
            Just (Loaded srcLm) -> do
                mLhs  <- findOrResolveLhs (lmSource srcLm) (lmKnown srcLm) name
                case mLhs of
                    Just _ ->
                        if exportsName srcLm name
                            then do
                                discoverInModule registry searchPath includeMap srcLm name
                                pure (Just ())
                            else tryViaImports moreImps depth rest
                    Nothing ->
                        -- srcLm might itself re-export via unqualified imports
                        -- (go one level deeper, decrementing the depth cap).
                        if exportsName srcLm name
                            then followNamedReexportD (depth - 1) srcLm rest >>= \case
                                    Just () -> pure (Just ())
                                    Nothing -> tryViaImports moreImps depth rest
                            else tryViaImports moreImps depth rest
            _ ->
                -- Module not yet loaded.
                -- Block bare GHC.* modules (GHC.Base, GHC.Num, etc.) to avoid
                -- pulling in unimplemented primops at bulk load time.
                -- GHC.Internal.* are allowed — they hold the real definitions.
                if isBlockedGhc
                    then tryViaImports moreImps depth rest
                    else do
                -- Use a targeted search (findOrResolveLhs) which only parses
                -- the specific binding, so loading a cache module here is safe
                -- even for large libraries — we never bulk-parse the whole file.
                    r <- try (loadModule registry searchPath includeMap (impModule imp))
                                :: IO (Either SomeException LoadedModule)
                    case r of
                        Left  _     -> tryViaImports moreImps depth rest
                        Right srcLm -> do
                            mLhs  <- findOrResolveLhs (lmSource srcLm) (lmKnown srcLm) name
                            case mLhs of
                                Just _ ->
                                    if exportsName srcLm name
                                        then do
                                            discoverInModule registry searchPath includeMap srcLm name
                                            pure (Just ())
                                        else tryViaImports moreImps depth rest
                                Nothing ->
                                    -- Not defined directly in srcLm; recurse one
                                    -- level deeper if srcLm exports the name via
                                    -- its own imports (limited by depth).
                                    if exportsName srcLm name && depth > 1
                                        then followNamedReexportD (depth - 1) srcLm rest >>= \case
                                                Just () -> pure (Just ())
                                                Nothing -> tryViaImports moreImps depth rest
                                        else tryViaImports moreImps depth rest

    -- | Chase every `module Foo` entry in the export list of @via@ to
    -- see whether any of them provides @name@.  We recurse through
    -- 'discoverInModule' so the transitive load chain is followed
    -- automatically.  @visited@ prevents infinite loops when modules
    -- have circular re-export chains (e.g. A re-exports B, B re-exports A).
    followModuleReexports via rest =
        followModuleReexportsV [lmName via] via rest

    followModuleReexportsV visited via rest = do
        let reexportedMods = filter (`notElem` visited)
                           $ moduleReexports (lmHeader via)
        tryReexports visited reexportedMods rest

    tryReexports _       [] rest = tryImports rest
    tryReexports visited (modName:mods) rest = do
        -- Skip bare GHC.* modules unless already loaded: following their
        -- re-export chains may pull in large subgraphs.
        -- GHC.Internal.* are allowed because they contain real definitions
        -- (e.g. ST, STRef) and discoverInModule is demand-driven.
        reg <- readIORef registry
        let alreadyLoaded  = Map.member modName reg
        let isBlockedGhc   = ("GHC." `BC.isPrefixOf` modName || modName == "GHC")
                          && not ("GHC.Internal." `BC.isPrefixOf` modName)
        if not alreadyLoaded && isBlockedGhc
            then tryReexports visited mods rest
            else do
                -- Silently skip modules that can't be found (e.g. missing packages
                -- like `array` that aren't in the search path).
                mReLm <- (Just <$> loadModule registry searchPath includeMap modName)
                            `catch` (\(_ :: ModuleNotFound) -> pure Nothing)
                case mReLm of
                    Nothing   -> tryReexports visited mods rest
                    Just reLm -> do
                        mLhs <- findOrResolveLhs (lmSource reLm) (lmKnown reLm) name
                        case mLhs of
                            Just _ ->
                                if exportsName reLm name
                                    then do
                                        r <- try (discoverInModule registry searchPath includeMap reLm name) :: IO (Either SomeException ())
                                        case r of
                                            Right () -> pure (Just ())
                                            Left  _  -> tryReexports visited mods rest
                                    else tryReexports visited mods rest
                            Nothing ->
                                -- Go one level deeper if reLm itself has module re-exports.
                                -- Mark modName as visited to prevent cycles.
                                followModuleReexportsV (modName : visited) reLm [] >>= \case
                                    Just ()  -> pure (Just ())
                                    Nothing  -> tryReexports visited mods rest

specAllows :: ImportSpec -> ByteString -> Bool
specAllows ImportAll         _ = True
specAllows (ImportOnly ns)   n = n `elem` ns
specAllows (ImportHiding ns) n = n `notElem` ns

-- | Remove duplicate 'ByteString' elements from a list, preserving order.
nubBS :: [ByteString] -> [ByteString]
nubBS = go []
  where
    go _    []     = []
    go seen (x:xs)
        | x `elem` seen = go seen xs
        | otherwise      = x : go (x:seen) xs

-- | Extract any @module Foo@ re-export module names from a module's
-- export list.  Used by 'resolveImport' to follow re-export chains.
moduleReexports :: ModuleHeader -> [ModuleName]
moduleReexports h = case mhExports h of
    ExportAll     -> []
    ExportList xs -> [ m | ExportModule m <- xs ]

-- | Returns True if @n@ is directly exported (by name or type entry)
-- or if the module re-exports everything ('ExportAll').  Does NOT
-- return True for @ExportModule@ items — use 'moduleReexports' to
-- follow those chains separately.
--
-- Like 'exportsName', this understands @T(..)@ and @T(Ctor1, Ctor2)@:
-- the former expands to the type head plus every constructor from the
-- module's 'lmTypeCtorReg', the latter to the type head plus the
-- listed sub-names.
exportsNameDirect :: LoadedModule -> ByteString -> Bool
exportsNameDirect lm n = case mhExports (lmHeader lm) of
    ExportAll     -> True
    ExportList xs -> any matchDirect xs
  where
    tCtors = lmTypeCtorReg lm

    matchDirect (ExportName m)            = n == m
    matchDirect (ExportType m Nothing)    = n == m
    matchDirect (ExportType m (Just [])) =
        n == m || n `elem` Map.findWithDefault [] m tCtors
    matchDirect (ExportType m (Just subs)) =
        n == m || n `elem` subs
    matchDirect (ExportModule _) = False

-- | Like 'exportsNameDirect' but also returns True when the export
-- list contains a @module Foo@ entry (because the name may come from
-- that re-exported module).  Used by 'resolveImport' so that the
-- name-not-found case can fall through to 'followModuleReexports'.
--
-- Unlike 'exportsNameDirect', this version also looks inside
-- @ExportType T (Just subs)@ items:
--
--   * @T(Ctor1, Ctor2)@ matches @T@ itself plus any name in the sub-list.
--   * @T(..)@ (represented as @Just []@) matches @T@ plus every
--     constructor of @T@ known from the module's 'lmTypeCtorReg'.
--   * @T@ (represented as @Nothing@) only matches the type head @T@.
exportsName :: LoadedModule -> ByteString -> Bool
exportsName lm n = case mhExports (lmHeader lm) of
    ExportAll     -> True
    ExportList xs -> any matchExport xs
  where
    tCtors = lmTypeCtorReg lm

    matchExport (ExportName m)            = n == m
    matchExport (ExportType m Nothing)    = n == m
    matchExport (ExportType m (Just [])) =
        -- The `T(..)` form: the type head plus every constructor of T
        -- that the scanner saw in this module's source.
        n == m || n `elem` Map.findWithDefault [] m tCtors
    matchExport (ExportType m (Just subs)) =
        -- The `T(Ctor1, Ctor2, field1, ...)` form: the type head plus
        -- every explicitly-listed sub-name.
        n == m || n `elem` subs
    -- `module Foo` re-export: the scheduler follows this dynamically;
    -- here we conservatively return True so the name is not filtered
    -- out before the dynamic check in resolveImport.
    matchExport (ExportModule _)  = True

--------------------------------------------------------------------------------
-- Qualified-name splitting
--------------------------------------------------------------------------------

-- | If the name contains at least one internal dot with the last
-- segment lowercase and all earlier segments uppercase
-- (e.g. @B.suffix@, @Data.Map.empty@), split into (qualifier, bare).
-- Returns 'Nothing' otherwise — including for bare @main@ or for
-- strings containing dots inside operator names.
--
-- The parser doesn't yet emit qualified names for expressions, so
-- this path is partially dormant in Phase 2.5. It's still here so
-- that the scheduler's resolveQualified can be exercised the moment
-- the parser gains qualified-name support.
splitQualified :: ByteString -> Maybe (ByteString, ByteString)
splitQualified bs =
    let parts = BC.split '.' bs
    in case reverse parts of
        (tailPart : rest@(_ : _))
            | not (BC.null tailPart)
            , all (not . BC.null) rest
            , isLower (BC.head tailPart)
            , all (isUpper . BC.head) rest ->
                Just (BC.intercalate (BC.pack ".") (reverse rest), tailPart)
        _ -> Nothing
  where
    isLower c = c >= 'a' && c <= 'z'
    isUpper c = c >= 'A' && c <= 'Z'

--------------------------------------------------------------------------------
-- Reusable pieces from the old scheduler
--------------------------------------------------------------------------------

findOrResolveLhs :: Source -> KnownSymbols -> ByteString -> IO (Maybe BindingLhs)
findOrResolveLhs src known name = do
    existing <- lookupSymbol known name
    case existing of
        Just (SpanOnly lhs) -> pure (Just lhs)
        Just (Compiled _)   -> pure Nothing
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
            in concatMap (\(_, rhs) -> goAll bound' rhs) bs
               ++ goAll bound' e
        ECase s as  -> goAll bound s ++ concatMap (goAlt bound) as
        EIf c t e   -> goAll bound c ++ goAll bound t ++ goAll bound e
        EDo stmts   -> goStmts bound stmts
        ENeg e      -> goAll bound e
        ETuple es   -> concatMap (goAll bound) es
        ERecordCon _ fields -> concatMap (goAll bound . snd) fields
        ERecordWild _   -> []   -- fields resolved by scheduler; no expr free vars
        ERecordUpdate e fields -> goAll bound e ++ concatMap (goAll bound . snd) fields
        EImplicitRef _  -> []
        EImplicitLet bs e ->
            let names = map fst bs
                bound' = names ++ bound
            in concatMap (\(_, rhs) -> goAll bound' rhs) bs ++ goAll bound' e
        ESplice inner   -> goAll bound inner
        EQuote _        -> []   -- Phase 2.12: quote body is not evaluated; treat as no free vars
        ELabel _        -> []   -- Phase 3.5: labels have no free variables
        ETyApp inner _  -> goAll bound inner   -- value-level @T: inner expr contributes free vars

    -- A do-block introduces bindings left-to-right; each SBind/SLet
    -- extends the bound set for subsequent stmts.
    goStmts _     []                  = []
    goStmts bound (SExpr e   : rest)  = goAll bound e ++ goStmts bound rest
    goStmts bound (SBind n e : rest)  = goAll bound e ++ goStmts (n : bound) rest
    goStmts bound (SLet bs   : rest)  =
        let names  = map fst bs
            bound' = names ++ bound
        in concatMap (\(_, rhs) -> goAll bound' rhs) bs
           ++ goStmts bound' rest

    goAlt bound (Alt p e) = goAll (patBound p ++ bound) e

    patBound :: Pat -> [ByteString]
    patBound (PVar n)            = [n]
    patBound (PCon _ ps)         = concatMap patBound ps
    patBound (PAs n p)           = n : patBound p
    patBound (PBang p)           = patBound p
    patBound (PTuple ps)         = concatMap patBound ps
    patBound (PRecord _ fps)     = concatMap (patBound . snd) fps
    patBound (PRecordWild _)     = []  -- resolved later; can't enumerate fields here
    patBound (PView _ p)         = patBound p
    patBound _                   = []

-- | Desugar @ERecordCon Con [(f1,e1),(f2,e2)]@ into the equivalent
-- positional application @Con e_at_0 e_at_1@ using the FieldRegistry to
-- determine the correct field ordering.
--
-- Also desugars 'ERecordWild' (RecordWildCards construction):
--   @Con {..}@ → @Con f0 f1 ...@ where each @fi@ is @EVar fi@
--   (i.e., all fields must be in scope with the same name).
--
-- Fields not found in the registry are placed in declaration order (graceful
-- degradation for programs without scanDataDecls coverage).
desugarRecordCons :: FieldRegistry -> Expr -> Expr
desugarRecordCons fldReg = go
  where
    go (ERecordCon conName fields) =
        -- Build index -> expr mapping using the FieldRegistry.
        let byIndex = Map.fromList
                [ (idx, go e)
                | (fname, e) <- fields
                , Just pairs <- [Map.lookup fname fldReg]
                , Just idx   <- [lookup conName pairs]
                ]
            -- Arity = one past the highest known index, or just the
            -- number of given fields as a fallback.
            maxIdx = if Map.null byIndex
                         then length fields - 1
                         else maximum (Map.keys byIndex)
            errExpr _i = EApp (EVar "error")
                             (EVar "undefined")
            args = [ Map.findWithDefault (errExpr i) i byIndex
                   | i <- [0 .. maxIdx] ]
        in foldl EApp (EVar conName) args
    -- RecordWildCards construction: Con {..}
    -- Expand to Con f0 f1 ... using field order from FieldRegistry.
    go (ERecordWild conName) =
        let fieldPairs = conFields fldReg conName
            -- Build positional list sorted by field index.
            args = map (\(fname, _) -> EVar fname) fieldPairs
        in foldl EApp (EVar conName) args
    -- Record update: e { f1 = e1, f2 = e2, ... }
    -- Desugar to a case expression that reconstructs the record with updated fields.
    -- We look up all constructors that contain the updated field names in fldReg,
    -- then generate one case alt per such constructor.
    go (ERecordUpdate baseExpr updates) =
        let scrut = go baseExpr
            updatesG = [(fn, go fe) | (fn, fe) <- updates]
            -- Find all constructors that own any of the updated field names.
            -- A constructor is included if it appears in the FieldRegistry for
            -- at least one of the updated fields.
            relevantCons :: [ByteString]
            relevantCons =
                nubBS
                [ conName
                | (fname, _) <- updatesG
                , Just pairs <- [Map.lookup fname fldReg]
                , (conName, _) <- pairs
                ]
            buildAlt conName =
                let allFields = conFields fldReg conName   -- [(fname, idx)]
                    arity     = length allFields
                    -- Fresh variable names for the pattern: $ru0, $ru1, ...
                    varNames  = [ BC.pack ("$ru" <> show i) | i <- [0 .. arity - 1] ]
                    -- For each positional slot, use the updated expr if present,
                    -- otherwise use the original field variable.
                    updateMap  = Map.fromList updatesG
                    args = [ case Map.lookup fname updateMap of
                                 Just e' -> e'
                                 Nothing -> EVar (varNames !! idx)
                           | (fname, idx) <- allFields
                           ]
                    pat = PCon conName (map PVar varNames)
                    body = foldl EApp (EVar conName) args
                in Alt pat body
            alts = map buildAlt relevantCons
            -- If no relevant constructors found (e.g. unknown ADT), fall back to
            -- a runtime error so we don't silently discard the update.
            fallback = Alt PWild
                         (EApp (EVar "error")
                               (ELit (LStr "record update: unknown constructor")))
        in if null alts
               then go baseExpr   -- no registry info; best effort: return original
               else ECase scrut (alts ++ [fallback])

    -- Recurse into all sub-expressions.
    go (EApp f x)       = EApp (go f) (go x)
    go (ELam n e)       = ELam n (go e)
    go (ELet bs e)      = ELet [(n, go b) | (n, b) <- bs] (go e)
    go (ECase s as)     = ECase (go s) [Alt p (go b) | Alt p b <- as]
    go (EIf c t e)      = EIf (go c) (go t) (go e)
    go (EDo stmts)      = EDo (map goStmt stmts)
    go (ENeg e)         = ENeg (go e)
    go (ETuple es)      = ETuple (map go es)
    go (EImplicitRef n) = EImplicitRef n
    go (EImplicitLet bs e) =
        EImplicitLet [(n, go b) | (n, b) <- bs] (go e)
    go (EQuote inner)   = EQuote inner   -- Phase 2.12: quote body is opaque
    go (ETyApp inner ty) = ETyApp (go inner) ty   -- value-level @T: recurse into inner
    go e                = e  -- EVar, ELit

    goStmt (SExpr e)   = SExpr (go e)
    goStmt (SBind n e) = SBind n (go e)
    goStmt (SLet bs)   = SLet [(n, go b) | (n, b) <- bs]

-- | Look up all fields for a constructor from the FieldRegistry,
-- sorted by their positional index.
conFields :: FieldRegistry -> ByteString -> [(ByteString, Int)]
conFields fldReg conName =
    -- The FieldRegistry maps field names -> [(conName, idx)] pairs.
    -- Invert: collect all (fieldName, idx) for this constructor, sort by idx.
    let pairs = [ (fname, idx)
                | (fname, entries) <- Map.toList fldReg
                , Just idx <- [lookup conName entries]
                ]
    in sortByIdx pairs
  where
    sortByIdx ps = map snd $ Map.toAscList $ Map.fromList [(idx, (fname, idx)) | (fname, idx) <- ps]

-- | Desugar record patterns and view patterns in an expression tree.
-- Handles:
--   * 'PRecord' (NamedFieldPuns) → positional 'PCon' via FieldRegistry
--   * 'PRecordWild' (RecordWildCards) → positional 'PCon' binding all fields
--   * 'PView' (ViewPatterns) — desugared in case-alt context into a let+case
desugarRecordPats :: FieldRegistry -> Expr -> Expr
desugarRecordPats fldReg = goExpr
  where
    goExpr (EApp f x)       = EApp (goExpr f) (goExpr x)
    goExpr (ELam n e)       = ELam n (goExpr e)
    goExpr (ELet bs e)      = ELet [(n, goExpr b) | (n, b) <- bs] (goExpr e)
    goExpr (ECase s as)     = goCase (goExpr s) as
    goExpr (EIf c t e)      = EIf (goExpr c) (goExpr t) (goExpr e)
    goExpr (EDo stmts)      = EDo (map goStmt stmts)
    goExpr (ENeg e)         = ENeg (goExpr e)
    goExpr (ETuple es)      = ETuple (map goExpr es)
    goExpr (ERecordCon n fs) = ERecordCon n [(fn, goExpr fe) | (fn, fe) <- fs]
    goExpr (ERecordWild n)  = ERecordWild n
    goExpr (ERecordUpdate e fs) = ERecordUpdate (goExpr e) [(fn, goExpr fe) | (fn, fe) <- fs]
    goExpr (EImplicitRef n) = EImplicitRef n
    goExpr (EImplicitLet bs e) =
        EImplicitLet [(n, goExpr b) | (n, b) <- bs] (goExpr e)
    goExpr (ESplice inner)  = ESplice (goExpr inner)
    goExpr (EQuote inner)   = EQuote inner   -- Phase 2.12: quote body is opaque
    goExpr (ETyApp inner ty) = ETyApp (goExpr inner) ty   -- value-level @T: recurse
    goExpr e                = e  -- EVar, ELit

    goStmt (SExpr e)   = SExpr (goExpr e)
    goStmt (SBind n e) = SBind n (goExpr e)
    goStmt (SLet bs)   = SLet [(n, goExpr b) | (n, b) <- bs]

    -- Handle a case expression, desugaring view-pattern alts into a chain.
    -- View pattern alt: (f -> p) → fresh var, let vp = f scrut, case vp of p
    -- The tricky bit is that when the view match fails, we need to try the
    -- NEXT alt in the OUTER case. We do this by building a fallback chain:
    -- each view-pattern alt is wrapped in a let+case whose wildcard branch
    -- falls through to the remaining alts (also wrapped, recursively).
    goCase scrut alts =
        -- Ensure the scrutinee is bound to a variable to avoid duplication.
        case scrut of
            EVar _ -> buildAltChain scrut alts
            _      -> let sn = "$cs"
                      in ELet [(sn, scrut)] (buildAltChain (EVar sn) alts)

    buildAltChain _ [] = EApp (EVar "error") (EVar "\"case: non-exhaustive patterns\"")
    buildAltChain scrut (Alt pat body : rest) =
        case pat of
            PView fn vp ->
                -- Desugar: let $vp = fn scrut in case $vp of { vp -> body; _ -> rest }
                let vpn      = "$vp"
                    restExpr = buildAltChain scrut rest
                    innerCase = ECase (EVar vpn)
                        [ Alt (goPat vp) (goExpr body)
                        , Alt PWild restExpr
                        ]
                in ELet [(vpn, EApp (goExpr fn) scrut)] innerCase
            _ ->
                -- No view pattern: emit as a regular case, but fold remaining
                -- alts into the same case expression to avoid redundant fallback.
                -- Collect contiguous non-view alts together.
                let (nonView, viewRest) = span (not . isViewAlt) (Alt pat body : rest)
                    regularAlts = [Alt (goPat p) (goExpr b) | Alt p b <- nonView]
                    -- If there are more view-pattern alts after, add a wildcard
                    -- fallthrough alt that continues the chain.
                    allAlts = case viewRest of
                        [] -> regularAlts
                        _  -> regularAlts ++
                              [Alt PWild (buildAltChain scrut viewRest)]
                in ECase scrut allAlts

    isViewAlt (Alt (PView _ _) _) = True
    isViewAlt _                   = False

    -- Desugar record patterns recursively (no PView handling here — that
    -- is handled above in goCase / buildAltChain).
    goPat (PRecord conName fieldPats) =
        -- Build positional sub-pattern list using FieldRegistry order.
        let allFields = conFields fldReg conName
            fieldMap  = Map.fromList fieldPats
            subPats   = [ case Map.lookup fname fieldMap of
                            Just p  -> goPat p
                            Nothing -> PWild   -- omitted field → wildcard
                        | (fname, _) <- allFields
                        ]
        in PCon conName subPats
    goPat (PRecordWild conName) =
        -- Con {..} binds each field to a variable with the same name.
        let allFields = conFields fldReg conName
            subPats   = [PVar fname | (fname, _) <- allFields]
        in PCon conName subPats
    goPat (PView fn p)     = PView (goExpr fn) (goPat p)  -- nested view (unusual)
    goPat (PCon n ps)      = PCon n (map goPat ps)
    goPat (PAs n p)        = PAs n (goPat p)
    goPat (PBang p)        = PBang (goPat p)
    goPat (PTuple ps)      = PTuple (map goPat ps)
    goPat p                = p  -- PVar, PWild, PLit
