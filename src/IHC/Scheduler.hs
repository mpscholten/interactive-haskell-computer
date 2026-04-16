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
    ) where

import Control.Exception (throwIO, Exception, catch, SomeException, try)
import Data.ByteString (ByteString, isSuffixOf)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (isPrefixOf)
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import IHC.AST
import IHC.Builtins (builtinEnv, buildConEnv, buildFieldEnv)
import IHC.CabalProject (cachedPackageSearchPath, cachedPackageSearchPathWithIncludes)
import IHC.Classes (ClassRegistry, newClassRegistry, registerInstance)
import IHC.Cpp (cppPreprocessWithIncludes, defaultCppContext)
import IHC.Eval (force)
import IHC.Lexer (startCursor)
import IHC.ModuleHeader
import qualified IHC.Parser as Parser
import IHC.Parser (FixityTable, defaultFixityTable, scanFixityDecls, ParseError)
import IHC.Scan
import IHC.Source
import IHC.TH (expandSplicesInExpr)
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
    { lmName     :: !ModuleName
    , lmHeader   :: !ModuleHeader
    , lmSource   :: !Source
    , lmKnown    :: !KnownSymbols
    , lmDataReg  :: !DataRegistry
    , lmFieldReg :: !FieldRegistry
      -- | Accumulated (local-name, parsed body) pairs for this module.
    , lmBodies   :: !(IORef (Map ByteString Expr))
      -- | Whether this is the entry module (its bindings stay unqualified
      -- in the final env; foreign-module bindings are namespaced).
    , lmIsEntry  :: !Bool
      -- | Per-module fixity table: defaults + any @infixl/infixr/infix@
      -- declarations found at column 1 in this source.
    , lmFixity   :: !FixityTable
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

    -- Load the entry module. Its name is what the `module X where`
    -- header declares (or "Main" as a default). We always register it
    -- as the entry module so its bindings stay unqualified.
    entry <- loadEntryModule registry src
    let entryName = lmName entry

    -- Drive discovery from `main`.
    discoverInModule registry fullSearchPath includeMap entry "main"

    -- Collect every loaded module.
    reg <- readIORef registry
    let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]

    -- Union data registries and field registries across all modules.
    let unionedData  = foldr Map.union Map.empty (map lmDataReg  loadedModules)
        unionedFields = foldr Map.union Map.empty (map lmFieldReg loadedModules)
    conEnv   <- buildConEnv  unionedData
    fieldEnv <- buildFieldEnv unionedFields
    builtins <- builtinEnv classReg
    let base = Map.union builtins (Map.union fieldEnv conEnv)

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
    mapM_ (registerInstancesFrom classReg env) loadedModules

    case lookupEnv "main" env of
        Just t  -> pure (env, t)
        Nothing -> error ("IHC.Scheduler: no `main` binding in module "
                           <> BC.unpack entryName)

-- | Build a fresh base environment with all builtins and an empty
-- ClassRegistry. Used by the REPL to get a starting env without
-- requiring a @main@ binding.
buildBaseEnv :: IO (Env, ClassRegistry)
buildBaseEnv = do
    classReg <- newClassRegistry
    builtins <- builtinEnv classReg
    conEnv   <- buildConEnv Map.empty
    let env = Map.union builtins conEnv
    pure (env, classReg)

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
    -- Load the entry module.
    entry <- loadEntryModule registry src
    -- Determine which names to bring into scope.
    allNames <- scanAllTopLevelNames (lmSource entry)
    let header      = lmHeader entry
        -- Names the module declares it exports (or all names if ExportAll).
        exported = case mhExports header of
            ExportAll    -> allNames
            ExportList _ -> filter (exportsNameDirect header) allNames
    -- Demand-discover every exported name.
    mapM_ (discoverInModule registry fullSearchPath includeMap entry) exported
    -- Collect all loaded modules.
    reg <- readIORef registry
    let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]
    let unionedData   = foldr Map.union Map.empty (map lmDataReg  loadedModules)
        unionedFields = foldr Map.union Map.empty (map lmFieldReg loadedModules)
    conEnv    <- buildConEnv  unionedData
    fieldEnv' <- buildFieldEnv unionedFields
    builtins  <- builtinEnv classReg
    let base = Map.union builtins (Map.union fieldEnv' conEnv)
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
    mapM_ (registerInstancesFrom classReg innerEnv) loadedModules
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
effectiveExports
    :: ModuleRegistry
    -> Map ByteString Thunk     -- ^ thunkByKey: fully-qualified key → pre-built slot
    -> LoadedModule             -- ^ the module being interrogated
    -> [ModuleName]             -- ^ visited: cycle guard
    -> IO [(ByteString, Thunk)] -- ^ (bare-name, slot)
effectiveExports registry thunkByKey lm visited = do
    case mhExports (lmHeader lm) of
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
  where
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
                            effectiveExports registry thunkByKey reLm (m : visited)
                        _ -> pure []

    -- Look up @n@ exported from @lm'@: first try the direct qualified key,
    -- then fall back to a suffix search (for named re-exports whose binding
    -- lives in a different module — e.g. Data.List exports `map` but it's
    -- keyed as "Data.OldList.map" or "GHC.List.map").
    lookupName lm' n =
        let prefix = lmName lm' <> BC.pack "."
            suffix = BC.pack "." <> n
        in case Map.lookup (prefix <> n) thunkByKey of
            Just t  -> pure [(n, t)]
            Nothing ->
                -- Suffix search: any key ending in ".n".
                case [ t | (k, t) <- Map.toList thunkByKey
                          , suffix `isSuffixOf` k ] of
                    (t:_) -> pure [(n, t)]
                    []    -> pure []

-- | Pre-load and discover all modules and bindings that will be needed by
-- 'effectiveExports' for @lm@.  Unlike 'discoverInModule' / 'resolveImport',
-- this helper explicitly loads cache modules because the user explicitly asked
-- to import them.
--
-- Steps:
--   1. Discover all locally-defined top-level names in @lm@.
--   2. For each missing @ExportName@ in @lm@, preload candidate unqualified
--      import modules once so their local bodies and transitive re-exports
--      are available to 'effectiveExports'.
--   3. For each @ExportModule m@ in @lm@'s export list, recursively call
--      'preloadForEffectiveExports' on @m@ (cycle-guarded by @visited@).
preloadForEffectiveExports
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]             -- ^ visited: cycle guard
    -> IO ()
preloadForEffectiveExports registry searchPath includeMap lm visited = do
    -- 1. Discover only the locally-defined names that can actually contribute
    --    to this module's export surface. Private helpers do not need to be
    --    preloaded just to answer a REPL import.
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
    preloadImportsForNamedReexports registry searchPath includeMap lm visited namedRe
    -- 3. ExportModule re-exports — recurse.
    let reexportMods = filter (`notElem` visited) (moduleReexports (lmHeader lm))
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
                   `catch` (\(_ :: ModuleNotFound) -> buildEmptyStubModule m)
        -- Wrap entire sub-load in try so a broken re-exported module
        -- doesn't abort preloading of other modules.
        r <- try (preloadForEffectiveExports registry searchPath includeMap reLm
                     (m : visited))
                 :: IO (Either SomeException ())
        case r of
            Right () -> pure ()
            Left  _  -> pure ()

-- | Preload the unqualified import modules of @lm@ that could provide any of
-- the named re-exports in @missingNames@. Each candidate import module is
-- visited at most once per call, which avoids repeated graph walks for
-- export-heavy modules like Data.List.
--
-- Parse errors and other exceptions from individual module loads or
-- recursive preloads are silently swallowed: if a candidate module can't be
-- parsed, we skip it and continue. The caller (@preloadForEffectiveExports@)
-- will discover whatever was successfully loaded.
preloadImportsForNamedReexports
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]             -- ^ visited import chain
    -> [ByteString]
    -> IO ()
preloadImportsForNamedReexports registry searchPath includeMap lm visited missingNames = do
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
                       `catch` (\(_ :: ModuleNotFound) -> buildEmptyStubModule (impModule imp))
            preloadForEffectiveExports registry searchPath includeMap srcLm
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
        preloadForEffectiveExports registry fullSearchPath includeMap targetLm
            [lmName targetLm]
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
                    r <- try (preloadForEffectiveExports registry fullSearchPath
                                  includeMap lm [lmName lm])
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
            unionedFields = foldr Map.union Map.empty (map lmFieldReg loadedModules0)
        conEnv    <- buildConEnv  unionedData
        fieldEnv' <- buildFieldEnv unionedFields
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
        let innerEnv = Map.union (Map.fromList selfAliases)
                     $ Map.union (Map.fromList effectivePairs)
                     $ Map.union aliasEnv qualEnv
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
    regAfterDiscover <- readIORef registry
    let discoveredDeps =
            [ lm
            | (m, Loaded lm) <- Map.toList regAfterDiscover
            , m /= lmName targetLm
            ]
    mapM_ (\lm -> do
        r <- try (preloadForEffectiveExports registry fullSearchPath includeMap lm [lmName lm])
                 :: IO (Either SomeException ())
        case r of
            Right () -> pure ()
            Left _   -> pure ()
        ) discoveredDeps
    reg0 <- readIORef registry
    let loadedModules0 = [ lm | (_, Loaded lm) <- Map.toList reg0 ]
        unionedData    = foldr Map.union Map.empty (map lmDataReg loadedModules0)
        unionedFields  = foldr Map.union Map.empty (map lmFieldReg loadedModules0)
    conEnv    <- buildConEnv unionedData
    fieldEnv' <- buildFieldEnv unionedFields
    builtins  <- builtinEnv classReg
    let baseForImport = Map.union builtins (Map.union fieldEnv' conEnv)
    mapM_ (expandSplicesInModule baseForImport) loadedModules0
    qualPairs <- concat <$> mapM (exportBodies registry (Map.keysSet builtins)) loadedModules0
    slots <- mapM (\_ -> newIORef BlackHole) qualPairs
    let qualEnv    = extendEnvMany (zip (map fst qualPairs) slots) baseForImport
        thunkByKey = Map.fromList (zip (map fst qualPairs) slots)
        modPrefix  = lmName targetLm <> BC.pack "."
        qualPrefix = case impAlias imp of
            Just a  -> a <> BC.pack "."
            Nothing
                | impQualified imp -> lmName targetLm <> BC.pack "."
                | otherwise        -> BC.empty
        requestedPairs =
            [ (n, t)
            | n <- requested
            , Just t <- [lookupRequestedThunk targetLm thunkByKey n]
            ]
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
        innerEnv = Map.union (Map.fromList selfAliases)
                 $ Map.union (Map.fromList requestedPairs)
                 $ Map.union (Map.fromList rewriteAliasPairs)
                 $ Map.union aliases qualEnv
    mapM_ (\((_, rhs), slot) ->
               writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
          (zip qualPairs slots)
    mapM_ (registerInstancesFrom classReg innerEnv) loadedModules0
    let additions  = Map.difference aliasEnv existingEnv
        merged     = Map.union existingEnv additions
    pure (merged, Map.size additions)
  where
    lookupRequestedThunk lm thunkByKey n =
        let prefix = lmName lm <> BC.pack "."
            suffix = BC.pack "." <> n
        in case Map.lookup (prefix <> n) thunkByKey of
            Just t  -> Just t
            Nothing ->
                case [ t | (k, t) <- Map.toList thunkByKey
                         , suffix `isSuffixOf` k ] of
                    (t:_) -> Just t
                    []    -> Nothing

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
registerInstancesFrom :: ClassRegistry -> Env -> LoadedModule -> IO ()
registerInstancesFrom classReg env lm = do
    decls <- scanInstanceDecls (lmSource lm)
    mapM_ (registerOne classReg env lm) decls

registerOne :: ClassRegistry -> Env -> LoadedModule -> InstanceDecl -> IO ()
registerOne classReg env lm (InstanceDecl cls typ methods) = do
    -- Evaluate each method; skip the entire instance if any method fails
    -- (e.g. unbound variable in a transitive library dependency).  A failed
    -- instance just means class-dispatch won't find that instance at runtime
    -- which is correct for methods that are never called.
    r <- try (mapM (evalMethod env lm) methods) :: IO (Either SomeException [Val])
    case r of
        Left  _ -> pure ()   -- silently skip unresolvable instances
        Right methodVals -> registerInstance classReg cls typ methodVals

evalMethod :: Env -> LoadedModule -> (ByteString, BindingLhs) -> IO Val
evalMethod env lm (_, lhs) = do
    expr0 <- Parser.parseBodyExprWithFixity
                (lmSource lm) (lmFixity lm) (lhsClauses lhs)
    let expr = desugarRecordPats (lmFieldReg lm)
                 (desugarRecordCons (lmFieldReg lm) expr0)
    -- Evaluate the method expression to a Val in the global env.
    -- We need Eval here but can't import it (cycle). Use a thunk trick:
    -- make a thunk and force it immediately.
    t <- newThunk env expr
    force t

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
                , exportsNameDirect (lmHeader tm) n
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
        let viaImports = filter (not . impQualified)
                           (mhImports (lmHeader tm))
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
    aliasesForImport thunkByKey imp
        | impModule imp == BC.pack "Prelude" = pure []
        | otherwise = do
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
                , exportsNameDirect (lmHeader tm) n
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
        let viaImports = filter (not . impQualified)
                           (mhImports (lmHeader tm))
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
    let header = fromMaybe emptyHeader mHeader
        name   = fromMaybe (BC.pack "Main") (mhName header)
    writeIORef registry (Map.singleton name Loading)
    lm <- buildLoadedModule name True header src
    modifyIORef' registry (Map.insert name (Loaded lm))
    pure lm

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

buildEmptyStubModule :: ModuleName -> IO LoadedModule
buildEmptyStubModule name = do
    known  <- emptyKnownSymbols
    bodies <- newIORef Map.empty
    let src = mkSource (BC.unpack name) ""
    pure LoadedModule
        { lmName     = name
        , lmHeader   = ModuleHeader (Just name) ExportAll []
        , lmSource   = src
        , lmKnown    = known
        , lmDataReg  = Map.empty
        , lmFieldReg = Map.empty
        , lmBodies   = bodies
        , lmIsEntry  = False
        , lmFixity   = defaultFixityTable
        }

buildLoadedModule :: ModuleName -> Bool -> ModuleHeader -> Source -> IO LoadedModule
buildLoadedModule name isEntry header src = do
    known         <- emptyKnownSymbols
    (dataR, fldR) <- scanDataDecls src
    bodies        <- newIORef Map.empty
    fixity        <- scanFixityDecls src defaultFixityTable
    pure LoadedModule
        { lmName     = name
        , lmHeader   = header
        , lmSource   = src
        , lmKnown    = known
        , lmDataReg  = dataR
        , lmFieldReg = fldR
        , lmBodies   = bodies
        , lmIsEntry  = isEntry
        , lmFixity   = fixity
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
discoverInModule registry searchPath includeMap lm name
    -- Qualified name (contains a dot and the prefix looks like a module
    -- alias)? Route to the target module directly.
    | Just (qual, bareName) <- splitQualified name = do
        mTarget <- resolveQualified registry searchPath includeMap lm qual
        case mTarget of
            Just targetLm -> do
                discoverInModule registry searchPath includeMap targetLm bareName
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
                            Nothing -> do
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
                                      discoverInModule registry searchPath includeMap lm fv
                                        `catch` (\(_ :: ModuleNotFound) -> pure ())
                                        `catch` (\(_ :: ParseError)     -> pure ())
                                mapM_ discoverFreeVar (nubBS (freeVars expr))
                    Nothing -> do
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
        -- Prelude is handled as a global-builtin no-op for Phase 2.5.
        | impModule imp == BC.pack "Prelude" = tryImports rest
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
                then tryImports rest
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
                                    -- Export-list check: the target module must
                                    -- actually export @name@.
                                    if exportsName (lmHeader targetLm) name
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
                                    if exportsName (lmHeader targetLm) name
                                        then followNamedReexport targetLm rest
                                        else followModuleReexports targetLm rest

    -- | @targetLm@ exports @name@ by name (ExportName entry) but doesn't
    -- define it locally.  Walk @targetLm@'s own unqualified imports and
    -- recurse into each until we find the definition.
    --
    -- @depth@ limits how many import-levels we traverse.  We use depth 2
    -- so that two-hop re-export chains (A → B → C where C defines the
    -- name) are found, but we don't blow up loading entire base dependency
    -- trees for name searches in large library modules.
    followNamedReexport via rest = followNamedReexportD (3 :: Int) via rest

    followNamedReexportD depth via rest
        | depth <= (0 :: Int) = tryImports rest
        | otherwise = do
            -- First try module-form re-exports (they may also apply).
            let reexportedMods = filter (/= lmName via)
                               $ moduleReexports (lmHeader via)
            r <- tryReexports [lmName via] reexportedMods rest
            case r of
                Just () -> pure (Just ())
                Nothing -> do
                    -- Follow the via module's own unqualified imports.
                    let viaImports = filter (not . impQualified)
                                       (mhImports (lmHeader via))
                        filteredImports = filter (\i ->
                            impModule i /= BC.pack "Prelude" &&
                            specAllows (impSpec i) name) viaImports
                    tryViaImports filteredImports depth rest

    tryViaImports [] _depth rest = tryImports rest
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
                        if exportsName (lmHeader srcLm) name
                            then do
                                discoverInModule registry searchPath includeMap srcLm name
                                pure (Just ())
                            else tryViaImports moreImps depth rest
                    Nothing ->
                        -- srcLm might itself re-export via unqualified imports
                        -- (go one level deeper, decrementing the depth cap).
                        if exportsName (lmHeader srcLm) name
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
                                    if exportsName (lmHeader srcLm) name
                                        then do
                                            discoverInModule registry searchPath includeMap srcLm name
                                            pure (Just ())
                                        else tryViaImports moreImps depth rest
                                Nothing ->
                                    -- Not defined directly in srcLm; recurse one
                                    -- level deeper if srcLm exports the name via
                                    -- its own imports (limited by depth).
                                    if exportsName (lmHeader srcLm) name && depth > 1
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
                                if exportsName (lmHeader reLm) name
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
exportsNameDirect :: ModuleHeader -> ByteString -> Bool
exportsNameDirect h n = case mhExports h of
    ExportAll     -> True
    ExportList xs -> any matchDirect xs
  where
    matchDirect (ExportName m)   = n == m
    matchDirect (ExportType m _) = n == m
    matchDirect (ExportModule _) = False

-- | Like 'exportsNameDirect' but also returns True when the export
-- list contains a @module Foo@ entry (because the name may come from
-- that re-exported module).  Used by 'resolveImport' so that the
-- name-not-found case can fall through to 'followModuleReexports'.
exportsName :: ModuleHeader -> ByteString -> Bool
exportsName h n = case mhExports h of
    ExportAll     -> True
    ExportList xs -> any matchExport xs
  where
    matchExport (ExportName m)    = n == m
    matchExport (ExportType m _)  = n == m
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
