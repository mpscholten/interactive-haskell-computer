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
    ) where

import Control.Exception (throwIO, Exception, catch)
import qualified System.IO
import Data.ByteString (ByteString, isSuffixOf)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getHomeDirectory)
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
import IHC.Parser (FixityTable, defaultFixityTable, scanFixityDecls)
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
    let base = Map.union fieldEnv (Map.union conEnv builtins)

    -- Phase 2.11: expand TH splices in every loaded module's bodies.
    -- Run AFTER all modules are discovered (so imports are resolved) but
    -- BEFORE knot-tying. Use 'base' as the splice evaluation env — it
    -- contains all builtins including the 'lift' function.
    mapM_ (expandSplicesInModule base) loadedModules

    -- Build (fully-qualified-name, Expr) pairs for every loaded body.
    qualPairs <- concat <$> mapM (exportBodies registry) loadedModules

    -- Tie the knot for all bodies at once.
    slots <- mapM (\_ -> newIORef BlackHole) qualPairs
    let qualEnv = extendEnvMany (zip (map fst qualPairs) slots) base

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
    let env = Map.union conEnv builtins
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
    let base = Map.union fieldEnv' (Map.union conEnv builtins)
    -- Phase 2.11: expand TH splices.
    mapM_ (expandSplicesInModule base) loadedModules
    -- Build (key, Expr) pairs.  Entry module bindings are keyed bare.
    qualPairs <- concat <$> mapM (exportBodies registry) loadedModules
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

-- | Load a single import declaration into an existing 'Env', as if the
-- REPL user typed @import Foo@ at the prompt.
--
-- For builtin-backed modules (Prelude, Data.List, etc.) the names are
-- already present in the base env — we return the env unchanged with
-- count 0.
--
-- For source-backed modules we load the target module, discover ALL its
-- top-level names (not just those reachable from some entry point), build
-- thunks for each exported name, and merge them into @existingEnv@.
-- Qualified imports (@import qualified Foo as F@) expose names under the
-- alias prefix (@F.name@) only; unqualified imports also add bare names.
--
-- The @searchPath@ should contain every directory that may hold the
-- target module's @.hs@ file.
--
-- On success returns the updated env and the number of NEW names added.
-- On failure raises an exception; the caller should catch it.
loadImportIntoEnv
    :: [FilePath]   -- ^ directories to search for module source files
    -> ImportDecl   -- ^ the parsed import declaration
    -> Env          -- ^ current REPL env
    -> IO (Env, Int)
loadImportIntoEnv searchPath imp existingEnv
    | isBuiltinBackedModule (impModule imp) = pure (existingEnv, 0)
    | otherwise = do
        -- Append cached package search dirs so mtl, transformers, etc. resolve.
        cacheWithIncludes <- cachedPackageSearchPathWithIncludes
        let cacheDirs      = map fst cacheWithIncludes
            includeMap     = Map.fromList cacheWithIncludes
            fullSearchPath = searchPath ++ cacheDirs
        registry <- newIORef Map.empty
        -- Load the target module (and its transitive imports).
        targetLm <- loadModule registry fullSearchPath includeMap (impModule imp)
        -- Collect names to discover:
        --   1. Locally-defined names exported by the module.
        --   2. ExportName entries not locally defined but re-exported via
        --      imports (e.g. Prelude exports `maybe` from Data.Maybe).
        --      discoverInModule follows the module's imports automatically.
        allLocalNames <- scanAllTopLevelNames (lmSource targetLm)
        let exportedLocal = filter (exportsName (lmHeader targetLm))
                          $ filter (specAllows (impSpec imp)) allLocalNames
            -- Named re-exports not locally defined; discoverInModule will
            -- chase through the module's imports to find them.
            -- Only lowercase-first names are value bindings; type constructors
            -- (uppercase or operators) would trigger loading GHC.Base → OOM.
            namedReexports = case mhExports (lmHeader targetLm) of
                ExportAll    -> []
                ExportList xs ->
                    [ n | ExportName n <- xs
                        , n `notElem` allLocalNames
                        , specAllows (impSpec imp) n
                        , not (BC.null n) && BC.head n >= 'a' && BC.head n <= 'z'
                    ]
            exported = nubBS (exportedLocal ++ namedReexports)
        mapM_ (discoverInModule registry fullSearchPath includeMap targetLm) exported
        -- For ExportModule re-exports (e.g. `module Control.Monad` in
        -- Prelude's export list) also load those modules and discover their
        -- names, so names from re-exported modules are available.
        let reexportedMods = moduleReexports (lmHeader targetLm)
        reexportedLms <- mapM (loadModule registry fullSearchPath includeMap) reexportedMods
        reexportedNamesPerMod <- mapM (\lm -> do
            ns <- scanAllTopLevelNames (lmSource lm)
            let ns' = filter (specAllows (impSpec imp)) ns
            mapM_ (discoverInModule registry fullSearchPath includeMap lm) ns'
            pure (lm, ns')
            ) reexportedLms
        -- Collect every loaded module and build a combined env.
        reg <- readIORef registry
        let loadedModules = [ lm | (_, Loaded lm) <- Map.toList reg ]
        let unionedData   = foldr Map.union Map.empty (map lmDataReg  loadedModules)
            unionedFields = foldr Map.union Map.empty (map lmFieldReg loadedModules)
        conEnv    <- buildConEnv  unionedData
        fieldEnv' <- buildFieldEnv unionedFields
        builtins <- builtinEnv =<< newClassRegistry
        let baseForImport = Map.union fieldEnv' (Map.union conEnv builtins)
        -- Build (qualified-key, Expr) pairs for each loaded module.
        -- All modules here are non-entry (lmIsEntry = False), so bodies
        -- are keyed as Module.name.
        qualPairs <- concat <$> mapM (exportBodies registry) loadedModules
        -- Tie the knot.
        slots <- mapM (\_ -> newIORef BlackHole) qualPairs
        let qualEnv = extendEnvMany (zip (map fst qualPairs) slots) baseForImport
        -- Build aliases according to the import declaration.
        -- For unqualified: bare name -> thunk.
        -- For qualified with alias A: A.name -> thunk.
        -- For qualified without alias: Module.name -> thunk (already in qualEnv).
        let thunkByKey = Map.fromList (zip (map fst qualPairs) slots)
            modPrefix  = lmName targetLm <> BC.pack "."
            qualPrefix = case impAlias imp of
                Just a  -> a <> BC.pack "."
                Nothing
                    | impQualified imp -> lmName targetLm <> BC.pack "."
                    | otherwise        -> BC.empty
            -- Look up a bare name in thunkByKey.  Try the direct modPrefix
            -- first (locally-defined names).  If not found, search for any
            -- key with suffix "." <> n — this handles named re-exports
            -- (e.g. Prelude exports `maybe` keyed as "Data.Maybe.maybe").
            lookupThunk n =
                case Map.lookup (modPrefix <> n) thunkByKey of
                    Just t  -> Just t
                    Nothing ->
                        let suffix = BC.pack "." <> n
                        in case [ t | (k, t) <- Map.toList thunkByKey
                                    , suffix `isSuffixOf` k ] of
                            (t:_) -> Just t
                            []    -> Nothing
            -- Aliases for exported names (local or re-exported via imports).
            bareAliases
                | impQualified imp = []
                | otherwise =
                    [ (n, t)
                    | n <- exported
                    , Just t <- [lookupThunk n]
                    ]
            qualAliases
                | BC.null qualPrefix = []
                | otherwise =
                    [ (qualPrefix <> n, t)
                    | n <- exported
                    , Just t <- [lookupThunk n]
                    ]
            -- Aliases for names re-exported via `module Foo` entries.
            -- These come from the re-exported modules' own exported names.
            reexportBareAliases
                | impQualified imp = []
                | otherwise =
                    [ (n, t)
                    | (reLm, reNs) <- reexportedNamesPerMod
                    , n <- reNs
                    , exportsNameDirect (lmHeader reLm) n
                    , let rePrefix = lmName reLm <> BC.pack "."
                    , Just t <- [Map.lookup (rePrefix <> n) thunkByKey]
                    ]
            reexportQualAliases
                | BC.null qualPrefix = []
                | otherwise =
                    [ (qualPrefix <> n, t)
                    | (reLm, reNs) <- reexportedNamesPerMod
                    , n <- reNs
                    , exportsNameDirect (lmHeader reLm) n
                    , let rePrefix = lmName reLm <> BC.pack "."
                    , Just t <- [Map.lookup (rePrefix <> n) thunkByKey]
                    ]
        let aliasEnv = Map.fromList
                ( bareAliases ++ qualAliases
               ++ reexportBareAliases ++ reexportQualAliases )
        -- For intra-module self-references (recursive calls, mutual recursion),
        -- locally-defined names in the target module must be reachable by their
        -- bare name inside the closure env.  The rewriteExpr transform uses the
        -- import-rewrite table for cross-module references but does NOT rewrite
        -- self-references (they stay as bare names).  Supply bare-name → slot
        -- mappings from the target module's own bodies so that, e.g.,
        -- `isSubsequenceOf` calling itself works inside the Data.List closure.
        let selfAliases =
                [ (n, slot)
                | (qualKey, slot) <- Map.toList thunkByKey
                , BC.isPrefixOf modPrefix qualKey
                , let n = BC.drop (BC.length modPrefix) qualKey
                ]
        -- The final env visible to imported bindings: qualEnv + aliases.
        -- We also include the aliases so that intra-module cross-references
        -- (e.g. `greet` calling `suffix`) resolve properly.
        let innerEnv = Map.union (Map.fromList selfAliases)
                     $ Map.union aliasEnv qualEnv
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
              (zip qualPairs slots)
        -- Merge into the REPL env: prefer existing REPL bindings (shadow).
        -- Report the count of NEW bare (or qualified-alias) names added so
        -- the REPL can print "imported Foo (N names)".
        let newBindings = Map.union qualEnv aliasEnv
            additions   = Map.difference newBindings existingEnv
            merged      = Map.union existingEnv additions
            newAliases  = Map.fromList
                            ( bareAliases ++ qualAliases
                           ++ reexportBareAliases ++ reexportQualAliases )
                          `Map.difference` existingEnv
        pure (merged, Map.size newAliases)

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
    methodVals <- mapM (evalMethod env lm) methods
    registerInstance classReg cls typ methodVals

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
exportBodies :: ModuleRegistry -> LoadedModule -> IO [(ByteString, Expr)]
exportBodies registry lm = do
    bs <- readIORef (lmBodies lm)
    let keyPrefix | lmIsEntry lm = BC.empty
                  | otherwise    = lmName lm <> BC.pack "."
    rewrites <- buildImportRewrites registry lm
    let transform e
            | lmIsEntry lm = e     -- entry keeps bare names; env has aliases
            | otherwise    = rewriteExpr rewrites e
    pure
        -- Filter out foreign-alias sentinels inserted by discoverInModule.
        -- A sentinel is an entry (n, EVar n) meaning "n resolved via import;
        -- no local body to export".  Exporting them would create either
        -- self-referential thunks (entry module) or dangling bare-name
        -- references in non-entry module bodies.
        [ (keyPrefix <> n, transform e)
        | (n, e) <- Map.toList bs
        , e /= EVar n   -- skip foreign-alias sentinels
        ]

-- | Build a map from each locally-visible imported name to its
-- fully-qualified target key (as stored in the flat env).
buildImportRewrites :: ModuleRegistry -> LoadedModule -> IO (Map ByteString ByteString)
buildImportRewrites registry lm = do
    reg <- readIORef registry
    let imports = mhImports (lmHeader lm)
    pairs <- concat <$> mapM (rewritesForImport reg) imports
    pure (Map.fromList pairs)
  where
    rewritesForImport reg imp
        | impModule imp == BC.pack "Prelude" = pure []
        | otherwise = case Map.lookup (impModule imp) reg of
            Just (Loaded tm) -> do
                -- Gather (name, qualifiedKey) pairs from the module's own
                -- bodies plus any `module Foo` re-exports.
                directPairs <- directRewritePairs reg tm
                reexportPairs <- concat <$>
                    mapM (rewritePairsFromReexport reg)
                         (moduleReexports (lmHeader tm))
                let allPairs = directPairs ++ reexportPairs
                    visible  = filter (specAllows (impSpec imp) . fst) allPairs
                    bare | impQualified imp = []
                         | otherwise = visible
                    qualRef = case impAlias imp of
                        Just a  -> Just (a <> BC.pack ".")
                        Nothing
                            | impQualified imp -> Just (lmName tm <> BC.pack ".")
                            | otherwise        -> Nothing
                    qual = case qualRef of
                        Just p  -> [(p <> n, q) | (n, q) <- visible]
                        Nothing -> []
                pure (bare ++ qual)
            _ -> pure []

    -- | @(bare-name, fully-qualified-key)@ pairs for names exported by a
    -- module — including names re-exported from its own imports via
    -- @ExportName@ entries (the named re-export chain).
    directRewritePairs reg tm = do
        bodiesMap <- readIORef (lmBodies tm)
        let prefix   = lmName tm <> BC.pack "."
            allNames = Map.keys bodiesMap
            -- Names defined directly in tm and exported.
            localExported = filter (exportsNameDirect (lmHeader tm)) allNames
            localPairs = [(n, prefix <> n) | n <- localExported]
        -- For ExportName entries not covered by local bodies, follow
        -- tm's own unqualified imports (named re-export chain).
        namedPairs <- namedReexportPairs reg tm bodiesMap
        pure (localPairs ++ namedPairs)

    -- | Collect (name, qualified-key) pairs for ExportName entries that
    -- are not locally defined in @tm@ but are re-exported via @tm@'s
    -- own unqualified imports.
    namedReexportPairs reg tm bodiesMap = do
        case mhExports (lmHeader tm) of
            ExportAll    -> pure []   -- ExportAll: no explicit name list to iterate
            ExportList xs -> do
                let exportedNames = [ n | ExportName n <- xs ]
                    -- Names that appear in the export list but are NOT locally defined
                    -- must come from an import (named re-export chain).
                    missingNames  = filter (\n -> not (Map.member n bodiesMap)) exportedNames
                concat <$> mapM (findNameInImports reg tm) missingNames

    -- | Find which of @tm@'s unqualified imports provides @n@, returning
    -- a @(n, qualified-key)@ pair if found.
    findNameInImports reg tm n = do
        let viaImports = filter (not . impQualified)
                           (mhImports (lmHeader tm))
            viable = filter (\i ->
                impModule i /= BC.pack "Prelude" &&
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
                            deeper <- findNameInImports reg srcLm n
                            case deeper of
                                [] -> go rest
                                ps -> pure ps
                _ -> go rest

    -- | @(bare-name, fully-qualified-key)@ pairs from a re-exported
    -- module (@module Foo@ in the export list).
    rewritePairsFromReexport reg modName =
        case Map.lookup modName reg of
            Just (Loaded reLm) -> directRewritePairs reg reLm
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
    -- Only the entry module contributes aliases (for now).
    let imports = mhImports (lmHeader entry)
    pairs <- concat <$> mapM (aliasesForImport thunkByKey) imports
    pure (Map.fromList pairs)
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

    -- | Return @(bare-name, Thunk)@ pairs for names exported by a loaded
    -- module — including names re-exported via ExportName from its imports.
    namesFromModule thunkByKey tm = do
        reg <- readIORef registry
        bodiesMap <- readIORef (lmBodies tm)
        let prefix   = lmName tm <> BC.pack "."
            allN     = Map.keys bodiesMap
            -- Names defined directly in tm and exported.
            localExported = filter (exportsNameDirect (lmHeader tm)) allN
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
                    missingNames  = filter (\n -> not (Map.member n bodiesMap)) exportedNames
                pairs <- concat <$> mapM (findThunkInImports reg thunkByKey tm) missingNames
                pure pairs

    -- | Find the Thunk for @n@ by walking @tm@'s unqualified imports.
    findThunkInImports reg thunkByKey tm n = do
        let viaImports = filter (not . impQualified)
                           (mhImports (lmHeader tm))
            viable = filter (\i ->
                impModule i /= BC.pack "Prelude" &&
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
                            deeper <- findThunkInImports reg thunkByKey srcLm n
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

-- | Return 'True' if @name@ can be found in the search path AND its file
-- is NOT under the global package cache (@~\/.cache\/ihc\/sources\/@).
-- Used by 'followNamedReexport' to avoid eagerly loading large library
-- dependency subtrees (e.g. GHC.Base from base-4.19) during re-export
-- chain following — those should only be followed if already in the
-- registry.
isLocalModule :: [FilePath] -> ModuleName -> IO Bool
isLocalModule searchPath name = do
    home <- getHomeDirectory
    let cachePrefix = home </> ".cache" </> "ihc" </> "sources"
    -- locateModule throws ModuleNotFound if absent; treat that as non-local.
    mPath <- (Just <$> locateModule searchPath name)
                `catch` (\(_ :: ModuleNotFound) -> pure Nothing)
    case mPath of
        Nothing -> pure False
        Just p  -> pure (not (cachePrefix `isPrefixOf` p))

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
            Just targetLm ->
                discoverInModule registry searchPath includeMap targetLm bareName
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
                        expr0 <- Parser.parseBodyExprWithFixity
                                    (lmSource lm)
                                    (lmFixity lm)
                                    (lhsClauses lhs)
                        let expr = desugarRecordPats (lmFieldReg lm)
                                     (desugarRecordCons (lmFieldReg lm) expr0)
                        modifyIORef' (lmBodies lm) (Map.insert name expr)
                        -- Recurse into every free var. Qualified ones
                        -- will be routed on the next call.
                        -- Deduplicate to avoid O(n^2) re-traversal when a
                        -- name appears many times in one binding.
                        mapM_ (discoverInModule registry searchPath includeMap lm)
                              (nubBS (freeVars expr))
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
            -- Don't eagerly load library modules that aren't already in
            -- the registry.  Loading them to satisfy a single unbound free
            -- variable would pull in the entire base transitive-closure
            -- (GHC.Base and friends) and cause OOM.  Only load a module
            -- when it is already in the registry (loaded by an earlier
            -- discoverInModule call) or when it is a local (non-cache) file.
            reg <- readIORef registry
            shouldLoad <- case Map.lookup (impModule imp) reg of
                Just _  -> pure True   -- already loaded, safe to query
                Nothing -> isLocalModule searchPath (impModule imp)
            if not shouldLoad
                then tryImports rest
                else do
                    targetLm <- loadModule registry searchPath includeMap (impModule imp)
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
    followNamedReexport via rest = followNamedReexportD 1 via rest

    followNamedReexportD depth via rest
        | depth <= 0 = tryImports rest
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
        reg <- readIORef registry
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
                -- Module not yet loaded. Only attempt to load it when the
                -- file resolves to a LOCAL path (not under ~/.cache/ihc/sources/).
                -- Library modules (GHC.Base, GHC.IO, etc.) are huge; loading
                -- them eagerly to search for one re-exported name causes OOM.
                -- Local modules (e.g. Inner.hs in a test fixture) are small
                -- and safe to load once.
                do
                    local <- isLocalModule searchPath (impModule imp)
                    if not local
                        then tryViaImports moreImps depth rest
                        else do
                            srcLm <- loadModule registry searchPath includeMap (impModule imp)
                            mLhs  <- findOrResolveLhs (lmSource srcLm) (lmKnown srcLm) name
                            case mLhs of
                                Just _ ->
                                    if exportsName (lmHeader srcLm) name
                                        then do
                                            discoverInModule registry searchPath includeMap srcLm name
                                            pure (Just ())
                                        else tryViaImports moreImps depth rest
                                Nothing ->
                                    -- Newly loaded module doesn't define the name
                                    -- either. Don't recurse further.
                                    tryViaImports moreImps depth rest

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
        reLm <- loadModule registry searchPath includeMap modName
        mLhs <- findOrResolveLhs (lmSource reLm) (lmKnown reLm) name
        case mLhs of
            Just _ ->
                if exportsName (lmHeader reLm) name
                    then do
                        discoverInModule registry searchPath includeMap reLm name
                        pure (Just ())
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
