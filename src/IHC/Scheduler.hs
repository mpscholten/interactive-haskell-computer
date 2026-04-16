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
      -- * Types exposed for testing
    , ModuleRegistry
    ) where

import Control.Exception (throwIO, Exception, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))

import IHC.AST
import IHC.Builtins (builtinEnv, buildConEnv, buildFieldEnv)
import IHC.Classes (ClassRegistry, newClassRegistry, registerInstance)
import IHC.Cpp (cppPreprocess, defaultCppContext)
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
cppSource :: Source -> IO Source
cppSource src = do
    bs' <- cppPreprocess defaultCppContext (srcName src) (srcBytes src)
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
loadProgramFromSource :: [FilePath] -> Source -> IO (Env, Thunk)
loadProgramFromSource searchPath src0 = do
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
    discoverInModule registry searchPath entry "main"

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
    aliases <- buildAliases registry searchPath entry slots qualPairs
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
        registry <- newIORef Map.empty
        -- Load the target module (and its transitive imports).
        targetLm <- loadModule registry searchPath (impModule imp)
        -- Discover ALL exported top-level names in the target module.
        allNames <- scanAllTopLevelNames (lmSource targetLm)
        let exported = filter (exportsName (lmHeader targetLm))
                     $ filter (specAllows (impSpec imp)) allNames
        mapM_ (discoverInModule registry searchPath targetLm) exported
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
            bareAliases
                | impQualified imp = []
                | otherwise =
                    [ (n, t)
                    | n <- exported
                    , Just t <- [Map.lookup (modPrefix <> n) thunkByKey]
                    ]
            qualAliases
                | BC.null qualPrefix = []
                | otherwise =
                    [ (qualPrefix <> n, t)
                    | n <- exported
                    , Just t <- [Map.lookup (modPrefix <> n) thunkByKey]
                    ]
        let aliasEnv = Map.fromList (bareAliases ++ qualAliases)
        -- The final env visible to imported bindings: qualEnv + aliases.
        -- We also include the aliases so that intra-module cross-references
        -- (e.g. `greet` calling `suffix`) resolve properly.
        let innerEnv = Map.union aliasEnv qualEnv
        mapM_ (\((_, rhs), slot) ->
                   writeIORef slot (Unevaluated (Closure innerEnv emptyIPMap rhs)))
              (zip qualPairs slots)
        -- Merge into the REPL env: prefer existing REPL bindings (shadow).
        -- Report the count of NEW bare (or qualified-alias) names added so
        -- the REPL can print "imported Foo (N names)".
        let newBindings = Map.union qualEnv aliasEnv
            additions   = Map.difference newBindings existingEnv
            merged      = Map.union existingEnv additions
            newAliases  = Map.fromList (bareAliases ++ qualAliases)
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
        [ (keyPrefix <> n, transform e)
        | (n, e) <- Map.toList bs
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
                directPairs <- directRewritePairs tm
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

    -- | @(bare-name, fully-qualified-key)@ pairs for names defined
    -- directly in a module.
    directRewritePairs tm = do
        bodiesMap <- readIORef (lmBodies tm)
        let prefix  = lmName tm <> BC.pack "."
            allNames = Map.keys bodiesMap
            exported = filter (exportsNameDirect (lmHeader tm)) allNames
        pure [(n, prefix <> n) | n <- exported]

    -- | @(bare-name, fully-qualified-key)@ pairs from a re-exported
    -- module (@module Foo@ in the export list).
    rewritePairsFromReexport reg modName =
        case Map.lookup modName reg of
            Just (Loaded reLm) -> directRewritePairs reLm
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
    -> LoadedModule
    -> [Thunk]
    -> [(ByteString, Expr)]
    -> IO Env
buildAliases registry _searchPath entry slots qualPairs = do
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

    -- | Return @(bare-name, Thunk)@ pairs for names defined and exported
    -- directly by a loaded module (not via re-exports).
    namesFromModule thunkByKey tm = do
        bodiesMap <- readIORef (lmBodies tm)
        let prefix  = lmName tm <> BC.pack "."
            allN    = Map.keys bodiesMap
            exported = filter (exportsNameDirect (lmHeader tm)) allN
        pure [ (n, t)
             | n <- exported
             , Just t <- [Map.lookup (prefix <> n) thunkByKey]
             ]

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
    -> ModuleName
    -> IO LoadedModule
loadModule registry searchPath name = do
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
                    src  <- cppSource src0
                    (mHeader, _) <- parseModuleHeader src startCursor
                    let header = fromMaybe emptyHeader mHeader
                        declared = fromMaybe name (mhName header)
                    modifyIORef' registry (Map.insert name Loading)
                    lm <- buildLoadedModule declared False header src
                    modifyIORef' registry (Map.insert name (Loaded lm))
                    pure lm

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

-- | Return the source root(s) of the cached @base@ package, if present.
-- base's @.cabal@ file omits @hs-source-dirs@, so the package root is
-- itself the source root.  Returns an empty list if the cache directory
-- does not exist (caller falls through to the normal "module not found"
-- error path).
baseCacheSearchPath :: IO [FilePath]
baseCacheSearchPath = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "ihc" </> "sources" </> "base-4.19.0.0"
    exists <- doesDirectoryExist dir
    pure (if exists then [dir] else [])

-- | Given a dotted module name, search each entry in @searchPath@ for
-- a matching file.  On miss, falls back to the base source cache at
-- @~\/.cache\/ihc\/sources\/base-4.19.0.0\/@.  Raises 'ModuleNotFound'
-- only when both the user search path AND the base cache miss.
locateModule :: [FilePath] -> ModuleName -> IO FilePath
locateModule searchPath name = do
    baseDirs <- baseCacheSearchPath
    go (searchPath ++ baseDirs)
  where
    candidates = modulePathCandidates name
    go []     = throwIO (ModuleNotFound name)
    go (d:ds) = tryCands d candidates ds
    tryCands _ []     rest = go rest
    tryCands d (c:cs) rest = do
        let p = d </> c
        exists <- doesFileExist p
        if exists then pure p else tryCands d cs rest

--------------------------------------------------------------------------------
-- Demand-driven discovery
--------------------------------------------------------------------------------

-- | Recursively discover bindings reachable from @name@ inside the
-- given module, following imports whenever the name isn't local.
discoverInModule
    :: ModuleRegistry
    -> [FilePath]
    -> LoadedModule
    -> ByteString
    -> IO ()
discoverInModule registry searchPath lm name
    -- Qualified name (contains a dot and the prefix looks like a module
    -- alias)? Route to the target module directly.
    | Just (qual, bareName) <- splitQualified name = do
        mTarget <- resolveQualified registry searchPath lm qual
        case mTarget of
            Just targetLm ->
                discoverInModule registry searchPath targetLm bareName
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
                        mapM_ (discoverInModule registry searchPath lm)
                              (freeVars expr)
                    Nothing -> do
                        -- Not local. Try imports.
                        mForeign <- resolveImport registry searchPath lm name
                        case mForeign of
                            Just () -> pure ()
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
    -> LoadedModule
    -> ByteString
    -> IO (Maybe LoadedModule)
resolveQualified registry searchPath lm qual = do
    let imports = mhImports (lmHeader lm)
    case filter (importMatchesQual qual) imports of
        (imp:_) -> Just <$> loadModule registry searchPath (impModule imp)
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
    -> LoadedModule
    -> ByteString
    -> IO (Maybe ())
resolveImport registry searchPath lm name = do
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
            targetLm <- loadModule registry searchPath (impModule imp)
            mLhs <- findOrResolveLhs (lmSource targetLm)
                                     (lmKnown targetLm) name
            case mLhs of
                Just _ ->
                    -- Export-list check: the target module must
                    -- actually export @name@.
                    if exportsName (lmHeader targetLm) name
                        then do
                            discoverInModule registry searchPath targetLm name
                            pure (Just ())
                        else tryImports rest
                Nothing ->
                    -- The name isn't defined locally in targetLm.
                    -- Check whether targetLm re-exports it via
                    -- `module Foo` entries in its export list.
                    followModuleReexports targetLm rest

    -- | Chase every `module Foo` entry in the export list of @via@ to
    -- see whether any of them provides @name@.  We recurse through
    -- 'discoverInModule' so the transitive load chain is followed
    -- automatically.
    followModuleReexports via rest = do
        let reexportedMods = moduleReexports (lmHeader via)
        tryReexports reexportedMods rest

    tryReexports [] rest = tryImports rest
    tryReexports (modName:mods) rest = do
        reLm <- loadModule registry searchPath modName
        mLhs <- findOrResolveLhs (lmSource reLm) (lmKnown reLm) name
        case mLhs of
            Just _ ->
                if exportsName (lmHeader reLm) name
                    then do
                        discoverInModule registry searchPath reLm name
                        pure (Just ())
                    else tryReexports mods rest
            Nothing ->
                -- Go one level deeper if reLm itself has module re-exports.
                followModuleReexports reLm [] >>= \case
                    Just ()  -> pure (Just ())
                    Nothing  -> tryReexports mods rest

specAllows :: ImportSpec -> ByteString -> Bool
specAllows ImportAll         _ = True
specAllows (ImportOnly ns)   n = n `elem` ns
specAllows (ImportHiding ns) n = n `notElem` ns

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
