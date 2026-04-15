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
import System.Directory (doesFileExist)
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
               writeIORef slot (Unevaluated (Closure env rhs)))
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
                   writeIORef slot (Unevaluated (Closure innerEnv rhs)))
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
    expr <- Parser.parseBodyExprWithFixity
                (lmSource lm) (lmFixity lm) (lhsClauses lhs)
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
                bodiesMap <- readIORef (lmBodies tm)
                let allNames = Map.keys bodiesMap
                    visible  = filter (specAllows (impSpec imp)) allNames
                    exported = filter (exportsName (lmHeader tm)) visible
                    targetPrefix = lmName tm <> BC.pack "."
                    bare | impQualified imp = []
                         | otherwise =
                             [(n, targetPrefix <> n) | n <- exported]
                    qualRef = case impAlias imp of
                        Just a  -> Just (a <> BC.pack ".")
                        Nothing
                            | impQualified imp -> Just (lmName tm <> BC.pack ".")
                            | otherwise        -> Nothing
                    qual = case qualRef of
                        Just p  -> [(p <> n, targetPrefix <> n) | n <- exported]
                        Nothing -> []
                pure (bare ++ qual)
            _ -> pure []

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

    patBound (PVar n)    = [n]
    patBound (PCon _ ps) = concatMap patBound ps
    patBound (PAs n p)   = n : patBound p
    patBound (PBang p)   = patBound p
    patBound (PTuple ps) = concatMap patBound ps
    patBound _           = []

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
                    bodiesMap <- readIORef (lmBodies tm)
                    let allNames = Map.keys bodiesMap
                        visible  = filter (specAllows (impSpec imp)) allNames
                        exported = filter (exportsName (lmHeader tm)) visible
                        prefix   = lmName tm <> BC.pack "."
                        -- The name the user writes to reach this import:
                        -- @B.@ if qualified with alias B, @Bar.@ if
                        -- qualified with no alias, empty if unqualified.
                        qualPrefix = case impAlias imp of
                            Just a  -> a <> BC.pack "."
                            Nothing
                                | impQualified imp -> lmName tm <> BC.pack "."
                                | otherwise        -> BC.empty
                        -- Qualified imports don't contribute bare aliases.
                        bareAliases
                            | impQualified imp = []
                            | otherwise =
                                [ (n, t)
                                | n <- exported
                                , Just t <- [Map.lookup (prefix <> n) thunkByKey]
                                ]
                        qualAliases
                            | BC.null qualPrefix = []
                            | otherwise =
                                [ (qualPrefix <> n, t)
                                | n <- exported
                                , Just t <- [Map.lookup (prefix <> n) thunkByKey]
                                ]
                    pure (bareAliases ++ qualAliases)
                _ -> pure []

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

-- | Modules whose names are backed entirely by the builtin environment.
-- We return empty stub modules so import declarations don't fail at file lookup.
isBuiltinBackedModule :: ModuleName -> Bool
isBuiltinBackedModule n =
       "GHC."         `BC.isPrefixOf` n
    || n == "Prelude"
    || n == "System.IO"
    || n == "System.IO.Unsafe"
    || n == "System.Exit"
    || "Foreign."     `BC.isPrefixOf` n
    || n == "Data.IORef"
    || n == "Data.Int"
    || n == "Data.Word"
    || n == "Data.Bits"
    || n == "Data.Char"
    || n == "Data.List"
    || n == "Data.Maybe"
    || n == "Data.Ord"
    || n == "Control.Monad"
    || n == "Control.DeepSeq"
    || "Data.Map"     `BC.isPrefixOf` n
    || "Data.Set"     `BC.isPrefixOf` n
    || "Data.IntMap"  `BC.isPrefixOf` n
    || "Data.Sequence" `BC.isPrefixOf` n

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
-- a matching file. Raises 'ModuleNotFound' on miss.
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
                        expr <- Parser.parseBodyExprWithFixity
                                    (lmSource lm)
                                    (lmFixity lm)
                                    (lhsClauses lhs)
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
                Nothing -> tryImports rest

specAllows :: ImportSpec -> ByteString -> Bool
specAllows ImportAll         _ = True
specAllows (ImportOnly ns)   n = n `elem` ns
specAllows (ImportHiding ns) n = n `notElem` ns

exportsName :: ModuleHeader -> ByteString -> Bool
exportsName h n = case mhExports h of
    ExportAll     -> True
    ExportList xs -> any matchExport xs
  where
    matchExport (ExportName m)   = n == m
    matchExport (ExportType m _) = n == m

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
    patBound (PVar n)    = [n]
    patBound (PCon _ ps) = concatMap patBound ps
    patBound (PAs n p)   = n : patBound p
    patBound (PBang p)   = patBound p
    patBound (PTuple ps) = concatMap patBound ps
    patBound _           = []
