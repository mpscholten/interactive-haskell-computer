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
import Data.List (isPrefixOf, sortOn)
import Control.Monad (forM_, foldM, when)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import qualified System.IO
import System.IO.Unsafe (unsafePerformIO)
import IHC.AST
import IHC.Builtins (builtinEnv, buildConEnv, buildFieldEnv, showValWith, stringToListValIO)
import IHC.CabalProject
    ( cachedPackageSearchPath, cachedPackageSearchPathWithIncludes
    , cachedPackageTable, pkgExtraLibs
    )
import IHC.Diagnostics (warnStub)
import IHC.Classes
    ( ClassRegistry, newClassRegistry, registerInstance, lookupInstance
    , lookupInstanceMethod, typeTagOf
    , scanHookRef, sharedClassRegRef, setSharedClassReg
    , unionInstanceScope, currentInstanceScope
    , setEnvFallback
    , setCoreInstanceLoadHook
    , setClassMethodFallback
    , setThExpToExpr
    )
import IHC.Cpp (cppPreprocessWithIncludes, defaultCppContext)
import IHC.Eval (force, apply, forceMethodVal, ownerSentinelKey)
import qualified IHC.FFI as FFI
import IHC.Lexer (startCursor)
import IHC.ModuleHeader
import qualified IHC.Parser as Parser
import IHC.Parser (FixityTable, defaultFixityTable, scanFixityDecls, ParseError)
import IHC.Scan
import IHC.Source
import IHC.TH (expandSplicesInExpr, thExpandSpliceDecl, thExpToExpr)
import qualified IHC.TypeAST
import IHC.TypeGlobals (globalTypeSigsRef, globalTypeSynonymsRef, globalClassMethodNamesRef, seedBuiltinClassMethodSigs)
import qualified IHC.TypeReduce as TR
import IHC.Val

-- | Merge data registries from many loaded modules. The interpreter's
-- current constructor environment is keyed by bare constructor name, so two
-- packages can collide on names such as @WriteBuffer@. When that happens,
-- keep the entry with the larger arity; record construction and saturated
-- constructor application depend on the arity, and selecting a shorter
-- unrelated constructor causes over-application at runtime.
unionDataRegistries :: [DataRegistry] -> DataRegistry
unionDataRegistries =
    foldr (Map.unionWith preferDataEntry) Map.empty
  where
    preferDataEntry a@(_, arityA, _) b@(_, arityB, _)
        | arityA >= arityB = a
        | otherwise        = b

-- | Merge field registries without dropping duplicate-record-field clauses.
-- The map value is a dispatch list, so plain 'Map.union' is wrong: it keeps
-- only one module's clauses for a field name. Earlier registries have
-- priority when duplicate bare constructor names collide.
unionFieldRegistries :: [FieldRegistry] -> FieldRegistry
unionFieldRegistries =
    foldl merge Map.empty
  where
    merge acc reg = Map.unionWith appendPreferred acc reg
    appendPreferred xs ys = xs ++ filter (`notElem` xs) ys

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
    -- Use 'withBytes' (which threads through 'mkSource') instead of a
    -- record-update of 'srcBytes': otherwise the post-CPP 'Source'
    -- would inherit the pre-CPP 'srcLineStarts' AND the pre-CPP
    -- 'srcScanCache'.  The stale cache silently returns scan results
    -- computed on the unprocessed bytes — most damagingly, missing
    -- 'data' constructors that were originally hidden behind a CPP
    -- @#if@.  withBytes allocates fresh values for both fields.
    pure (withBytes src bs')

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
    -- Install the demand-driven env fallback for this program run so
    -- that 'IHC.Eval.eval' can resolve FQN misses via the global
    -- module catalogue.  See 'installEnvFallbackHook'.
    installEnvFallbackHook
    cacheWithIncludes0 <- cachedPackageSearchPathWithIncludes
    let fullSearchPath0 = searchPath ++ map fst cacheWithIncludes0
        includeMap0     = Map.fromList cacheWithIncludes0
    setGlobalSearchPath fullSearchPath0 includeMap0
    -- Auto-dlopen per-package cbits dylibs (IHC_CBITS_DIR).  See
    -- buildBaseEnv for the REPL-path counterpart.  Needed so that
    -- `foreign import ccall "_hs_text_measure_off"` etc. resolve via
    -- the nix-built libhs<pkg>-cbits.dylib.
    FFI.registerCbitsDylibs
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
    -- Install as the shared reg so the ETypedMethod evaluator path +
    -- on-demand elaborator can consult it at runtime.
    setSharedClassReg classReg
    -- Fallback: if 'resolveTypedMethod' can't resolve (cls, tag, method)
    -- it consults this hook to get a value-directed dispatcher, so
    -- ambiguous type annotations don't hard-error when the tag points
    -- at an instance we haven't loaded (e.g. @return 42 :: ST s Int@).
    setClassMethodFallback (\cls method ->
        pure (Just (classMethodDispatcher classReg cls method)))
    -- Seed the type-sig registry with canonical class method sigs
    -- (pure, return, mempty, minBound, maxBound).
    seedBuiltinClassMethodSigs

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

    -- Discover transitively-referenced ENTRY-MODULE bindings.
    -- 'discoveryFreeVars' deliberately doesn't descend into function
    -- arguments (to keep implicit-Prelude tractable for programs that
    -- only reach builtin names), so a binding like
    -- @main = print (sumStrict 0 [1..10])@ never reports @sumStrict@
    -- as a free var even though it must be discovered.  Walk the
    -- entry module's discovered bodies with the *deep* 'freeVars'
    -- iterator, and discover any free var that is itself a top-level
    -- binding in the entry source (cheap: bounded by entry-module
    -- size).  Repeat to a fixed point so transitive references like
    -- @sumStrict@ → @helper@ → @inner@ all resolve.  Names not
    -- defined in the entry source are left to the existing fallback
    -- chain (builtins, imports, Prelude); we only chase locals.
    entryTopLevels <- Set.fromList <$> scanAllTopLevelNames (lmSource entry)
    let discoverEntryLocal n =
            discoverInModuleWith earlyBuiltinNames registry fullSearchPath
                                 includeMap entry n
                `catch` (\(_ :: SomeException) -> pure ())
        chaseLocals seen = do
            bodies <- readIORef (lmBodies entry)
            let allFvs = Set.fromList
                    [ fv
                    | expr <- Map.elems bodies
                    , fv   <- freeVars expr
                    ]
                newLocals = Set.toList
                    (Set.intersection entryTopLevels allFvs
                       `Set.difference` (seen `Set.union` Map.keysSet bodies))
            case newLocals of
                [] -> pure ()
                _  -> do
                    mapM_ discoverEntryLocal newLocals
                    chaseLocals (Set.union seen (Set.fromList newLocals))
    chaseLocals Set.empty

    -- Force-load every module the entry source imports.  Without this,
    -- @import M (T(..))@ where the user only uses some constructor of T
    -- never triggers a 'loadModule' call for M (the qualified-FQN
    -- discovery path needs a @M.foo@ shape; bare ctor refs go through
    -- the bare-name fallback which only scans modules already in the
    -- registry).  Result: @TextNode@ from `data Node = Node | TextNode`
    -- exported from M reads as 'unbound variable' even though both
    -- @import M (Node(..))@ and the source-level data decl are
    -- correct.  Eager-load every import after entry-module discovery so
    -- the global module catalogue is populated before any FV lookup.
    -- Errors are swallowed (best-effort) — a missing dependency should
    -- surface as an unbound-variable error at use site, not abort the
    -- whole load.
    let entryImports = map impModule (mhImports (lmHeader entry))
    forM_ entryImports $ \m -> do
        _ <- try (loadModule registry fullSearchPath includeMap m)
                :: IO (Either SomeException LoadedModule)
        pure ()

    -- Force-load a small set of core modules that provide fundamental
    -- typeclass instances (Functor/Applicative/Monad for [], Maybe,
    -- Either; Show/Eq/Ord for primitives; etc.).  Without this, a
    -- fixture like @main = print (fmap (+10) [1,2,3])@ never triggers
    -- loading of @GHC.Internal.Base@ because every FV in its body
    -- (@print@, @fmap@, numeric ops) short-circuits via the builtin
    -- name set — and an instance that isn't scanned is an instance
    -- that won't register.  These modules are cheap to parse and
    -- their instance decls are needed for dispatch-time lookups to
    -- succeed.
    let coreInstanceModules =
            [ BC.pack "GHC.Internal.Base"
            , BC.pack "GHC.Internal.Show"
            , BC.pack "GHC.Internal.Enum"
            , BC.pack "GHC.Internal.Ix"
            , BC.pack "GHC.Internal.Num"
            , BC.pack "GHC.Internal.Real"
            , BC.pack "GHC.Internal.Maybe"
            -- Language.Haskell.TH.Quote declares 'data QuasiQuoter = QuasiQuoter
            -- { quoteExp, quotePat, quoteType, quoteDec }'.  QuasiQuoter-providing
            -- libraries like ihp-hsx record-construct values like
            -- @hsx = customHsx (HsxSettings…)@ where @customHsx@'s body is
            -- @QuasiQuoter { quoteExp = … }@.  Without this force-load the
            -- demand-driven loader doesn't visit TH.Quote so the constructor
            -- isn't registered, and the record-construction emits 'EVar
            -- "QuasiQuoter"' which is unbound.
            , BC.pack "Language.Haskell.TH.Quote"
            -- Text.Megaparsec.Internal declares 'instance MonadParsec
            -- e s (ParsecT e s m)' — the only MonadParsec instance
            -- ihp-hsx and most users actually exercise.  The
            -- corresponding methods (takeWhileP, satisfy, …) only
            -- bind through this instance.  Lazy loading visits the
            -- file (so 'scanDataDecls' fires) AFTER the
            -- 'registerInstancesFrom' pass has run, so its instances
            -- never get registered.  Force-load it.
            , BC.pack "Text.Megaparsec.Internal"
            , BC.pack "Text.Megaparsec.Class"
            ]
    forM_ coreInstanceModules $ \m -> do
        r <- try (loadModule registry fullSearchPath includeMap m)
                :: IO (Either SomeException LoadedModule)
        case r of
            Right _ -> pure ()
            Left  _ -> pure ()   -- best-effort; keep going if a module is absent

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
    let unionedData  = unionDataRegistries (map lmDataReg loadedModules)
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
    fieldEnv <- buildFieldAccessorEnv loadedModules publicFields unionedFields
    builtins <- builtinEnv classReg
    -- Install a thunk per scanned @foreign import ccall@ declaration
    -- under a synthetic @__ffi.Module.name@ key. Module bodies reach
    -- these through sentinel @EVar@ entries inserted in 'buildLoadedModule'.
    ffiEnv   <- buildForeignEnv loadedModules fullSearchPath
    let baseNoClass = Map.union builtins (Map.union fieldEnv (Map.union conEnv ffiEnv))
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
    mapM_ (expandSplicesInModule registry fullSearchPath includeMap base) loadedModules

    -- Build (fully-qualified-name, Expr) pairs for every loaded body.
    qualPairs <- concat <$> mapM (exportBodies registry fullSearchPath includeMap (Map.keysSet builtins)) loadedModules

    -- Tie the knot for all bodies at once.
    slots <- mapM (\_ -> newIORef (BlackHole "<import-placeholder>")) qualPairs
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
    let builtinBareName k =
            case BC.elemIndexEnd (toEnum (fromEnum '.')) k of
                Just idx -> BC.drop (idx + 1) k
                Nothing  -> k
        alwaysBuiltinNames =
            Set.union ffiBuiltinNames
                (Set.fromList
                    [">>=", ">>", "return", "pure", "fmap", "<*>", "void"
                    , "catch", "handle", "try", "evaluate"
                    , "mask", "mask_", "uninterruptibleMask", "uninterruptibleMask_"
                    , "block", "unblock", "unsafeUnmask", "allowInterrupt", "interruptible"
                    , "bracket", "bracket_", "bracketOnError", "finally", "onException"
                    , "unIO", "ioToST", "unsafeIOToST", "stToIO", "unsafeSTToIO"
                    , "forkIO", "fromThreadId"
                    , "create", "createAndTrim", "createFp", "createFpAndTrim"
                    , "newForeignPtr", "addForeignPtrFinalizer"
                    , "newUnique", "hashUnique"
                    , "socket", "setSocketOption", "listen", "accept", "getSocketName", "bind", "sendBuf", "recvBuf", "mallocBytes", "free", "closeFdWith", "fdSocket", "unsafeFdSocket"
                    , "getSystemEventManager", "getSystemTimerManager"
                    , "registerTimeout", "unregisterTimeout", "updateTimeout"
                    , "withHandle", "withHandleKillThread"
                    , "labelThread", "labelThreadByteArray#"
                    , "settingsHost", "settingsPort"
                    , "putStrLn", "putStr", "print"
                    , "hPutStrLn", "hPutStr", "hGetLine", "hFlush"
                    , "stdout", "stderr", "stdin"
                    ])
        builtinOverrides =
            Map.filterWithKey
                (\k _ -> Set.member (builtinBareName k) alwaysBuiltinNames)
                builtins
        -- Import aliases should not overwrite base entries such as
        -- class-method dispatchers.  Network.Socket.Info.getAddrInfo is a
        -- class selector; replacing its bare dispatcher with an alias to the
        -- fully-qualified selector creates a self-loop.
        aliasesWithoutBase = Map.difference aliases base
        envWithAliases = Map.union builtinOverrides (Map.union aliasesWithoutBase qualEnv)
    let env = envWithAliases

    -- Each body's closure gets the @"$$owner"@ sentinel pointing at
    -- the module that owns the binding (extracted from the FQN's
    -- module prefix), so 'IHC.Eval.currentOwner' can scope the
    -- unqualified-name fallback to that module's import declarations
    -- (Haskell 2010 §5.5).  Sub-closures that extend this env (lambdas,
    -- lets) inherit the sentinel automatically.
    mapM_ (\((fqn, rhs), slot) -> do
               let ownerName = case BC.elemIndexEnd (toEnum (fromEnum '.')) fqn of
                       Just idx -> BC.take idx fqn
                       Nothing  -> lmName entry
               ownerThunk <- newWHNFThunk (VStr ownerName)
               let envWithOwner = Map.insert ownerSentinelKey ownerThunk env
               writeIORef slot (Unevaluated (Closure envWithOwner emptyIPMap rhs)))
          (zip qualPairs slots)

    -- Seed the env-fallback's base env so any 'resolveFallback'-built
    -- Closure can reach builtins + class dispatchers + constructors.
    writeIORef envBaseForFallbackRef env
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
    registerDerivedEnumBoundedInstances classReg loadedModules

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
    -- Install this as the REPL's shared class registry.  Instances
    -- registered by subsequent imports are written here so the
    -- dispatcher's lookup fallback can see them (Haskell 2010 §4.3.2:
    -- instances from the transitive import closure are in scope).
    setSharedClassReg classReg
    -- Seed canonical class method sigs (pure/return/mempty/...).
    seedBuiltinClassMethodSigs
    -- Install the demand-driven env-fallback hook so that
    -- 'IHC.Eval.eval' can resolve fully-qualified references lazily on
    -- EVar miss.  See 'installEnvFallbackHook' for the mechanics.
    installEnvFallbackHook
    cacheWithIncludes0 <- cachedPackageSearchPathWithIncludes
    setGlobalSearchPath (map fst cacheWithIncludes0)
                        (Map.fromList cacheWithIncludes0)
    -- Auto-discover per-package cbits dylibs.  The nix build emits one
    -- @libhs<pkg>-cbits.dylib@ per Hackage package that declares
    -- @c-sources:@ in its cabal; the path is exported as
    -- @IHC_CBITS_DIR@.  dlopen every such dylib we find there so that
    -- @foreign import ccall "_hs_text_measure_off"@ and friends resolve
    -- via RTLD_DEFAULT without any package-specific hardcoding.
    FFI.registerCbitsDylibs
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
    -- Pre-discover 'QuasiQuoter' from Language.Haskell.TH.Quote so
    -- libraries like ihp-hsx can build their @hsx :: QuasiQuoter@
    -- record-construction without the user having to explicitly
    -- import the TH module.  See 'preDiscoverTHQuoteConstructors'
    -- for why this matters for QQ dispatch.
    env3 <- preDiscoverTHQuoteConstructors env2
                `catch` (\(_ :: SomeException) -> pure env2)
    -- Seed the env-fallback's base env so Closures built by
    -- 'resolveFallback' can reach builtins + class dispatchers.
    writeIORef envBaseForFallbackRef env3
    -- Install the core-instance load hook: on the first elaborator
    -- lookup miss ('IHC.Eval.resolveTypedMethod'), force-load
    -- GHC.Internal.Base / Maybe / … so their Applicative / Monad /
    -- Functor dicts are in the registry.  One-shot (guarded by an
    -- IORef flag); later calls are free.  Kept out of startup so the
    -- bare REPL prompt stays fast for users who never use type
    -- annotations.
    installCoreInstanceLoadHook classReg env3
    -- Install the class-method fallback: if resolveTypedMethod can't
    -- find an instance even after loading core dicts, return the
    -- dispatcher so value-directed lookup can still run.
    setClassMethodFallback (\cls method ->
        pure (Just (classMethodDispatcher classReg cls method)))
    -- Install the TH Exp -> Expr decoder so that QuasiQuoter dispatch
    -- in 'IHC.Eval' can convert the Val produced by @quoteExp@ into an
    -- 'Expr' to evaluate.  Lives in 'IHC.TH' which already depends on
    -- 'IHC.Eval'; the hook breaks the would-be cycle.
    setThExpToExpr thExpToExpr
    pure (env3, classReg)

-- | Install a one-shot hook that force-loads core instance modules
-- (@GHC.Internal.Base@ and friends) and registers their instance
-- dictionaries.  Keeps REPL startup fast: the modules aren't touched
-- until the elaborator's 'IHC.Eval.resolveTypedMethod' hits its first
-- lookup miss (e.g. @pure 42 :: Maybe Int@ at the bare prompt).  The
-- hook captures the REPL's 'ClassRegistry' so the registrations land in
-- the same reg the dispatcher reads.  After the first successful call
-- the flag is flipped and further invocations short-circuit.
installCoreInstanceLoadHook :: ClassRegistry -> Env -> IO ()
installCoreInstanceLoadHook classReg baseEnv = do
    doneRef <- newIORef False
    let hook = do
            done <- readIORef doneRef
            if done
              then pure ()
              else do
                  writeIORef doneRef True
                  r <- try (loadCoreInstanceModules classReg baseEnv)
                          :: IO (Either SomeException ())
                  case r of
                      Right () -> pure ()
                      Left  _  -> pure ()   -- best-effort
    setCoreInstanceLoadHook hook

-- | Force-load the canonical set of "instance-bearing" modules and
-- register the instances they declare.  Called at most once per REPL
-- session via 'installCoreInstanceLoadHook'.
loadCoreInstanceModules :: ClassRegistry -> Env -> IO ()
loadCoreInstanceModules classReg baseEnv = do
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs      = map fst cacheWithIncludes
        includeMap     = Map.fromList cacheWithIncludes
        fullSearchPath = cacheDirs
        coreModules =
            [ BC.pack "GHC.Internal.Base"
            , BC.pack "GHC.Internal.Maybe"
            , BC.pack "GHC.Internal.Data.Either"
            ]
    registry <- newIORef Map.empty
    loaded <- mapM
        (\m -> do
            r <- try (loadModule registry fullSearchPath includeMap m)
                     :: IO (Either SomeException LoadedModule)
            case r of
                Right lm -> pure (Just lm)
                Left  _  -> pure Nothing)
        coreModules
    let lms = [lm | Just lm <- loaded]
    unionInstanceScope (Set.fromList (map lmName lms))
    classTable <- buildClassMethodTable lms
    let tyCtors = foldr Map.union Map.empty (map lmTypeCtorReg lms)
    mapM_ (registerInstancesFrom registry fullSearchPath includeMap
                                 classReg tyCtors classTable baseEnv) lms
    -- Also mirror into the shared reg (matches the loadImport path).
    mSharedReg <- readIORef sharedClassRegRef
    case mSharedReg of
        Just sharedReg | sharedReg /= classReg ->
            mapM_ (registerInstancesFrom registry fullSearchPath includeMap
                                         sharedReg tyCtors classTable baseEnv) lms
        _ -> pure ()

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
    let unionedData = unionDataRegistries dataRegs
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

-- | Source-load 'Language.Haskell.TH.Quote' so its 'QuasiQuoter' record
-- type — the data constructor + the four field accessors @quoteExp@ /
-- @quotePat@ / @quoteType@ / @quoteDec@ — are available in env without
-- the user's program needing an explicit @import@.
--
-- Why pre-load: a 'QuasiQuoter'-providing library like @ihp-hsx@ has
-- @hsx :: QuasiQuoter@ defined as @customHsx (HsxSettings…)@; the body
-- of @customHsx@ is a record-construction
-- @QuasiQuoter { quoteExp = …, quotePat = …, … }@.  When the evaluator
-- reaches that 'ERecordCon' it does @EVar "QuasiQuoter"@ to look up
-- the constructor.  Without this pre-load the demand loader hasn't
-- visited @Language.Haskell.TH.Quote@ yet so the constructor is
-- unbound, killing every @[hsx|…|]@ / @[sql|…|]@ / etc.
--
-- Mirrors 'preDiscoverPreludeConstructors' but ALSO seeds the field
-- accessors via 'buildFieldAccessorEnv' since 'QuasiQuoter' has named
-- record fields.
preDiscoverTHQuoteConstructors :: Env -> IO Env
preDiscoverTHQuoteConstructors env0 = do
    cacheWithIncludes <- cachedPackageSearchPathWithIncludes
    let cacheDirs  = map fst cacheWithIncludes
        includeMap = Map.fromList cacheWithIncludes
    registry <- newIORef Map.empty
    let modName = BC.pack "Language.Haskell.TH.Quote"
    r <- try (loadModule registry cacheDirs includeMap modName)
            :: IO (Either SomeException LoadedModule)
    case r of
        Left  _  -> pure env0   -- best-effort: missing cache shouldn't break startup
        Right lm -> do
            let dataReg = lmDataReg lm
                fieldReg = lmFieldReg lm
            conEnv <- buildConEnv dataReg
            -- Treat the module's full field registry as both "public"
            -- and "all" — there's only one module in scope here, so
            -- there's no public/private distinction to draw.
            fieldEnv <- buildFieldAccessorEnv [lm] fieldReg fieldReg
            pure (Map.union env0 (Map.union conEnv fieldEnv))

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
    let unionedData   = unionDataRegistries (map lmDataReg loadedModules)
        (publicFields, unionedFields) = partitionFieldRegistries loadedModules
        unionedTypeCtors = foldr Map.union Map.empty (map lmTypeCtorReg loadedModules)
        unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                         (map lmTypeFamilies loadedModules)
    TR.setGlobalRegistry unionedTFReg
    conEnv    <- buildConEnv  unionedData
    fieldEnv' <- buildFieldAccessorEnv loadedModules publicFields unionedFields
    builtins  <- builtinEnv classReg
    -- Foreign import dispatchers (see parallel call in loadImportOnlyIntoEnv).
    ffiEnv    <- buildForeignEnv loadedModules fullSearchPath
    let baseNoClass = Map.union builtins (Map.union fieldEnv' (Map.union conEnv ffiEnv))
    classMethodEnv <- buildClassMethodEnv classReg baseNoClass loadedModules
    let base = Map.union classMethodEnv baseNoClass
    -- Phase 2.11: expand TH splices.
    mapM_ (expandSplicesInModule registry fullSearchPath includeMap base) loadedModules
    -- Build (key, Expr) pairs.  Entry module bindings are keyed bare.
    qualPairs <- concat <$> mapM (exportBodies registry fullSearchPath includeMap (Map.keysSet builtins)) loadedModules
    -- Tie the knot.
    slots <- mapM (\_ -> newIORef (BlackHole "<import-placeholder>")) qualPairs
    let qualEnv = extendEnvMany (zip (map fst qualPairs) slots) base
    -- Aliases: imported libs get bare+qualified aliases in the entry scope.
    aliases <- buildAliases registry fullSearchPath includeMap entry slots qualPairs
    let innerEnv = Map.union aliases qualEnv
    -- Per-body owner sentinel — see 'loadProgramFromSource' for the
    -- analogous block in the run-from-source path.  The owner is
    -- extracted from the FQN's module prefix; entries that aren't
    -- module-prefixed default to the entry module.
    mapM_ (\((fqn, rhs), slot) -> do
               let ownerName = case BC.elemIndexEnd (toEnum (fromEnum '.')) fqn of
                       Just idx -> BC.take idx fqn
                       Nothing  -> lmName entry
               ownerThunk <- newWHNFThunk (VStr ownerName)
               let envWithOwner = Map.insert ownerSentinelKey ownerThunk innerEnv
               writeIORef slot (Unevaluated (Closure envWithOwner emptyIPMap rhs)))
          (zip qualPairs slots)
    -- Register type-class instances.
    do { classTable <- buildClassMethodTable loadedModules; mapM_ (registerInstancesFrom registry fullSearchPath includeMap classReg unionedTypeCtors classTable innerEnv) loadedModules }
    registerClassDefaults registry fullSearchPath includeMap classReg innerEnv loadedModules
    registerDerivedFunctorInstances classReg loadedModules
    registerDerivedEnumInstances    classReg loadedModules
    registerDerivedBoundedInstances classReg loadedModules
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

type ExportNamesMemo = IORef (Map ModuleName [ByteString])

newExportNamesMemo :: IO ExportNamesMemo
newExportNamesMemo = newIORef Map.empty

-- | Enumerate the runtime-visible export names of a module without forcing
-- any binding bodies. This is the cheap import-time surface used by the
-- lazy @ImportAll@ path: each returned name gets a placeholder thunk whose
-- first force materializes the real binding via @ImportOnly@.
effectiveExportNames
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]
    -> IO [ByteString]
effectiveExportNames registry searchPath includeMap lm visited = do
    memo <- newExportNamesMemo
    effectiveExportNamesMemo memo registry searchPath includeMap lm visited

effectiveExportNamesMemo
    :: ExportNamesMemo
    -> ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> [ModuleName]
    -> IO [ByteString]
effectiveExportNamesMemo memo registry searchPath includeMap lm visited = do
    cached <- Map.lookup (lmName lm) <$> readIORef memo
    case cached of
        Just result -> pure result
        Nothing -> do
            result <- compute
            modifyIORef' memo (Map.insert (lmName lm) result)
            pure result
  where
    compute = case mhExports (lmHeader lm) of
        ExportAll -> localRuntimeNames lm
        ExportList items -> nubBS . concat <$> mapM exportItem items

    exportItem item = case item of
        ExportName n ->
            pure [visibleExportName n]
        ExportType ty mbSubs -> do
            let visibleSubs = maybe [] (map visibleExportName) mbSubs
            case splitQualified ty of
                Just (qual, bareTy) -> do
                    mTarget <- resolveQualified registry searchPath includeMap lm qual
                    case mTarget of
                        Just targetLm -> typeLikeRuntimeNames targetLm bareTy visibleSubs
                        Nothing       -> pure visibleSubs
                Nothing ->
                    -- Type heads are not value bindings. The only runtime-visible
                    -- names from an export-type item are its constructors /
                    -- fields or class methods when the export names a class.
                    typeLikeRuntimeNames lm ty visibleSubs
        ExportModule m
            | m `elem` visited -> pure []
            | otherwise -> do
                reLm <- loadModule registry searchPath includeMap m
                            `catch` (\(_ :: ModuleNotFound) ->
                                warnMissingStub m searchPath
                                    >> buildEmptyStubModule m)
                effectiveExportNamesMemo memo registry searchPath includeMap reLm
                    (m : visited)

localRuntimeNames :: LoadedModule -> IO [ByteString]
localRuntimeNames lm = do
    topLevelNames <- scanAllTopLevelNames (lmSource lm)
    classDecls    <- scanClassDecls (lmSource lm)
    let classMethods = concatMap classMethodNames classDecls
        ctors        = Map.keys (lmDataReg lm)
        fields       = Map.keys (lmFieldReg lm)
        foreignNames = map FFI.fdName (lmForeignDecls lm)
    pure (nubBS (topLevelNames ++ classMethods ++ ctors ++ fields ++ foreignNames))

visibleExportName :: ByteString -> ByteString
visibleExportName n =
    case splitQualified n of
        Just (_, bare) -> bare
        Nothing        -> n

typeLikeRuntimeNames :: LoadedModule -> ByteString -> [ByteString] -> IO [ByteString]
typeLikeRuntimeNames lm ty subs = do
    classDecls <- scanClassDecls (lmSource lm)
    let ctorsOfTy = Map.findWithDefault [] ty (lmTypeCtorReg lm)
        fieldsOfTy =
            [ field
            | (field, ctorIdxs) <- Map.toList (lmFieldReg lm)
            , any (\(ctor, _) -> ctor `elem` ctorsOfTy) ctorIdxs
            ]
        classMethods =
            case [ classMethodNames decl
                 | decl <- classDecls
                 , classClassName decl == ty
                 ] of
                (methods:_) -> methods
                []          -> []
    pure $ case subs of
        [] -> nubBS (ctorsOfTy ++ fieldsOfTy ++ classMethods)
        _  -> nubBS
                [ n
                | n <- subs
                , n `elem` ctorsOfTy || n `elem` fieldsOfTy || n `elem` classMethods
                ]

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
-- Builtin-backed modules install their host-provided bindings directly.
--
-- Source-backed @ImportOnly@ requests stay targeted: discover just the
-- requested names, tie the needed env, and register any instances they
-- pull in.
--
-- Source-backed @ImportAll@ / @ImportHiding@ requests are now lazy: we
-- enumerate the module's effective export NAMES (walking headers and
-- export lists only), then install one lazy slot per visible name. The
-- first force of that slot re-enters the targeted @ImportOnly@ path for
-- the specific name. This avoids parsing the bodies of every exported
-- binding during @import Data.Map.Strict@-style dense re-export imports.
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
    | isBuiltinBackedModule (impModule imp) = do
        -- For builtin-backed modules we don't parse source. But if the
        -- builtin env carries FQN-keyed bindings for this module (e.g.
        -- "Data.ByteString.length"), install them under the caller's
        -- chosen qualifier so `BS.length` resolves.
        classReg <- newClassRegistry
        builtins <- builtinEnv classReg
        let modName = impModule imp
            prefix  = modName <> BC.pack "."
            fqnMap  = Map.filterWithKey (\k _ -> BC.isPrefixOf prefix k) builtins
        if Map.null fqnMap
            then pure (existingEnv, 0)
            else do
                let qualPrefix = case impAlias imp of
                        Just a                    -> a <> BC.pack "."
                        Nothing | impQualified imp -> prefix
                                | otherwise        -> BC.empty
                    aliasUnder p =
                        [ (p <> BC.drop (BC.length prefix) k, slot)
                        | (k, slot) <- Map.toList fqnMap
                        ]
                    bareAliases
                        | impQualified imp = []
                        | otherwise        = aliasUnder BC.empty
                    qualAliases
                        | BC.null qualPrefix = []
                        | otherwise          = aliasUnder qualPrefix
                    additions = Map.fromList
                                    (bareAliases ++ qualAliases)
                                    `Map.difference` existingEnv
                    merged    = Map.union existingEnv additions
                pure (merged, Map.size additions)
    | ImportOnly names <- impSpec imp = loadImportOnlyIntoEnv searchPath imp names existingEnv
    | otherwise = do
        -- Append cached package search dirs so mtl, transformers, etc. resolve.
        cacheWithIncludes <- cachedPackageSearchPathWithIncludes
        let cacheDirs      = map fst cacheWithIncludes
            includeMap     = Map.fromList cacheWithIncludes
            fullSearchPath = searchPath ++ cacheDirs
        registry <- newIORef Map.empty
        targetLm <- loadModule registry fullSearchPath includeMap (impModule imp)
        -- Track transitive import closure for Haskell 2010 §4.3.2
        -- instance visibility. The dispatcher consults this set on miss.
        headerCache <- newHeaderCache
        closure <- transitiveImportClosure headerCache fullSearchPath includeMap
                        (impModule imp)
        unionInstanceScope closure
        exportNames0 <- effectiveExportNames registry fullSearchPath includeMap targetLm
                            [lmName targetLm]
        let exportNames = [ n | n <- exportNames0, specAllows (impSpec imp) n ]
            qualPrefix = case impAlias imp of
                Just a  -> a <> BC.pack "."
                Nothing
                    | impQualified imp -> lmName targetLm <> BC.pack "."
                    | otherwise        -> BC.empty
            importOne n = imp { impSpec = ImportOnly [n] }
            lookupKeys n =
                nubBS ([ qualPrefix <> n | not (BC.null qualPrefix) ] ++ [n])
            resolveOne n = newLazyBuiltinThunk $ do
                (env', _) <- loadImportIntoEnv searchPath (importOne n) existingEnv
                let mThunk = mapMaybe (`Map.lookup` env') (lookupKeys n)
                case mThunk of
                    (t:_) -> force t
                    []    -> error
                        ( "loadImportIntoEnv: failed to materialize `"
                          <> BC.unpack (impModule imp) <> "."
                          <> BC.unpack n <> "` on demand" )
        slots <- mapM resolveOne exportNames
        let exportPairs = zip exportNames slots
            bareAliases
                | impQualified imp = []
                | otherwise        = exportPairs
            qualAliases
                | BC.null qualPrefix = []
                | otherwise =
                    [ (qualPrefix <> n, t) | (n, t) <- exportPairs ]
            newBindings = Map.fromList (bareAliases ++ qualAliases)
            additions   = Map.difference newBindings existingEnv
            merged      = Map.union existingEnv additions
        pure (merged, Map.size additions)

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
    earlyBuiltins <- builtinEnv classReg
    let earlyBuiltinNames = Map.keysSet earlyBuiltins
    mapM_ (discoverInModuleWith earlyBuiltinNames registry fullSearchPath includeMap targetLm) requested
    -- Preload direct imports of targetLm so class declarations in
    -- re-export chains become visible to 'buildClassMethodEnv'.
    -- Without this, a request like `import qualified Data.Monoid as M`
    -- + `M.mempty` fails: Data.Monoid re-exports Monoid from
    -- GHC.Internal.Base, but `discoverInModuleWith` on the method
    -- name `mempty` never loads GHC.Internal.Base (there's no
    -- top-level body named `mempty` in any direct import), so the
    -- class decl isn't scanned and 'classMethodEnv' is empty for
    -- ambient methods.  Loading direct imports is bounded (one level
    -- per call) and, per 'isLocalCacheModule', still blocks the bare
    -- GHC.* cascade.
    -- BFS-load modules reachable from targetLm's imports, up to a
    -- bounded total to avoid fan-out in dense graphs like Data.Text.
    let preloadBudget = 40 :: Int
    let preloadBFS remaining seenSet frontier
            | remaining <= 0 = pure ()
            | null frontier = pure ()
            | otherwise = do
                let (thisBatch, rest) = splitAt remaining frontier
                next <- fmap concat $ mapM (\modName ->
                    if Set.member modName seenSet
                      then pure []
                      else do
                        r <- try (loadModule registry fullSearchPath includeMap modName)
                                 :: IO (Either SomeException LoadedModule)
                        case r of
                            Right lm' -> pure (map impModule (mhImports (lmHeader lm')))
                            Left  _   -> pure []) thisBatch
                let loadedThisPass = length thisBatch
                    seenSet' = Set.union seenSet (Set.fromList thisBatch)
                    frontier' = nubBS (rest ++ filter (not . (`Set.member` seenSet')) next)
                preloadBFS (remaining - loadedThisPass) seenSet' frontier'
    preloadBFS preloadBudget (Set.singleton (lmName targetLm))
        (map impModule (mhImports (lmHeader targetLm)))
    -- ImportOnly is the REPL's deferred-name path: keep it targeted.
    -- Preloading every discovered dependency's full export surface defeats
    -- the point and makes requests like Prelude.map bulk-load GHC.Base's
    -- entire ExportAll set before the prompt can return.
    -- Compute transitive-import closure (H2010 §4.3.2 instance visibility).
    -- Header-only — cheap — and reused by the scope filter below.
    headerCacheForClosure <- newHeaderCache
    closure <- transitiveImportClosure headerCacheForClosure fullSearchPath includeMap
                    (impModule imp)
    unionInstanceScope closure
    reg0 <- readIORef registry
    let loadedModules0 = [ lm | (_, Loaded lm) <- Map.toList reg0 ]
        unionedData    = unionDataRegistries (map lmDataReg loadedModules0)
        (publicFields, unionedFields) = partitionFieldRegistries loadedModules0
        unionedTypeCtors0 = foldr Map.union Map.empty (map lmTypeCtorReg loadedModules0)
        unionedTFReg = foldr (Map.unionWith (++)) Map.empty
                         (map lmTypeFamilies loadedModules0)
    TR.setGlobalRegistry unionedTFReg
    conEnv    <- buildConEnv unionedData
    fieldEnv' <- buildFieldAccessorEnv loadedModules0 publicFields unionedFields
    builtins  <- builtinEnv classReg
    -- Foreign import dispatcher thunks keyed as @__ffi.Module.name@.
    -- Required so that bodies referencing `foreign import ccall` names
    -- (e.g. Data.Text.Internal.Measure's @measure_off@) can be
    -- dispatched via libffi when forced at eval time.  Without this,
    -- the sentinel EVar inserted by buildLoadedModule points at a key
    -- that isn't in the env.
    ffiEnv   <- buildForeignEnv loadedModules0 fullSearchPath
    let baseNoClass = Map.union builtins (Map.union fieldEnv' (Map.union conEnv ffiEnv))
    classMethodEnv <- buildClassMethodEnv classReg baseNoClass loadedModules0
    let baseForImport = Map.union classMethodEnv baseNoClass
    mapM_ (expandSplicesInModule registry fullSearchPath includeMap baseForImport) loadedModules0
    qualPairs <- concat <$> mapM (exportBodies registry fullSearchPath includeMap (Map.keysSet builtins)) loadedModules0
    slots <- mapM (\_ -> newIORef (BlackHole "<import-placeholder>")) qualPairs
    -- For builtin-backed stubs with no qualPairs, synthesize alias
    -- slots for any requested name whose FQN has a builtin binding.
    -- This makes e.g. `BS.length` resolve to the host `Data.ByteString.length`
    -- shim when Data.ByteString is on the builtin-backed list.
    let synthFromBuiltin n =
            let fqn = lmName targetLm <> BC.pack "." <> n
                -- Data constructors live in 'conEnv' under their
                -- BARE name (the scanner in 'scanDataDecls' stores
                -- @TkConId@ literals — no module prefix).  So a
                -- qualified REPL request like @M.Nothing@ misses
                -- the FQN lookup above even though 'baseForImport'
                -- contains @Data.Maybe@'s @Nothing@ via @conEnv@.
                -- Bridge to the bare-name lookup, gated on a
                -- capitalized first char so we never hand out
                -- unrelated lowercase bindings that share a name.
                capStart = case BC.uncons n of
                    Just (c, _) -> c >= 'A' && c <= 'Z'
                    Nothing     -> False
            in case Map.lookup fqn baseForImport of
                Just slot -> pure (Just (n, slot))
                Nothing | capStart
                        , Just slot <- Map.lookup n conEnv
                        -> pure (Just (n, slot))
                Nothing   ->
                    -- Record selectors are synthesized into fieldEnv'
                    -- rather than exported as source bodies, so an
                    -- ImportOnly request for a selector like runStateT
                    -- must be able to surface the prebuilt accessor.
                    case Map.lookup n fieldEnv' of
                        Just slot -> pure (Just (n, slot))
                        Nothing   ->
                            -- Class methods are ambient: they have no single
                            -- owning module key, only the bare method name in
                            -- 'classMethodEnv'.  If an import requests a class
                            -- method (e.g. `import qualified Data.Monoid as M`
                            -- → `M.mempty`), fall back to the bare name here so
                            -- `M.mempty` resolves to the dispatcher.  Ordinary
                            -- top-level bindings are keyed under their FQN in
                            -- 'baseForImport' and are NOT eligible for this
                            -- fallback.
                            case Map.lookup n classMethodEnv of
                                Just slot -> pure (Just (n, slot))
                                Nothing   -> pure Nothing
    requestedStandard0 <- mapM (resolveRequestedPair targetLm qualPairs slots) requested
    let preferBuiltinRequested n resolved
            | Set.member n ffiBuiltinNames
            , Just slot <- Map.lookup n baseForImport
            = Just (n, slot)
            | otherwise
            = resolved
        requestedStandard =
            zipWith preferBuiltinRequested requested requestedStandard0
    requestedFromBuiltins <- mapM synthFromBuiltin
        [ n | (n, Nothing) <- zip requested requestedStandard ]
    let requestedPairs =
            mapMaybe id requestedStandard ++ mapMaybe id requestedFromBuiltins
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
    aliases <- buildAliases registry fullSearchPath includeMap targetLm slots qualPairs
    rewriteAliasPairs <- concat <$> mapM (rewriteAliases registry fullSearchPath includeMap thunkByKey (Map.keysSet builtins)) loadedModules0
    let selfAliases =
            [ (n, slot)
            | (qualKey, slot) <- Map.toList thunkByKey
            , BC.isPrefixOf modPrefix qualKey
            , let n = BC.drop (BC.length modPrefix) qualKey
            ]
        builtinBareName k =
            case BC.elemIndexEnd (toEnum (fromEnum '.')) k of
                Just idx -> BC.drop (idx + 1) k
                Nothing  -> k
        alwaysBuiltinNames =
            Set.union ffiBuiltinNames
                (Set.fromList
                    [">>=", ">>", "return", "pure", "fmap", "<*>", "void"
                    , "catch", "handle", "try", "evaluate"
                    , "mask", "mask_", "uninterruptibleMask", "uninterruptibleMask_"
                    , "block", "unblock", "unsafeUnmask", "allowInterrupt", "interruptible"
                    , "bracket", "bracket_", "bracketOnError", "finally", "onException"
                    , "unIO", "ioToST", "unsafeIOToST", "stToIO", "unsafeSTToIO"
                    , "socket", "setSocketOption", "listen", "accept", "getSocketName", "bind", "mallocBytes", "free", "closeFdWith", "fdSocket", "unsafeFdSocket"
                    , "getSystemEventManager", "getSystemTimerManager"
                    , "registerTimeout", "unregisterTimeout", "updateTimeout"
                    , "withHandle", "withHandleKillThread"
                    , "labelThread", "labelThreadByteArray#"
                    , "settingsHost", "settingsPort"
                    , "putStrLn", "putStr", "print"
                    , "hPutStrLn", "hPutStr", "hGetLine", "hFlush"
                    , "stdout", "stderr", "stdin"
                    ])
        builtinOverrides =
            Map.filterWithKey
                (\k _ -> Set.member (builtinBareName k) alwaysBuiltinNames)
                builtins
        -- innerEnv (see the parallel note in 'loadImportIntoEnv'): we
        -- include @existingEnv@ as the lowest-priority layer so that
        -- REPL-level pre-discoveries (e.g. the GHC.Exception helpers
        -- primed by 'buildBaseEnv') remain reachable from inside the
        -- imported bindings.
        innerEnv = Map.union builtinOverrides
                 $ Map.union (Map.fromList selfAliases)
                 $ Map.union (Map.fromList requestedPairs)
                 $ Map.union (Map.fromList rewriteAliasPairs)
                 $ Map.union aliases
                 $ Map.union qualEnv existingEnv
    -- Per-body owner sentinel for scoped fallback (see comment in
    -- 'loadProgramFromSource').
    mapM_ (\((fqn, rhs), slot) -> do
               let ownerName = case BC.elemIndexEnd (toEnum (fromEnum '.')) fqn of
                       Just idx -> BC.take idx fqn
                       Nothing  -> impModule imp
               ownerThunk <- newWHNFThunk (VStr ownerName)
               let envWithOwner = Map.insert ownerSentinelKey ownerThunk innerEnv
               writeIORef slot (Unevaluated (Closure envWithOwner emptyIPMap rhs)))
          (zip qualPairs slots)
    -- H2010 §4.3.2 scope filter: modules not in the user's import
    -- closure ('closure' computed above) MUST NOT contribute instances.
    -- We reuse the same closure for the pre-discovery pass and the
    -- register-instance calls so both stay in sync.
    let inUserScope lm' =
            let n = lmName lm'
            in Set.member n closure || n == impModule imp
        instanceScope = filter inUserScope loadedModules0
    do { classTable <- buildClassMethodTable instanceScope; mapM_ (registerInstancesFrom registry fullSearchPath includeMap classReg unionedTypeCtors0 classTable innerEnv) instanceScope }
    registerClassDefaults registry fullSearchPath includeMap classReg innerEnv instanceScope
    registerDerivedFunctorInstances classReg instanceScope
    registerDerivedEnumBoundedInstances classReg instanceScope
    -- ALSO mirror instance registrations into the REPL's shared class
    -- registry so the dispatcher (closed over the shared reg via
    -- 'sharedClassRegRef') can find them on later dispatch calls.
    -- Without this, instances registered here into the per-call
    -- 'classReg' are invisible to the REPL's dispatcher thunks.
    mSharedReg <- readIORef sharedClassRegRef
    case mSharedReg of
        Just sharedReg | sharedReg /= classReg -> do
            ct <- buildClassMethodTable instanceScope
            mapM_ (registerInstancesFrom registry fullSearchPath includeMap sharedReg unionedTypeCtors0 ct innerEnv) instanceScope
            registerClassDefaults registry fullSearchPath includeMap sharedReg innerEnv instanceScope
            registerDerivedFunctorInstances sharedReg instanceScope
            registerDerivedEnumBoundedInstances sharedReg instanceScope
        _ -> pure ()
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

    rewriteAliases registry searchPath includeMap thunkByKey builtinNames lm = do
        rw <- buildImportRewrites False registry searchPath includeMap lm builtinNames
        pure
            [ (alias, slot)
            | (alias, targetKey) <- Map.toList rw
            , BC.elem '.' alias
            , Just slot <- [Map.lookup targetKey thunkByKey]
            ]

-- | Phase 2.11: expand TH splices in all bodies of a loaded module.
-- Mutates the @lmBodies@ IORef in place.
expandSplicesInModule
    :: IORef (Map ModuleName ModuleState)
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> Env
    -> LoadedModule
    -> IO ()
expandSplicesInModule registry searchPath includeMap spliceEnv lm = do
    -- Stage 1: expand ESplice nodes nested inside existing body exprs.
    bodies0 <- readIORef (lmBodies lm)
    expanded0 <- mapM (expandSplicesInExpr spliceEnv emptyIPMap 0) bodies0
    writeIORef (lmBodies lm) expanded0

    -- Stage 2 (Phase 2.13 wiring): evaluate top-level $(...) splices
    -- and merge the decoded [(Name, Expr)] into lmBodies.
    spans <- scanTopLevelSplices (lmSource lm)
    case spans of
        [] -> pure ()
        _  -> do
            newPairs <- concat <$> mapM evalOneSplice spans
            case newPairs of
                [] -> pure ()
                _  -> do
                    bodiesNow <- readIORef (lmBodies lm)
                    let merged = Map.union (Map.fromList newPairs) bodiesNow
                    expanded1 <- mapM (expandSplicesInExpr spliceEnv emptyIPMap 0) merged
                    writeIORef (lmBodies lm) expanded1
  where
    -- Build a let-wrapper over ALL bodies currently parsed into the
    -- module so the splice evaluator can reach same-module helpers
    -- like `mySplice`. Normal discovery is driven from `main`, so
    -- splice-only helpers would otherwise be unparsed — we first
    -- force-discover the splice's free vars, then snapshot bodies.
    wrapWithLocals e = do
        b <- readIORef (lmBodies lm)
        pure $ case Map.toList b of
            []    -> e
            binds -> ELet binds e

    evalOneSplice :: Span -> IO [(Name, Expr)]
    evalOneSplice sp@(start, end)
        | end <= start = pure []
        | otherwise = do
            let innerBytes = sliceBytes (lmSource lm) sp
                src = mkSource ("<splice:" <> srcName (lmSource lm) <> ">") innerBytes
            spliceExpr <- Parser.parseExprOnly src (lmFixity lm)
            -- Pre-discover free vars of the splice expr in the module
            -- so same-module helpers like `mySplice` get parsed into
            -- lmBodies before we wrap and evaluate.
            let fvs = freeVars spliceExpr
            mapM_ (\fv -> do
                    r <- try (discoverInModule registry searchPath includeMap lm fv)
                             :: IO (Either SomeException ())
                    case r of
                        Right () -> pure ()
                        Left _   -> pure ()
                ) fvs
            spliceExprExp <- expandSplicesInExpr spliceEnv emptyIPMap 0 spliceExpr
            wrapped <- wrapWithLocals spliceExprExp
            thExpandSpliceDecl spliceEnv emptyIPMap wrapped

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
    rewrites0 <- buildImportRewritesForNames registry lm instMethodFVs
    -- Single-shot retry: some FVs may now resolve thanks to transitive
    -- loads triggered by the per-FV discoverInModule pass just above.
    -- For each FV still missing from the rewrite map, try discover
    -- once more and rebuild.  Bounded: one extra pass, no fixpoint.
    let unresolvedFVs =
            [ fv | fv <- Set.toList instMethodFVs, not (Map.member fv rewrites0) ]
    rewrites <- if null unresolvedFVs
        then pure rewrites0
        else do
            mapM_ (\fv -> do
                      r <- try (discoverInModule registry searchPath includeMap lm fv)
                                :: IO (Either SomeException ())
                      case r of
                          Right () -> pure ()
                          Left  _  -> pure ())
                  unresolvedFVs
            buildImportRewritesForNames registry lm instMethodFVs
    -- After the per-FV discovery passes above have mutated 'lmBodies' for
    -- every rewrite target, the target FQNs exist in their owning
    -- modules' bodies, but the env we'll pass to 'evalMethodWithLazy'
    -- ('env') was snapshotted earlier (line 1246 in 'loadImportOnlyIntoEnv')
    -- and has no slot for those FQNs.  Add one slot per rewrite target
    -- that (a) is an FQN, (b) names a body we've already discovered, and
    -- (c) isn't already in 'env'.  This is NOT pre-discovery beyond what
    -- the retry just did — it's materialising env slots for bodies the
    -- retry already produced.  Knot-tie so an augmented-slot body can
    -- reference other augmented slots.
    -- Build a name-keyed method table. When the class declaration is
    -- known, the class's declared method names are canonical for
    -- dispatch. Extra instance bindings are preserved under their own
    -- names so non-standard extensions don't crash registration, but
    -- dispatch only consults the class-declared names. If the class
    -- declaration isn't available, keep every method the instance
    -- provided so the legacy fallback still works.
    let methodMap = Map.fromList methods
        evalMethodIn = evalOneMethodWith env
    methodVals <- case Map.lookup cls classTable of
        Just classMethods -> do
            let classMethodSet = Set.fromList classMethods
                extraMethods =
                    [ (mn, lhs)
                    | (mn, lhs) <- methods
                    , not (Set.member mn classMethodSet)
                    ]
            classEntries <- mapM (\mn -> do
                    v <- evalMethodIn rewrites mn (Map.lookup mn methodMap)
                    pure (mn, v))
                classMethods
            extraEntries <- mapM (\(mn, lhs) -> do
                    v <- evalMethodIn rewrites mn (Just lhs)
                    pure (mn, v))
                extraMethods
            pure (Map.fromList (classEntries ++ extraEntries))
        Nothing ->
            Map.fromList <$>
                mapM (\(mn, lhs) -> do
                    v <- evalMethodIn rewrites mn (Just lhs)
                    pure (mn, v))
                methods
    -- Register under the head type name (used by Bool/Int/Char/String
    -- dispatch via 'typeTagOf' specializations).  Qualified instance heads
    -- like @FoldCase B.ByteString@ intentionally keep their qualified key so
    -- strict and lazy modules with the same abstract type name do not collide.
    registerInstance classReg cls typ methodVals
    -- Also register under every runtime data constructor of that type so
    -- that 'typeTagOf (VCon n _) = n' lookups succeed.  For qualified type
    -- heads, resolve the qualifier through the owning module's imports and
    -- chase type re-exports to the module that defines the constructors.
    ctors <- instanceRuntimeCtors typ
    mapM_ (\ctor -> registerInstance classReg cls ctor methodVals) ctors
  where
    evalOneMethodWith _e _rw _mn Nothing = pure methodPlaceholder
    evalOneMethodWith e rw _mn (Just lhs) = do
        r <- try (evalMethodWithLazy e lm rw (_mn, lhs)) :: IO (Either SomeException Val)
        case r of
            Right v -> pure v
            Left  _ -> pure methodPlaceholder

    instanceRuntimeCtors ty =
        case splitQualified ty of
            Just (qual, bareTy) -> do
                mTarget <- resolveQualified registry searchPath includeMap lm qual
                case mTarget of
                    Just targetLm ->
                        findRuntimeCtorsForType registry searchPath includeMap Set.empty targetLm bareTy
                    Nothing -> pure []
            Nothing ->
                pure (Map.findWithDefault [] ty typeCtors)

findRuntimeCtorsForType
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> Set ByteString
    -> LoadedModule
    -> ByteString
    -> IO [ByteString]
findRuntimeCtorsForType registry searchPath includeMap seen lm ty
    | lmName lm `Set.member` seen = pure []
    | otherwise =
        case Map.lookup ty (lmTypeCtorReg lm) of
            Just ctors | not (null ctors) -> pure ctors
            _ -> searchImports (mhImports (lmHeader lm))
  where
    seen' = Set.insert (lmName lm) seen

    searchImports imps = do
        candidates <- loadCandidates imps
        let localProviders =
                [ targetLm
                | targetLm <- candidates
                , hasLocalTypeCtors targetLm
                ]
            preferredLocalProviders =
                [ targetLm
                | targetLm <- localProviders
                , moduleNamePrefix (lmName lm) (lmName targetLm)
                ]
        case preferredLocalProviders ++ localProviders of
            (targetLm:_) ->
                findRuntimeCtorsForType registry searchPath includeMap seen' targetLm ty
            [] -> searchProvided candidates

    loadCandidates [] = pure []
    loadCandidates (imp:rest)
        | not (specAllows (impSpec imp) ty) = loadCandidates rest
        | otherwise = do
            loaded <- try (loadModule registry searchPath includeMap (impModule imp))
                        :: IO (Either SomeException LoadedModule)
            more <- loadCandidates rest
            case loaded of
                Left _         -> pure more
                Right targetLm -> pure (targetLm : more)

    searchProvided [] = pure []
    searchProvided (targetLm:rest)
        | directlyProvidesType targetLm = do
            ctors <- findRuntimeCtorsForType registry searchPath includeMap seen' targetLm ty
            if null ctors then searchProvided rest else pure ctors
        | otherwise = searchProvided rest

    hasLocalTypeCtors targetLm =
        case Map.lookup ty (lmTypeCtorReg targetLm) of
            Just ctors -> not (null ctors)
            Nothing    -> False

    directlyProvidesType targetLm =
        hasLocalTypeCtors targetLm
        || case mhExports (lmHeader targetLm) of
            ExportList _ -> exportsNameDirect targetLm ty
            ExportAll    -> False

    moduleNamePrefix prefix name =
        prefix == name
        || (prefix <> BC.pack ".") `BC.isPrefixOf` name

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

-- | Like 'evalMethodWith' but does NOT force the thunk.  Returns a
-- 'VLazyMethod' wrapper which the class-method dispatcher
-- ('classMethodDispatcher' -> 'forceMethodVal') forces on demand.
--
-- Deferring the force to dispatch time sidesteps the env-snapshot bug:
-- at registration we may not yet have populated every transitively-
-- required binding in the caller's env slots, but by the time the
-- user's expression actually invokes the method, all relevant
-- 'discoverInModule' work has run (either via the per-FV pre-pass in
-- 'registerOne' itself, or via later REPL-level discovery), so the
-- thunk's captured env resolves successfully.
evalMethodWithLazy :: Env -> LoadedModule -> Map ByteString ByteString -> (ByteString, BindingLhs) -> IO Val
evalMethodWithLazy env lm rewrites (_, lhs) = do
    expr0 <- Parser.parseBodyExprWithFixity
                (lmSource lm) (lmFixity lm) (lhsClauses lhs)
    let expr1 = desugarRecordPats (lmFieldReg lm)
                 (desugarRecordCons (lmFieldReg lm) expr0)
        expr  = if Map.null rewrites then expr1 else rewriteExpr rewrites expr1
    t <- newThunk env expr
    pure (VLazyMethod t)

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
    -- Self-rewrites: instance method bodies reference sibling top-level
    -- names in the OWNING module under their bare name; rewrite them
    -- to the module's FQN so the env fallback can resolve them lazily.
    -- Example: @instance Monad [] where xs >>= f = [y | x <- xs, y <- f x]@
    -- in GHC.Internal.Base desugars to a body referencing @concatMap@,
    -- which is a local GHC.Internal.Base binding.  Without this rewrite
    -- the bare @concatMap@ misses both the caller's env AND the env
    -- fallback (which refuses bare names for namespace integrity), so
    -- the method body fails and the dispatcher falls through to IO.
    selfPairs <- if lmIsEntry lm
        then pure []
        else do
            let prefix = lmName lm <> BC.pack "."
            allScanned <- scanAllTopLevelNames (lmSource lm)
                            `catch` (\(_ :: SomeException) -> pure [])
            pure [ (n, prefix <> n)
                 | n <- allScanned
                 , Set.member n needed
                 ]
    -- Import pairs take priority over self-rewrites: if a name is both
    -- locally defined and imported from elsewhere, the import wins
    -- (GHC semantics).  Map.fromList keeps the last occurrence.
    pure (Map.fromList (selfPairs ++ filteredImportPairs))
  where
    rewritesForImport reg needed' imp
        = case Map.lookup (impModule imp) reg of
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
            fieldExported =
                [ n
                | n <- names
                , Map.member n (lmFieldReg tm)
                , not (lmNoFieldSelectors tm)
                , exportsNameDirect tm n
                ]
            localPairs = [(n, prefix <> n) | n <- nubBS (localExported ++ fieldExported)]
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
                       || (Map.member n (lmFieldReg srcLm)
                           && not (lmNoFieldSelectors srcLm)
                           && exportsNameDirect srcLm n)
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
        let hits = filter (elem (BC.pack "Functor") . fdDerivClasses) decls
        mapM_ (registerOneFunctor classReg) hits

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
            mInnerFmap0 <- lookupInstanceMethod classReg (BC.pack "Functor") innerTag (BC.pack "fmap")
            mInnerFmap <- case mInnerFmap0 of
                Nothing -> pure Nothing
                Just v' -> do
                    r <- try (forceMethodVal v') :: IO (Either SomeException Val)
                    case r of
                        Right v'' -> pure (Just v'')
                        Left _    -> pure Nothing
            case mInnerFmap of
                Just innerFmap -> do
                    stepT <- newWHNFThunk v
                    r1 <- apply innerFmap fT
                    r2 <- apply r1 stepT
                    newWHNFThunk r2
                _ -> pure t   -- no Functor instance; leave field untouched
        _ -> pure t

--------------------------------------------------------------------------------
-- Deriving Enum / Bounded synthesis
--------------------------------------------------------------------------------

registerDerivedEnumBoundedInstances :: ClassRegistry -> [LoadedModule] -> IO ()
registerDerivedEnumBoundedInstances classReg loadedModules =
    mapM_ oneModule loadedModules
  where
    oneModule lm = do
        decls <- scanSimpleDerivings (lmSource lm)
        mapM_ (registerOne lm) decls

    registerOne lm (SimpleDerivDecl tyName classes) = do
        let ctors = nullaryCtorOrder lm tyName
        when (not (null ctors)) $ do
            when (BC.pack "Bounded" `elem` classes) $
                registerBounded tyName ctors
            when (BC.pack "Enum" `elem` classes) $
                registerEnum tyName ctors

    registerBounded tyName ctors = do
        existing <- lookupInstance classReg (BC.pack "Bounded") tyName
        case existing of
            Just _  -> pure ()
            Nothing -> do
                let methods = Map.fromList
                        [ (BC.pack "minBound", VCon (head ctors) [])
                        , (BC.pack "maxBound", VCon (last ctors) [])
                        ]
                registerUnderTypeAndCtors (BC.pack "Bounded") tyName ctors methods

    registerEnum tyName ctors = do
        existing <- lookupInstance classReg (BC.pack "Enum") tyName
        case existing of
            Just _  -> pure ()
            Nothing -> do
                let ctorIndex = Map.fromList (zip ctors [0 :: Int ..])
                    methods = Map.fromList
                        [ (BC.pack "fromEnum", derivedFromEnum ctorIndex)
                        , (BC.pack "toEnum", derivedToEnum ctors)
                        ]
                registerUnderTypeAndCtors (BC.pack "Enum") tyName ctors methods

    registerUnderTypeAndCtors cls tyName ctors methods = do
        registerInstance classReg cls tyName methods
        mapM_ (\ctor -> registerInstance classReg cls ctor methods) ctors

    nullaryCtorOrder lm tyName =
        let dreg = lmDataReg lm
            ctors = Map.findWithDefault [] tyName (lmTypeCtorReg lm)
            annotated =
                [ (idx, ctor)
                | ctor <- ctors
                , Just (owner, arity, idx) <- [Map.lookup ctor dreg]
                , owner == tyName
                , arity == 0
                ]
        in map snd (sortOn fst annotated)

    derivedFromEnum ctorIndex = VFun $ \xT -> do
        xv <- force xT
        case xv of
            VCon ctor _ ->
                case Map.lookup ctor ctorIndex of
                    Just idx -> pure (VInt (fromIntegral idx))
                    Nothing  -> error ("derived Enum.fromEnum: unknown constructor "
                                      <> BC.unpack ctor)
            other -> error ("derived Enum.fromEnum: expected constructor, got "
                          <> showValForDebug other)

    derivedToEnum ctors = VFun $ \iT -> do
        iv <- force iT
        case iv of
            VInt n
                | n >= 0
                , fromIntegral n < length ctors ->
                    pure (VCon (ctors !! fromIntegral n) [])
                | otherwise ->
                    error ("derived Enum.toEnum: index out of range " <> show n)
            other -> error ("derived Enum.toEnum: expected Int, got "
                          <> showValForDebug other)

shortShow :: Val -> String
shortShow (VCon n _) = "VCon " <> BC.unpack n
shortShow (VInt _)   = "VInt"
shortShow (VFloat _) = "VFloat"
shortShow (VChar _)  = "VChar"
shortShow (VStr _)   = "VStr"
shortShow _          = "<other>"

--------------------------------------------------------------------------------
-- Derived 'Enum' / 'Bounded' instance synthesis
--
-- For @data T = C1 | C2 | ... | Cn deriving (Enum, Bounded)@ (all
-- constructors nullary — the only shape GHC allows for stock Enum/Bounded
-- on sum types), we synthesize instance dictionaries that match the
-- semantics in GHC.Internal.Enum:
--
--   * @fromEnum Ci = i-1@  (0-indexed in source order)
--   * @toEnum i   = C(i+1)@
--   * @minBound   = C1@
--   * @maxBound   = Cn@
--
-- Registration mirrors 'registerDerivedFunctorInstances' — the instance
-- table is keyed both under the type name @T@ (for type-annotation
-- dispatch like @maxBound :: T@) and under each constructor name (so a
-- value-directed dispatch @fromEnum someCtor@ finds the table via
-- 'typeTagOf' → ctor-name lookup).
--------------------------------------------------------------------------------

-- | Synthesise derived @Enum@ instances for every data decl whose
-- deriving clause lists @Enum@ and whose constructors are all nullary.
-- Types that don't match both predicates are skipped silently.
registerDerivedEnumInstances :: ClassRegistry -> [LoadedModule] -> IO ()
registerDerivedEnumInstances classReg loadedModules =
    mapM_ oneModule loadedModules
  where
    oneModule lm = do
        decls <- scanFunctorDerivings (lmSource lm)
        let hits =
                [ (fdTyName d, [fcName c | c <- fdCtors d])
                | d <- decls
                , elem (BC.pack "Enum") (fdDerivClasses d)
                , all (null . fcRoles) (fdCtors d)      -- all-nullary
                , not (null (fdCtors d))                -- at least one ctor
                ]
        mapM_ (registerOneEnum classReg) hits

-- | Synthesise derived @Bounded@ instances for every data decl whose
-- deriving clause lists @Bounded@ and whose constructors are all nullary.
registerDerivedBoundedInstances :: ClassRegistry -> [LoadedModule] -> IO ()
registerDerivedBoundedInstances classReg loadedModules =
    mapM_ oneModule loadedModules
  where
    oneModule lm = do
        decls <- scanFunctorDerivings (lmSource lm)
        let hits =
                [ (fdTyName d, [fcName c | c <- fdCtors d])
                | d <- decls
                , elem (BC.pack "Bounded") (fdDerivClasses d)
                , all (null . fcRoles) (fdCtors d)
                , not (null (fdCtors d))
                ]
        mapM_ (registerOneBounded classReg) hits

registerOneEnum :: ClassRegistry -> (ByteString, [ByteString]) -> IO ()
registerOneEnum classReg (tyName, ctors) = do
    let enumCls    = BC.pack "Enum"
        fromEnumV  = synthFromEnumForCtors ctors
        toEnumV    = synthToEnumForCtors   ctors
        methods    = Map.fromList
                        [ (BC.pack "fromEnum", fromEnumV)
                        , (BC.pack "toEnum",   toEnumV)
                        ]
    existing <- lookupInstance classReg enumCls tyName
    case existing of
        Just _  -> pure ()
        Nothing -> do
            registerInstance classReg enumCls tyName methods
            mapM_ (\c -> registerInstance classReg enumCls c methods) ctors

registerOneBounded :: ClassRegistry -> (ByteString, [ByteString]) -> IO ()
registerOneBounded classReg (tyName, ctors) = do
    let boundedCls = BC.pack "Bounded"
        firstCtor  = head ctors
        lastCtor   = last ctors
        minBoundV  = VCon firstCtor []
        maxBoundV  = VCon lastCtor  []
        methods    = Map.fromList
                        [ (BC.pack "minBound", minBoundV)
                        , (BC.pack "maxBound", maxBoundV)
                        ]
    existing <- lookupInstance classReg boundedCls tyName
    case existing of
        Just _  -> pure ()
        Nothing -> do
            registerInstance classReg boundedCls tyName methods
            mapM_ (\c -> registerInstance classReg boundedCls c methods) ctors

-- | Build the @fromEnum@ Val for a derived-Enum sum type.
-- @fromEnum v@ forces @v@ to a 'VCon', locates its constructor name in
-- the ctor list, and returns the 0-based index as a 'VInt'.
synthFromEnumForCtors :: [ByteString] -> Val
synthFromEnumForCtors ctors = VFun $ \t -> do
    v <- force t
    case v of
        VCon n _ -> case indexOf n ctors 0 of
            Just i  -> pure (VInt (fromIntegral i))
            Nothing -> error ("derived fromEnum: unknown ctor "
                              <> BC.unpack n)
        _ -> error ("derived fromEnum: expected constructor, got "
                    <> shortShow v)
  where
    indexOf _ []       _ = Nothing
    indexOf x (c : cs) i
        | x == c    = Just i
        | otherwise = indexOf x cs (i + 1)

-- | Build the @toEnum@ Val for a derived-Enum sum type.
-- @toEnum i@ forces @i@ to a 'VInt' and returns the @i@-th constructor
-- as a nullary 'VCon'. Out-of-range indices raise a runtime error
-- matching GHC's semantics.
synthToEnumForCtors :: [ByteString] -> Val
synthToEnumForCtors ctors = VFun $ \t -> do
    v <- force t
    case v of
        VInt i ->
            let idx = fromIntegral i
            in if idx >= 0 && idx < length ctors
                 then pure (VCon (ctors !! idx) [])
                 else error ("derived toEnum: index "
                             <> show i <> " out of range")
        _ -> error ("derived toEnum: expected Int, got "
                    <> shortShow v)

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

-- | 'lookupInstanceMethod' + 'forceMethodVal' in one step.  Used by
-- 'classMethodDispatcher' so a stored 'VLazyMethod' is forced the
-- moment it's observed by dispatch.  On a parse/eval failure inside
-- the lazy body, we surface 'methodPlaceholder' so the dispatcher's
-- existing "fall through to default" path kicks in.
lookupInstanceMethodForced
    :: ClassRegistry -> ByteString -> ByteString -> ByteString
    -> IO (Maybe Val)
lookupInstanceMethodForced reg cls tag methodName = do
    mv <- lookupInstanceMethod reg cls tag methodName
    traverse forceSafely mv
  where
    forceSafely v = do
        r <- try (forceMethodVal v) :: IO (Either SomeException Val)
        case r of
            Right v' -> pure v'
            Left  _  -> pure methodPlaceholder

-- | 'lookupInSharedReg' + 'forceMethodVal'.  Parallel to
-- 'lookupInstanceMethodForced' for the REPL-level shared registry.
lookupInSharedRegForced
    :: ByteString -> ByteString -> ByteString -> IO (Maybe Val)
lookupInSharedRegForced cls tag methodName = do
    mv <- lookupInSharedReg cls tag methodName
    traverse forceSafely mv
  where
    forceSafely v = do
        r <- try (forceMethodVal v) :: IO (Either SomeException Val)
        case r of
            Right v' -> pure v'
            Left  _  -> pure methodPlaceholder

-- | Build a dispatcher Val for a single class method.
--
-- @classMethodDispatcher reg cls methodName@ returns a VFun that,
-- when applied to its first argument, looks up the instance dict for
-- the argument's type tag and re-applies the named instance method.
-- Remaining arguments (if any) flow through naturally via
-- the returned VFun's own arity.
classMethodDispatcher :: ClassRegistry -> ByteString -> ByteString -> Val
classMethodDispatcher reg cls methodName = selfVal
  where
    -- We knot-tie `selfVal` so the closure can return it as a
    -- "not dispatched" marker when tag-path lookup misses — matchPat
    -- in Eval treats a returned VClassMethod as "no match".
    selfVal = VClassMethod methodName 0 [] $ \tags argT -> case tags of
        -- Type-tag-driven path: matchPat synthesised a tag from a PCon
        -- pattern.  Look up the instance method for that type and
        -- return it WITHOUT applying — the argT we were given is a
        -- matchPat sentinel (VUnit), not a real class-method arg.
        -- Nullary methods (mempty, maxBound) return a concrete value
        -- that matchPat can re-match directly.  Unary+ methods return
        -- a VFun; matchPat treats that as "no match" and the dispatch
        -- fails cleanly.
        (firstTag:_) | isDispatchableTag firstTag -> do
            mM <- lookupInstanceMethodForced reg cls firstTag methodName
            mShared <- lookupInSharedRegForced cls firstTag methodName
            case preferMethod mM mShared of
                Just methodVal
                  | not (isMethodPlaceholder methodVal) -> pure methodVal
                _ -> pure selfVal   -- no instance; tell caller "no match"
        _ -> argDirectedDispatch argT
    argDirectedDispatch argT0 = do
        let go = dispatch 4 []
        case go of
            VFun f -> f argT0
            _      -> pure go
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
                    mSpecial <- specialClassApplication tag av argT accArgs
                    case mSpecial of
                        Just specialVal -> pure specialVal
                        Nothing
                          | isIxIndexMethod
                          , isPairVal av ->
                              -- For Ix methods with a separate index argument,
                              -- the first argument is bounds.  Its runtime
                              -- constructor is always `(,)`, regardless of the
                              -- actual index type, so a failed bounds-specific
                              -- classification must not fall through to the
                              -- generic `(,)` instance.  Carry the bounds arg
                              -- forward and let the real index argument drive
                              -- dispatch.
                              pure (dispatch (remaining - 1) (argT : accArgs))
                        Nothing -> do
                          -- First lookup against the dispatcher's own classReg.
                          mMethod0 <- lookupInstanceMethodForced reg cls tag methodName
                          -- Also check the shared classReg (per Haskell 2010
                          -- §4.3.2, instances from the user's transitive import
                          -- closure should be visible — they may have been
                          -- registered by a later import via the shared ref).
                          mMethodShared <- lookupInSharedRegForced cls tag methodName
                          let mMethod = preferMethod mMethod0 mMethodShared
                          case mMethod of
                            Just methodVal
                              | not (isMethodPlaceholder methodVal) ->
                                    applyAll methodVal (reverse (argT : accArgs))
                            _ -> do
                              mHost <- hostShowFallback reg cls tag methodName av
                              case mHost of
                                Just hostVal -> pure hostVal
                                Nothing -> do
                                  -- Lazy-scan in-scope modules once and retry.
                                  didScan <- lazyInstanceRetry cls tag
                                  mMethod2 <- if didScan
                                      then do
                                          a <- lookupInstanceMethodForced reg cls tag methodName
                                          b <- lookupInSharedRegForced cls tag methodName
                                          pure (preferMethod a b)
                                      else pure mMethod
                                  case mMethod2 of
                                      Just methodVal
                                        | not (isMethodPlaceholder methodVal) ->
                                              applyAll methodVal (reverse (argT : accArgs))
                                      _ -> do
                                          mResult <- resultPolymorphicMethod
                                          case mResult of
                                              Just resultVal ->
                                                  applyAll resultVal (reverse (argT : accArgs))
                                              Nothing -> do
                                                  -- Dispatchable arg but no matching instance.
                                                  -- Fall back to the class's default body.
                                                  mDef0 <- lookupInstanceMethodForced reg cls defaultTypeTag methodName
                                                  mDefShared <- lookupInSharedRegForced cls defaultTypeTag methodName
                                                  let mDef = preferMethod mDef0 mDefShared
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
        mDef <- lookupInstanceMethodForced reg cls defaultTypeTag methodName
        case mDef of
            Just defVal | not (isMethodPlaceholder defVal) ->
                applyAll defVal (reverse (finalArgT : accArgs))
            _ -> do
                mResult <- resultPolymorphicMethod
                case mResult of
                    Just resultVal ->
                        applyAll resultVal (reverse (finalArgT : accArgs))
                    Nothing -> error
                        ( "class-method dispatch: no dispatchable instance of `"
                         <> BC.unpack cls
                         <> "` for method `" <> BC.unpack methodName
                         <> "` (after trying " <> show (length accArgs + 1) <> " arguments)" )

    resultPolymorphicMethod = tryTags (resultPolymorphicDefaultTags cls methodName)
      where
        tryTags [] = pure Nothing
        tryTags (tag:rest) = do
            m0 <- lookupInstanceMethodForced reg cls tag methodName
            mShared <- lookupInSharedRegForced cls tag methodName
            case preferMethod m0 mShared of
                Just methodVal
                  | not (isMethodPlaceholder methodVal) -> pure (Just methodVal)
                _ -> tryTags rest

    resultPolymorphicDefaultTags clsName method
        | clsName == BC.pack "GetAddrInfo"
        , method == BC.pack "getAddrInfo" = [BC.pack "[]"]
        | clsName == BC.pack "MArray"
        , method `elem` map BC.pack ["newArray", "newArray_", "newListArray", "newGenArray"] =
            [BC.pack "STArray"]
        -- MonadParsec methods are parameterized by the parser monad
        -- @m@, which only appears in the result type (e.g. @takeWhileP
        -- :: Maybe String -> (Token s -> Bool) -> m (Tokens s)@).
        -- Argument-directed dispatch picks the first arg's tag (often
        -- @Just@/@Nothing@ from a 'Maybe' label), and the per-tag
        -- lookup misses.  Fall back to the @ParsecT@ instance — the
        -- only MonadParsec instance ihp-hsx and most users actually
        -- exercise — so the parser body resolves without proper type
        -- elaboration.
        | clsName == BC.pack "MonadParsec"
        , method `elem` map BC.pack
            [ "parseError", "label", "hidden", "try", "lookAhead"
            , "notFollowedBy", "withRecovery", "observing", "eof"
            , "token", "tokens", "takeWhileP", "takeWhile1P", "takeP"
            , "getParserState", "updateParserState", "mkParsec"
            ]
        = [BC.pack "ParsecT"]
        -- Applicative / Functor / Monad methods on parser monads have
        -- the same shape as MonadParsec methods: 'm' only appears in
        -- the result type, so argument-directed dispatch can't find
        -- the ParsecT instance from the args alone.  Class defaults
        -- like 'a1 *> a2 = (id <$ a1) <*> a2' may also fail to
        -- evaluate (parser hasn't yet learned every operator they
        -- use), leaving placeholder slots in the registry.  When the
        -- result-polymorphic fallback gets a chance to fire, route
        -- to ParsecT — that instance defines '*>' / '<*' / '<*>' /
        -- 'fmap' / '>>=' explicitly.
        | clsName == BC.pack "Applicative"
        , method `elem` map BC.pack ["*>", "<*", "<*>", "liftA2", "pure"]
        = [BC.pack "ParsecT"]
        | clsName == BC.pack "Functor"
        , method `elem` map BC.pack ["fmap", "<$"]
        = [BC.pack "ParsecT"]
        | clsName == BC.pack "Monad"
        , method `elem` map BC.pack [">>=", ">>", "return"]
        = [BC.pack "ParsecT"]
        | otherwise = []

    specialClassApplication tag av argT accArgs
        | cls == BC.pack "IsString"
        , methodName == BC.pack "fromString"
        , tag == BC.pack "[]"
        , null accArgs
        = Just <$> hostPreferenceFromString av argT
        | cls == BC.pack "Ix"
        , methodName `elem` map BC.pack ["range", "index", "unsafeIndex", "inRange", "rangeSize", "unsafeRangeSize"]
        = do
            mIxTag <- ixBoundsTag av
            case mIxTag of
                Nothing -> pure Nothing
                Just ixTag -> do
                    mHost <- ixHostMethod ixTag av
                    case mHost of
                        Just hostVal -> pure (Just hostVal)
                        Nothing -> do
                            mMethod0 <- lookupInstanceMethodForced reg cls ixTag methodName
                            mShared <- lookupInSharedRegForced cls ixTag methodName
                            case preferMethod mMethod0 mShared of
                                Just methodVal
                                  | not (isMethodPlaceholder methodVal) ->
                                        Just <$> applyAll methodVal (reverse (argT : accArgs))
                                _ -> pure Nothing
        | otherwise = pure Nothing

    ixBoundsTag (VCon "(,)" [loT, hiT]) = do
        lo <- force loT
        hi <- force hiT
        let loTag = typeTagOf lo
            hiTag = typeTagOf hi
        pure $
            if loTag == hiTag && isDispatchableTag loTag
                then Just loTag
                else Nothing
    ixBoundsTag _ = pure Nothing

    isIxIndexMethod =
        cls == BC.pack "Ix"
        && methodName `elem` map BC.pack ["index", "unsafeIndex", "inRange"]

    isPairVal (VCon "(,)" _) = True
    isPairVal _              = False

    ixHostMethod ixTag boundsVal
        | ixTag == BC.pack "Int" = ixIntMethod boundsVal
        | otherwise = pure Nothing

    ixIntMethod boundsVal = do
        mBounds <- ixIntBounds boundsVal
        case mBounds of
            Nothing -> pure Nothing
            Just (lo, hi)
                | methodName == BC.pack "rangeSize" ->
                    pure (Just (VInt (max 0 (hi - lo + 1))))
                | methodName == BC.pack "unsafeRangeSize" ->
                    pure (Just (VInt (hi - lo + 1)))
                | methodName == BC.pack "index" || methodName == BC.pack "unsafeIndex" ->
                    pure (Just (VFun $ \iT -> do
                        i <- force iT
                        case i of
                            VInt n -> pure (VInt (n - lo))
                            _      -> error "Ix Int.index: non-Int index"))
                | methodName == BC.pack "inRange" ->
                    pure (Just (VFun $ \iT -> do
                        i <- force iT
                        case i of
                            VInt n -> pure (if n >= lo && n <= hi
                                            then VCon (BC.pack "True") []
                                            else VCon (BC.pack "False") [])
                            _      -> error "Ix Int.inRange: non-Int index"))
                | otherwise -> pure Nothing

    ixIntBounds (VCon "(,)" [loT, hiT]) = do
        lo <- force loT
        hi <- force hiT
        case (lo, hi) of
            (VInt l, VInt h) -> pure (Just (l, h))
            _                -> pure Nothing
    ixIntBounds _ = pure Nothing

    hostPreferenceFromString av argT = do
        ms <- charListString av
        case ms of
            Just "*"  -> pure (VCon (BC.pack "HostAny") [])
            Just "*4" -> pure (VCon (BC.pack "HostIPv4") [])
            Just "!4" -> pure (VCon (BC.pack "HostIPv4Only") [])
            Just "*6" -> pure (VCon (BC.pack "HostIPv6") [])
            Just "!6" -> pure (VCon (BC.pack "HostIPv6Only") [])
            _         -> pure (VCon (BC.pack "Host") [argT])

    charListString (VCon "[]" _) = pure (Just "")
    charListString (VCon ":" [hT, tT]) = do
        hv <- force hT
        tv <- force tT
        case hv of
            VChar c -> fmap (c :) <$> charListString tv
            _       -> pure Nothing
    charListString _ = pure Nothing

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

-- | Host-backed fallback for @Show.show@ on primitive types.
--
-- Source-loaded @instance Show Int@ overrides @showsPrec@ with
-- @showSignedInt@, whose body uses primop patterns @(I# n)@ that the
-- parser doesn't yet handle.  The instance method ends up registered
-- as 'methodPlaceholder', so the dispatcher falls through to the
-- class default body @show x = showsPrec 0 x ""@, which itself
-- dispatches @showsPrec.Int@ → also placeholder → its default @showsPrec
-- _ x s = show x ++ s@ → calls @show x@ → infinite loop.
--
-- For primitive types where 'IHC.Builtins.showValWith' already has a
-- correct implementation, short-circuit to that instead of letting the
-- placeholder/default chain run.  Only fires for @Show.show@ — other
-- methods (showsPrec, showList) keep their normal dispatch so user
-- overrides still work.
hostShowFallback
    :: ClassRegistry
    -> ByteString    -- ^ class
    -> ByteString    -- ^ type tag
    -> ByteString    -- ^ method name
    -> Val           -- ^ already-forced argument value
    -> IO (Maybe Val)
hostShowFallback reg cls tag methodName av
    | cls == BC.pack "Show"
    , methodName == BC.pack "show"
    , isHostShowable tag = do
        s <- showValWith reg av
        Just <$> stringToListValIO s
    | otherwise = pure Nothing
  where
    isHostShowable t =
        t == BC.pack "Int" || t == BC.pack "Integer" ||
        t == BC.pack "Float" || t == BC.pack "Double" ||
        t == BC.pack "Char" || t == BC.pack "Bool" ||
        t == BC.pack "Word" ||
        t == BC.pack "Int8" || t == BC.pack "Int16" ||
        t == BC.pack "Int32" || t == BC.pack "Int64" ||
        t == BC.pack "Word8" || t == BC.pack "Word16" ||
        t == BC.pack "Word32" || t == BC.pack "Word64"

-- | Look up a class method in the shared (REPL-level) class registry
-- set up by 'setSharedClassReg'. Returns 'Nothing' if no shared reg is
-- installed or if no method is registered under @(cls, tag)@.
lookupInSharedReg :: ByteString -> ByteString -> ByteString -> IO (Maybe Val)
lookupInSharedReg cls tag methodName = do
    mReg <- readIORef sharedClassRegRef
    case mReg of
        Just sharedReg -> lookupInstanceMethod sharedReg cls tag methodName
        Nothing        -> pure Nothing

-- | Given two method lookups (the dispatcher-local and the shared), pick
-- the first non-placeholder Just. Neither winning means the retry path
-- has to fire (or we fall through to default/error).
preferMethod :: Maybe Val -> Maybe Val -> Maybe Val
preferMethod a b =
    case a of
        Just v | not (isMethodPlaceholder v) -> Just v
        _ -> case b of
            Just v | not (isMethodPlaceholder v) -> Just v
            _ -> a

-- | Lazy-scan any in-scope modules that haven't yet been scanned for
-- instances. Called once per dispatch miss; the 'scannedModulesRef' set
-- carried in 'scanHookRef''s closure guards against re-scanning. Returns
-- 'True' if the scan hook was invoked at least once this call (i.e. the
-- caller should retry the lookup).
lazyInstanceRetry :: ByteString -> ByteString -> IO Bool
lazyInstanceRetry _cls _tag = do
    mHook <- readIORef scanHookRef
    case mHook of
        Nothing   -> pure False
        Just hook -> do
            scope <- currentInstanceScope
            -- Each call to hook is idempotent: the hook maintains its
            -- own 'scanned' set and returns immediately for already-
            -- scanned modules.  The retry is bounded by the size of the
            -- current instance scope.
            let modList = Set.toList scope
            anyScanned <- foldM (\acc m -> do
                                    r <- try (hook m) :: IO (Either SomeException ())
                                    case r of
                                        Right () -> pure (acc || True)
                                        Left  _  -> pure acc)
                                False modList
            pure anyScanned

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
    -- Publish every declared method name so the elaborator's
    -- 'classMethodHint' can distinguish real class methods from
    -- top-level bindings that merely happen to have a single-pred
    -- constrained signature (e.g. @array :: Ix i => (i, i) -> ...@).
    let allMethodNames = Set.fromList
            [ m | ClassDecl _ ms _ <- decls, m <- ms ]
    modifyIORef' globalClassMethodNamesRef (Set.union allMethodNames)
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

    -- When the class default for 'methodName' couldn't be captured or
    -- evaluated (e.g. because the body uses operators that scanClassDecls
    -- doesn't recognise, like 'a1 *> a2 = (id <$ a1) <*> a2'), register
    -- 'methodPlaceholder' instead of a function that errors.  The
    -- dispatcher's 'fallback' then sees this via 'isMethodPlaceholder'
    -- and routes through 'resultPolymorphicMethod' before erroring,
    -- giving e.g. 'MonadParsec.*>' a chance to find the ParsecT instance
    -- (which DOES define '*>' explicitly) instead of failing on the
    -- missing class default.
    placeholder _cls _methodName = methodPlaceholder

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
exportBodies
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> Set ByteString
    -> LoadedModule
    -> IO [(ByteString, Expr)]
exportBodies registry searchPath includeMap builtinNames lm = do
    bs <- readIORef (lmBodies lm)
    let keyPrefix | lmIsEntry lm = BC.empty
                  | otherwise    = lmName lm <> BC.pack "."
    rewrites <- buildImportRewrites False registry searchPath includeMap lm builtinNames
    let qualifiedRewrites = Map.filterWithKey
            (\k _ -> BC.any (== '.') k)
            rewrites
    let transform e
            | lmIsEntry lm = rewriteExpr qualifiedRewrites e
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
        [ (keyPrefix <> n, maybe (transform e) EVar (specialSelfAliasTarget lm n e))
        | (n, e) <- Map.toList bs
        , not (isSelfAliasIn lm n e) || isJust (specialSelfAliasTarget lm n e)
        ]

-- | Names of builtins that are FFI/primop-backed and should ALWAYS resolve
-- to the host builtin, never to source definitions. These are excluded from
-- import rewrites so that bare references hit the builtin in the flat env.
-- Only includes names that wrap C FFI calls or primops with no interpretable
-- Haskell source path.
ffiBuiltinNames :: Set ByteString
ffiBuiltinNames = Set.fromList
    [ "hPutBuf", "hGetBuf", "hPutBufNonBlocking", "hGetBufNonBlocking"
    , "with"
    , "withCString", "withCStringLen", "withCStringLen0"
    , "peekCString", "peekCAString", "newCString", "newCAString"
    , "withForeignPtr", "unsafeWithForeignPtr", "newForeignPtr", "addForeignPtrFinalizer"
    , "mallocPlainForeignPtrBytes", "mallocForeignPtrBytes"
    , "newIORef", "readIORef", "writeIORef", "modifyIORef", "modifyIORef'"
    , "atomicModifyIORef'"
    , "mkWeak#", "mkWeakNoFinalizer#"
    , "newAlignedPinnedByteArray#", "byteArrayContents#"
    , "sizeOf", "alignment"
    , "peek", "poke", "peekByteOff", "pokeByteOff", "peekElemOff", "pokeElemOff"
    , "memcpy", "copyBytes"
    , "socket"
    , "setSocketOption"
    , "listen"
    , "accept"
    , "getSocketName"
    , "bind"
    , "mallocBytes"
    , "free"
    , "newUnique", "hashUnique", "fromThreadId"
    , "settingsHost", "settingsPort"
    , "plusForeignPtr", "minusForeignPtr", "plusPtr", "minusPtr", "castPtr"
    , "mkWeakIORef"  -- wraps mkWeak#, which has no Haskell implementation
    , "stdout", "stdin", "stderr"  -- RTS pre-built handles
    ]

-- | Build a map from each locally-visible imported name to its
-- fully-qualified target key (as stored in the flat env).
buildImportRewrites
    :: Bool
    -> ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> Set ByteString
    -> IO (Map ByteString ByteString)
buildImportRewrites allowLoadImports registry searchPath includeMap lm builtinNames = do
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
            -- Union two sources:
            --   (a) bodies already demand-discovered — we know these
            --       are real local definitions (not foreign-alias
            --       sentinels, which are (n, EVar n)).
            --   (b) ALL top-level names scanned from the source —
            --       needed so instance method bodies that reference
            --       siblings not yet demand-loaded (e.g. @concatMap@
            --       referenced by @Monad []@'s @>>=@ body in
            --       GHC.Internal.Base) can still self-rewrite to the
            --       FQN.  The demand-driven env fallback then resolves
            --       the FQN lazily at force time via 'resolveFallback'.
            let discoveredPairs =
                    [ (n, prefix <> n)
                    | (n, expr) <- Map.toList bodiesNow
                    , expr /= EVar n
                    ]
                discoveredNames = Set.fromList (map fst discoveredPairs)
            allScanned <- scanAllTopLevelNames (lmSource lm)
                            `catch` (\(_ :: SomeException) -> pure [])
            let scannedPairs =
                    [ (n, prefix <> n)
                    | n <- allScanned
                    , not (Set.member n discoveredNames)
                    ]
            pure (discoveredPairs ++ scannedPairs)
    importPairs <- concat <$> mapM (rewritesForImport neededNames) imports
    -- Exclude FFI/primop builtins from import rewrites so bare references
    -- resolve to the host builtin rather than chasing source sentinel chains.
    let filteredImportPairs = filter
            (\(n, _) -> not (Set.member n ffiBuiltinNames)
                    && not (Set.member n builtinNames))
            importPairs
    -- Self-rewrites take lower priority than import-rewrites (an import
    -- that brings in the same name shadows the local self-rewrite).
    -- Data.Map.fromList keeps the LAST occurrence for duplicate keys, so
    -- selfPairs must come first and importPairs second for imports to win.
    pure (Map.fromList (selfPairs ++ filteredImportPairs))
  where
    rewritesForImport needed imp
        = do
            let unloadedQualRef = case impAlias imp of
                    Just a  -> Just (a <> BC.pack ".")
                    Nothing
                        | impQualified imp -> Just (impModule imp <> BC.pack ".")
                        | otherwise        -> Nothing
            mTm0 <- lookupOrLoadImport imp
            case mTm0 of
                Nothing -> pure (lazyRewritePairs needed imp unloadedQualRef)
                Just tm -> do
                    let qualRef = case impAlias imp of
                            Just a  -> Just (a <> BC.pack ".")
                            Nothing
                                | impQualified imp -> Just (lmName tm <> BC.pack ".")
                                | otherwise        -> Nothing
                        requestedNames = requestedNamesForImport needed imp qualRef
                    if null requestedNames
                        then pure []
                        else do
                            mapM_ (\n ->
                                (discoverInModule registry searchPath includeMap tm n)
                                    `catch` (\(_ :: SomeException) -> pure ()))
                                requestedNames
                            regAfterDiscover <- readIORef registry
                            -- Gather only the names this module body actually mentions.
                            directPairs <- directRewritePairs regAfterDiscover tm requestedNames
                            reexportPairs <- concat <$>
                                mapM (\m -> rewritePairsFromReexport regAfterDiscover m requestedNames)
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
                                concrete = bare ++ qual
                                concreteKeys = Set.fromList (map fst concrete)
                                lazyMissing =
                                    [ p
                                    | p@(localName, _) <- lazyRewritePairs needed imp qualRef
                                    , not (Set.member localName concreteKeys)
                                    , lazyTargetVisible tm p
                                    ]
                            pure (concrete ++ lazyMissing)

    lazyRewritePairs needed imp qualRef =
        if shouldLazyRewriteImport imp
            then
                [ (localName, impModule imp <> BC.pack "." <> bare)
                | bare <- requestedNamesForImport needed imp qualRef
                , localName <- localNamesForLazyPair needed imp qualRef bare
                ]
            else []

    shouldLazyRewriteImport imp =
        impModule imp /= BC.pack "Prelude" &&
        not (ambiguousQualifiedImport imp) &&
        (impQualified imp ||
        case impSpec imp of
            ImportOnly _ -> True
            _            -> False)

    ambiguousQualifiedImport imp
        | not (impQualified imp) = False
        | otherwise =
            let q = importQualKey imp
            in length [ () | other <- mhImports (lmHeader lm)
                           , impQualified other
                           , importQualKey other == q ] > 1

    importQualKey imp =
        case impAlias imp of
            Just a  -> a
            Nothing -> impModule imp

    localNamesForLazyPair needed imp qualRef bare =
        bareNames ++ qualNames
      where
        bareNames
            | impQualified imp = []
            | Set.member bare needed = [bare]
            | otherwise = []
        qualNames =
            case qualRef of
                Just p
                    | Set.member (p <> bare) needed -> [p <> bare]
                _ -> []

    lazyTargetVisible tm (_, targetKey) =
        case BC.stripPrefix (lmName tm <> BC.pack ".") targetKey of
            Just bare -> exportsNameDirect tm bare
            Nothing   -> True

    lookupOrLoadImport imp = do
        regNow <- readIORef registry
        case Map.lookup (impModule imp) regNow of
            Just (Loaded tm) -> pure (Just tm)
            _ | (allowLoadImports && shouldLoadRewriteImport imp)
             || ambiguousQualifiedImport imp ->
                (Just <$> loadModule registry searchPath includeMap (impModule imp))
                    `catch` (\(_ :: SomeException) -> pure Nothing)
              | otherwise -> pure Nothing

    shouldLoadRewriteImport imp =
        impModule imp == BC.pack "Prelude" ||
        impQualified imp ||
        case impSpec imp of
            ImportOnly _ -> True
            _            -> False

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
            , not (BC.elem '.' key)
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
                , not (isSelfAliasIn tm n expr) || isJust (specialSelfAliasTarget tm n expr)
                , exportsNameDirect tm n
                ]
            fieldExported =
                [ n
                | n <- requestedNames
                , Map.member n (lmFieldReg tm)
                , not (lmNoFieldSelectors tm)
                , exportsNameDirect tm n
                ]
            -- Data constructors declared in tm.  These live in 'buildConEnv'
            -- under their bare name (no module prefix), so the rewrite
            -- target stays as the bare name — the import-rewrite for
            -- e.g. @qualified Text.Megaparsec as Megaparsec@ then maps
            -- @Megaparsec.SourcePos@ to bare @SourcePos@ which the env
            -- already resolves.
            ctorExported =
                [ n
                | n <- requestedNames
                , Map.member n (lmDataReg tm)
                , exportsNameDirect tm n
                ]
            localPairs = [(n, prefix <> n) | n <- nubBS (localExported ++ fieldExported)]
                      ++ [(n, n)            | n <- ctorExported, n `notElem` localExported, n `notElem` fieldExported]
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
                            Just expr -> isSelfAliasIn tm n expr && not (isJust (specialSelfAliasTarget tm n expr))
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
                    case Map.lookup n srcBodies of
                      Just expr | not (isSelfAliasIn srcLm n expr) -> do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            pure [(n, srcPrefix <> n)]
                      _ | Map.member n (lmFieldReg srcLm)
                        , not (lmNoFieldSelectors srcLm)
                        , exportsNameDirect srcLm n -> do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            pure [(n, srcPrefix <> n)]
                      _ -> do
                            -- Try one level deeper (srcLm might also re-export).
                            deeper <- findNameInImports reg srcLm (impModule imp : visited) n
                            case deeper of
                                [] -> go rest
                                ps -> pure ps
                _ -> go rest

    isSelfAliasIn tm n (EVar v) =
        v == n || v == lmName tm <> BC.pack "." <> n
    isSelfAliasIn _ _ _ = False

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
        -- QuasiQuoter: rewrite the QQ function name like any free EVar;
        -- the body bytes are opaque.
        EQuasiQuote n b
            | n `elem` bound            -> EQuasiQuote n b
            | Just q <- Map.lookup n rw -> EQuasiQuote q b
            | otherwise                 -> EQuasiQuote n b
        e@(ELabel _)    -> e   -- Phase 3.5: labels are self-contained
        ETyApp inner ty -> ETyApp (go bound inner) ty   -- value-level @T: recurse into inner expr
        e@ETypedMethod{} -> e   -- elaborator product; no name references to rewrite
        EGuardFail      -> EGuardFail

    goAlt bound (Alt p e) = Alt p (go (patBound p ++ bound) e)

    goStmts _     []                 = []
    goStmts bound (SExpr e   : rest) = SExpr (go bound e)
                                       : goStmts bound rest
    goStmts bound (SBind n e : rest) = SBind n (go bound e)
                                       : goStmts (n : bound) rest
    goStmts bound (SBangBind n e : rest) = SBangBind n (go bound e)
                                       : goStmts (n : bound) rest
    goStmts bound (SLet bs   : rest) =
        let names = map fst bs
            bound' = names ++ bound
            bs'   = [(n, go bound' b) | (n, b) <- bs]
        in SLet bs' : goStmts bound' rest
    goStmts bound (SImplicitLet bs : rest) =
        SImplicitLet [(n, go bound b) | (n, b) <- bs]
          : goStmts bound rest

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
                    lazyPairs0 <- lazyAliasesForImport imp
                    let fieldAliases = Set.fromList
                            [ a
                            | n <- Map.keys (lmFieldReg tm)
                            , not (lmNoFieldSelectors tm)
                            , specAllows (impSpec imp) n
                            , a <- importedAliasesForName imp n
                            ]
                        lazyPairs = filter
                            (\(a, _) -> not (Set.member a fieldAliases))
                            lazyPairs0
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
                    pure (lazyPairs ++ bareAliases ++ qualAliases)
                _ -> lazyAliasesForImport imp

    lazyAliasesForImport imp =
        case impSpec imp of
            ImportOnly names -> concat <$> mapM (lazyAliasesForName imp) names
            _                -> pure []

    lazyAliasesForName imp n = do
        slot <- newIORef (Unevaluated (Closure Map.empty emptyIPMap target))
        pure [ (alias, slot) | alias <- importedAliasesForName imp n ]
      where
        target = EVar (impModule imp <> BC.pack "." <> n)
    
    importedAliasesForName imp n =
        bareAliases ++ qualAliases
      where
        qualPrefix = case impAlias imp of
            Just a  -> Just (a <> BC.pack ".")
            Nothing
                | impQualified imp -> Just (impModule imp <> BC.pack ".")
                | otherwise        -> Nothing
        bareAliases
            | impQualified imp = []
            | otherwise        = [n]
        qualAliases = maybe [] (\p -> [p <> n]) qualPrefix

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
                , not (isSelfAliasIn tm n expr) || isJust (specialSelfAliasTarget tm n expr)
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
                            Just expr -> isSelfAliasIn tm n expr && not (isJust (specialSelfAliasTarget tm n expr))
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
                    case Map.lookup n srcBodies of
                        Just expr | not (isSelfAliasIn srcLm n expr) -> do
                            let srcPrefix = lmName srcLm <> BC.pack "."
                            case Map.lookup (srcPrefix <> n) thunkByKey of
                                Just t  -> pure [(n, t)]
                                Nothing -> go rest
                        _ -> do
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
    registerGlobalLoadedModule lm
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
    let allFields    = unionFieldRegistries (map lmFieldReg lms)
        publicFields = unionFieldRegistries
                         [ lmFieldReg lm | lm <- lms, not (lmNoFieldSelectors lm) ]
    in (publicFields, allFields)

-- | Build the combined field-accessor env: every field is bound under
-- its 'fieldProjName' prefix (for record-dot), and fields from modules
-- that do NOT have 'NoFieldSelectors' are also bound under their bare
-- name (so legacy @fname record@ application works).
buildFieldAccessorEnv :: [LoadedModule] -> FieldRegistry -> FieldRegistry -> IO Env
buildFieldAccessorEnv lms publicFields allFields = do
    -- Bare-name accessors only for non-NoFieldSelectors modules.
    bareEnv <- buildFieldEnv publicFields
    -- Internal record-dot accessors for every field.
    projEnv <- buildFieldEnv allFields
    -- Fully-qualified accessors for import rewrites and FQN fallback.
    qualEnv <- buildQualifiedFieldEnv lms
    let projKeyed = Map.mapKeys fieldProjName projEnv
    pure (Map.unions [bareEnv, projKeyed, qualEnv])
  where
    buildQualifiedFieldEnv :: [LoadedModule] -> IO Env
    buildQualifiedFieldEnv mods = do
        pieces <- mapM perModule mods
        pure (Map.unions pieces)

    perModule :: LoadedModule -> IO Env
    perModule lm
        | lmNoFieldSelectors lm = pure Map.empty
        | Map.null (lmFieldReg lm) = pure Map.empty
        | otherwise = do
            env <- buildFieldEnv (lmFieldReg lm)
            let prefix = lmName lm <> BC.pack "."
            pure (Map.mapKeys (prefix <>) env)

-- | Fields visible while desugaring a module body. Record constructors and
-- patterns can mention records imported from other modules, but the owning
-- module's field registry is empty in that case. Load unqualified imports and
-- append their field clauses after the local ones so local records win on
-- duplicate bare constructor names.
visibleFieldRegistry
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> IO FieldRegistry
visibleFieldRegistry registry searchPath includeMap lm = do
    imported <- mapM importFields (mhImports (lmHeader lm))
    reg <- readIORef registry
    let loadedFields =
            [ lmFieldReg loadedLm
            | (_, Loaded loadedLm) <- Map.toList reg
            ]
    pure (unionFieldRegistries (lmFieldReg lm : imported ++ loadedFields))
  where
    importFields imp
        | impQualified imp = pure Map.empty
        | otherwise = do
            r <- try (loadModule registry searchPath includeMap (impModule imp))
                    :: IO (Either SomeException LoadedModule)
            pure $ case r of
                Right importedLm -> lmFieldReg importedLm
                Left _           -> Map.empty

-- | Cache of parsed-header-only results, keyed by module name.
-- Separate from the full-load 'ModuleRegistry' so that walking the
-- transitive import graph (to compute instance-scope per Haskell
-- 2010 §4.3.2) doesn't require paying the full-load cost.
--
-- 'Nothing' means we attempted to parse the header and failed
-- (file missing, unreadable, or parseModuleHeader error) — the
-- negative result is cached too so repeated walks don't retry.
type HeaderCache = IORef (Map ModuleName (Maybe ModuleHeader))

newHeaderCache :: IO HeaderCache
newHeaderCache = newIORef Map.empty

-- | Cheap header-only load: parse just the module header, cache the
-- result.  Does NOT populate data/foreign/fixity scans, does NOT
-- install body thunks, does NOT touch the ModuleRegistry.  Use this
-- to walk the import graph before deciding what bodies to force.
--
-- Builtin-backed modules (GHC.Prim, etc.) return an empty header —
-- they have no source to parse.
loadModuleHeader
    :: HeaderCache
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> ModuleName
    -> IO (Maybe ModuleHeader)
loadModuleHeader cache searchPath includeMap name = do
    m <- readIORef cache
    case Map.lookup name m of
        Just r  -> pure r
        Nothing -> do
            r <- if isBuiltinBackedModule name
                    then pure (Just emptyHeader)
                    else do
                        mp <- try (locateModule searchPath name)
                                :: IO (Either SomeException FilePath)
                        case mp of
                            Left  _    -> pure Nothing
                            Right path -> do
                                src0 <- readSourceFile path
                                let fileDir = takeDirectory path
                                    incDirs = lookupIncludeDirs includeMap fileDir
                                src  <- cppSourceWithIncludes incDirs src0
                                (mH, _) <- parseModuleHeader src startCursor
                                pure mH
            modifyIORef' cache (Map.insert name r)
            pure r

-- | Walk the transitive import closure of a module, using only
-- header parses.  Returns the set of reachable module names.
-- Cycle-safe (visited set).
--
-- Per Haskell 2010 §4.3.2, an instance declaration is in scope iff
-- the module containing it is in the transitive import closure of
-- the current module.  This function computes that closure cheaply
-- so downstream dispatch can scan only the modules the user's
-- imports actually bring into scope.
transitiveImportClosure
    :: HeaderCache
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> ModuleName
    -> IO (Set ModuleName)
transitiveImportClosure cache searchPath includeMap root =
    go Set.empty [root]
  where
    go seen [] = pure seen
    go seen (m : rest)
      | Set.member m seen = go seen rest
      | otherwise = do
          let seen' = Set.insert m seen
          mh <- loadModuleHeader cache searchPath includeMap m
          let deps = case mh of
                  Just h  -> map impModule (mhImports h)
                  Nothing -> []
          go seen' (rest ++ deps)

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
            if isBuiltinBackedModule name
                then do
                    globalMods <- readIORef globalLoadedModulesRef
                    case Map.lookup name globalMods of
                        Just lm -> do
                            modifyIORef' registry (Map.insert name (Loaded lm))
                            pure lm
                        Nothing -> do
                            lm <- buildEmptyStubModule name
                            modifyIORef' registry (Map.insert name (Loaded lm))
                            registerGlobalLoadedModule lm
                            pure lm
                else do
                    path <- locateModule searchPath name
                    globalMods <- readIORef globalLoadedModulesRef
                    case Map.lookup name globalMods of
                        Just lm
                          | not (lmIsEntry lm)
                          , srcName (lmSource lm) == path -> do
                              modifyIORef' registry (Map.insert name (Loaded lm))
                              -- A previous run's discovery populated
                              -- 'lmBodies' with sentinel 'EVar
                              -- "Target.name"' entries for re-exports,
                              -- and also called 'loadModule' on
                              -- @Target@ as a side effect via
                              -- 'resolveImport'.  Serving the cached
                              -- module skips that side effect, so the
                              -- referenced modules would be missing
                              -- from this run's registry and their
                              -- bindings wouldn't end up in the final
                              -- env — yielding eval-time unbound
                              -- variables like
                              -- @GHC.Internal.Data.OldList.sort@.  Walk
                              -- the cached bodies, extract every module
                              -- prefix, and 'loadModule' each to
                              -- rebuild the transitive closure in the
                              -- per-run registry.  'loadModule' is
                              -- idempotent (it short-circuits on
                              -- per-run hits) and recursively triggers
                              -- hydration for any cached module it
                              -- pulls in.
                              hydrateTransitiveImports registry searchPath includeMap lm
                              pure lm
                        _ -> do
                            src0 <- readSourceFile path
                            let fileDir    = takeDirectory path
                                incDirs    = lookupIncludeDirs includeMap fileDir
                            src  <- cppSourceWithIncludes incDirs src0
                            (mHeader, _) <- parseModuleHeader src startCursor
                            let header = fromMaybe emptyHeader mHeader
                                declared = fromMaybe name (mhName header)
                            modifyIORef' registry (Map.insert name Loading)
                            lm <- buildLoadedModule declared False header src
                            modifyIORef' registry (Map.insert name (Loaded lm))
                            registerGlobalLoadedModule lm
                            pure lm

-- | Global catalogue of every 'LoadedModule' we've ever built.  Used by
-- 'IHC.Eval.eval''s demand-driven env fallback: when a closure's frozen
-- env misses a fully-qualified name, the fallback hook consults this
-- catalogue to find the owning module's body and materialise a 'Thunk'
-- on-demand.  Transient per-import 'ModuleRegistry' values that
-- 'loadImportOnlyIntoEnv' allocates are now ALSO mirrored here so the
-- REPL can see modules loaded by earlier imports.
{-# NOINLINE globalLoadedModulesRef #-}
globalLoadedModulesRef :: IORef (Map ModuleName LoadedModule)
globalLoadedModulesRef = unsafePerformIO (newIORef Map.empty)

registerGlobalLoadedModule :: LoadedModule -> IO ()
registerGlobalLoadedModule lm = do
    modifyIORef' globalLoadedModulesRef (Map.insert (lmName lm) lm)
    -- Mirror per-module type sigs + synonyms into the flat global
    -- registries used by 'IHC.Elaborate'.  Last-writer-wins for name
    -- collisions across modules (rare in practice — sigs are
    -- module-scoped in source).
    modifyIORef' globalTypeSigsRef (Map.union (lmTypeSigs lm))
    modifyIORef' globalTypeSynonymsRef (Map.union (lmTypeSynonyms lm))

-- | Ensure every module referenced by a cached 'LoadedModule''s bodies
-- is present in the per-run registry.  See the comment at the cache-hit
-- branch of 'loadModule' for motivation: serving a parsed 'LoadedModule'
-- from the cross-run cache skips the 'resolveImport' side-effect that
-- originally populated the per-run registry with all transitively
-- referenced modules.
--
-- Walks every body in 'lmBodies', collects all qualified names (those
-- that split into @(Module, bareName)@ via 'splitQualified'), and calls
-- 'loadModule' for each unique module prefix.  'loadModule' is
-- idempotent against the per-run registry and recursively calls this
-- function for any cached module it serves, so the fixed point is
-- reached without an explicit loop here.
--
-- Failures to locate referenced modules are swallowed: the original
-- discovery pass tolerated missing optional modules the same way
-- (see the @try@ at the force-load of core modules in
-- 'loadProgramFromSource'), and a missing transitive dep only matters
-- if this run's code actually references that binding — in which case
-- the evaluator will still report a clean unbound-variable error
-- rather than this helper short-circuiting.
hydrateTransitiveImports
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> IO ()
hydrateTransitiveImports registry searchPath includeMap lm = do
    bodies <- readIORef (lmBodies lm)
    let refs = Set.fromList
            [ modName
            | expr <- Map.elems bodies
            , fv <- freeVars expr
            , Just (modName, _) <- [splitQualified fv]
            ]
    forM_ (Set.toList refs) $ \m -> do
        _ <- try (loadModule registry searchPath includeMap m)
                :: IO (Either SomeException LoadedModule)
        pure ()

-- (moved to IHC.TypeGlobals so both scheduler + evaluator can reach
-- them without a cycle.  Imported via 'globalTypeSigsRef' and
-- 'globalTypeSynonymsRef' at module head.)

-- | Global search path + include-map captured at setup time so the env
-- fallback hook can trigger 'discoverInModule' for FQNs whose bodies
-- haven't been demand-loaded yet.  Populated by 'buildBaseEnv' and
-- 'loadProgramFromSource'.
{-# NOINLINE globalSearchPathRef #-}
globalSearchPathRef :: IORef [FilePath]
globalSearchPathRef = unsafePerformIO (newIORef [])

{-# NOINLINE globalIncludeMapRef #-}
globalIncludeMapRef :: IORef (Map FilePath [FilePath])
globalIncludeMapRef = unsafePerformIO (newIORef Map.empty)

setGlobalSearchPath :: [FilePath] -> Map FilePath [FilePath] -> IO ()
setGlobalSearchPath sp im = do
    writeIORef globalSearchPathRef sp
    writeIORef globalIncludeMapRef im

-- | Memoised slots produced by the env fallback hook.  Keeps one
-- 'Thunk' per FQN so successive demand-lookups share evaluation +
-- memoisation, matching the normal import-driven env layer.
{-# NOINLINE envFallbackCache #-}
envFallbackCache :: IORef (Map ByteString Thunk)
envFallbackCache = unsafePerformIO (newIORef Map.empty)

-- | Install the demand-driven env-fallback hook that 'IHC.Eval.eval'
-- consults when an 'EVar' lookup misses.  Given a fully-qualified name
-- like @Data.Text.Internal.empty@, the hook:
--
--   * splits it into @(Data.Text.Internal, empty)@,
--   * looks up the owning module in 'globalLoadedModulesRef' (every
--     'loadModule' call registers here),
--   * reads the module's 'lmBodies' to find the body expression,
--   * wraps it in a 'Closure' using 'envBaseForFallbackRef''s base env
--     (builtins + class dispatchers + FFI sentinels — NOT the caller's
--     snapshot, so the fallback is self-consistent regardless of which
--     import triggered the miss),
--   * memoises the resulting 'Thunk' in 'envFallbackCache' so repeated
--     lookups share evaluation.
--
-- This replaces the "augment env at registration time" approach with a
-- truly lazy resolution: no FV pre-discovery walks, no eager
-- materialisation of bodies the user's expression never touches.
{-# NOINLINE envBaseForFallbackRef #-}
envBaseForFallbackRef :: IORef Env
envBaseForFallbackRef = unsafePerformIO (newIORef Map.empty)

installEnvFallbackHook :: IO ()
installEnvFallbackHook =
    setEnvFallback $ \mOwner name -> do
        cache <- readIORef envFallbackCache
        case Map.lookup name cache of
            Just t  -> pure (Just t)
            Nothing -> resolveFallback mOwner name

resolveFallback :: Maybe ByteString -> ByteString -> IO (Maybe Thunk)
resolveFallback _mOwner name
    -- warp's modules use a fixed set of qualified aliases.  When env
    -- binding for an alias-qualified name fails, the demand-driven env
    -- fallback lands here.  Each rewrite redirects an alias to its
    -- canonical module-qualified form.  Two shapes show up: literal
    -- @<Alias>.<name>@ (no module prefix) and module-qualified
    -- @<owner>.<Alias>.<name>@; per-symbol rewrites use suffix
    -- matching, prefix rewrites cover the literal form.
    --
    -- @import qualified Network.Wai.Handler.Warp.FdCache as F@
    | BC.pack ".F.setFileCloseOnExec" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FdCache.setFileCloseOnExec")
    | BC.pack ".F.withFdCache" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FdCache.withFdCache")
    | BC.pack ".F.Fd" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FdCache.Fd")
    | BC.pack ".F.Refresh" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FdCache.Refresh")
    -- @import qualified Network.Wai.Handler.Warp.Date as D@
    | BC.pack ".D.withDateCache" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.Date.withDateCache")
    | BC.pack ".D.GMTDate" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.Date.GMTDate")
    -- @import qualified Network.Wai.Handler.Warp.FileInfoCache as I@
    -- (Note: @I@ is reused in other warp files for Data.IORef and
    -- Data.IntMap.Strict, so we cannot prefix-rewrite — only the
    -- specific FileInfoCache symbols below are safe.)
    | BC.pack ".I.withFileInfoCache" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FileInfoCache.withFileInfoCache")
    | BC.pack ".I.FileInfo" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FileInfoCache.FileInfo")
    | BC.pack ".I.fileInfoDate" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FileInfoCache.fileInfoDate")
    | BC.pack ".I.fileInfoSize" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FileInfoCache.fileInfoSize")
    | BC.pack ".I.fileInfoTime" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Wai.Handler.Warp.FileInfoCache.fileInfoTime")
    -- @import qualified Control.Exception as E@.  Pre-existing
    -- @bracket@ / @bracketOnError@ rewrites strip to bare names (those
    -- are re-exported through the Prelude path).  Other E.* names are
    -- not in Prelude, so we redirect to fully-qualified
    -- @Control.Exception.<name>@.
    | BC.pack ".E.bracketOnError" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "bracketOnError")
    | BC.pack ".E.bracket" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "bracket")
    | BC.pack ".E.try" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.try")
    | BC.pack ".E.catch" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.catch")
    | BC.pack ".E.handle" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.handle")
    | BC.pack ".E.handleJust" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.handleJust")
    | BC.pack ".E.finally" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.finally")
    | BC.pack ".E.throwIO" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.throwIO")
    | BC.pack ".E.toException" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.toException")
    | BC.pack ".E.mask_" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.mask_")
    | BC.pack ".E.allowInterrupt" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.allowInterrupt")
    | BC.pack ".E.SomeException" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.SomeException")
    | BC.pack ".E.IOException" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.IOException")
    | BC.pack ".E.ErrorCall" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.ErrorCall")
    | BC.pack ".E.Exception" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Exception.Exception")
    -- @import qualified System.TimeManager as T@.  Note: warp's
    -- Settings.hs uses @T@ for Data.Text instead, so we cannot
    -- prefix-rewrite — per-symbol only.
    | BC.pack ".T.initialize" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.initialize")
    | BC.pack ".T.stopManager" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.stopManager")
    | BC.pack ".T.withHandleKillThread" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.withHandleKillThread")
    | BC.pack ".T.tickle" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.tickle")
    | BC.pack ".T.pause" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.pause")
    | BC.pack ".T.resume" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.resume")
    | BC.pack ".T.Handle" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.Handle")
    | BC.pack ".T.Manager" `isSuffixOf` name =
        resolveFallback _mOwner (BC.pack "System.TimeManager.Manager")
    -- streaming-commons aliases @import qualified Network.Socket as NS@
    -- and uses the alias for accessors / constructors / actions on AddrInfo
    -- (NS.addrFamily, NS.addrSocketType, NS.addrProtocol, …) plus bind /
    -- accept / etc.  warp's hello-world reaches Data.Streaming.Network's
    -- @bindPortGenEx@ which fans out to ~33 NS.* references.  Rather than
    -- enumerate each one, rewrite any @NS.<bareName>@ → the canonical
    -- @Network.Socket.<bareName>@ qualified form, which is registered in
    -- 'IHC.Builtins' (or interpreted from Network.Socket source).
    | BC.pack "NS." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Socket." `BC.append` BC.drop 3 name)
    -- @import qualified Network.Socket.ByteString as Sock@ in warp's
    -- Run.hs / SendFile paths (only Sock.sendAll / Sock.sendMany used).
    | BC.pack "Sock." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Network.Socket.ByteString." `BC.append` BC.drop 5 name)
    -- @import qualified Control.Concurrent as Conc@ — only Conc.yield
    -- and Conc.Sync are referenced, but Conc is unique to warp's Run.hs.
    | BC.pack "Conc." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Control.Concurrent." `BC.append` BC.drop 5 name)
    -- @import qualified Data.Vault.Lazy as Vault@ — used by warp's
    -- HTTP1.hs / HTTP2/Request.hs and Settings.hs for vault keys.
    | BC.pack "Vault." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Data.Vault.Lazy." `BC.append` BC.drop 6 name)
    -- @import qualified Data.ByteString.Builder as BB@ — used in
    -- warp's HTTP1 path for response body construction.
    | BC.pack "BB." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Data.ByteString.Builder." `BC.append` BC.drop 3 name)
    -- @import qualified Data.CaseInsensitive as CI@ — used for
    -- case-insensitive HTTP header keys.
    | BC.pack "CI." `BC.isPrefixOf` name =
        resolveFallback _mOwner (BC.pack "Data.CaseInsensitive." `BC.append` BC.drop 3 name)
resolveFallback mOwner name = do
    mods <- readIORef globalLoadedModulesRef
    case splitQualified name
            <|> splitQualifiedByLoadedModule mods name
            <|> splitQualifiedDottedOperator name of
        Nothing -> resolveBarePrelude mOwner name mods
        Just (modName, bareName) ->
            case Map.lookup modName mods of
                Nothing    -> do
                    -- A qualified rewrite can point directly at a source
                    -- module that has not been loaded yet, e.g.
                    -- System.Posix.Internals re-exporting
                    -- GHC.Internal.System.Posix.Internals.  Try to load that
                    -- owner lazily instead of treating the FQN as missing.
                    searchPath <- readIORef globalSearchPathRef
                    includeMap <- readIORef globalIncludeMapRef
                    transientReg <- newIORef (Map.map Loaded mods)
                    loaded <- try (loadModule transientReg searchPath includeMap modName)
                                :: IO (Either SomeException LoadedModule)
                    case loaded of
                        Left _ -> pure Nothing
                        Right owner -> do
                            if Map.member bareName (lmFieldReg owner)
                               && not (lmNoFieldSelectors owner)
                                then tryFieldSlot (Map.insert modName owner mods) owner bareName
                                else do
                                    _ <- try (discoverInModule transientReg
                                                searchPath includeMap owner bareName)
                                            :: IO (Either SomeException ())
                                    reg <- readIORef transientReg
                                    let newMods = Map.fromList
                                            [ (n, lm) | (n, Loaded lm) <- Map.toList reg ]
                                    modifyIORef' globalLoadedModulesRef
                                        (Map.union newMods)
                                    mods' <- readIORef globalLoadedModulesRef
                                    buildSlotFromOwner mods'
                                        (Map.findWithDefault owner modName mods')
                                        bareName
                Just owner -> do
                    if Map.member bareName (lmFieldReg owner)
                       && not (lmNoFieldSelectors owner)
                        then tryFieldSlot mods owner bareName
                        else do
                            -- If the body isn't yet in 'lmBodies', trigger a
                            -- demand-discovery pass for this single name and
                            -- refresh 'mods'.  This turns the fallback into a
                            -- genuinely lazy resolver: any FQN that CAN be
                            -- discovered in its owning module is discovered
                            -- exactly when the evaluator first references it.
                            bodies0 <- readIORef (lmBodies owner)
                            mods' <- case Map.lookup bareName bodies0 of
                                Just expr | not (isSelfAlias owner bareName expr) -> pure mods
                                _ -> do
                                    searchPath <- readIORef globalSearchPathRef
                                    includeMap <- readIORef globalIncludeMapRef
                                    transientReg <- newIORef (Map.map Loaded mods)
                                    _ <- try (discoverInModule transientReg
                                                searchPath includeMap owner bareName)
                                            :: IO (Either SomeException ())
                                    -- discoverInModule may have loaded new modules
                                    -- into the transient reg; merge them into the
                                    -- global catalogue so subsequent fallbacks see
                                    -- them.
                                    reg <- readIORef transientReg
                                    let newMods = Map.fromList
                                            [ (n, lm) | (n, Loaded lm) <- Map.toList reg ]
                                    modifyIORef' globalLoadedModulesRef
                                        (Map.union newMods)
                                    readIORef globalLoadedModulesRef
                            buildSlotFromOwner mods'
                                (Map.findWithDefault owner modName mods')
                                bareName
  where
    resolveBarePrelude mOwner bareName mods = do
        mBase <- tryBaseBareSlot bareName
        case mBase of
            Just slot -> pure (Just slot)
            Nothing -> do
                mField <- tryGlobalFieldSlot mods bareName
                case mField of
                    Just slot -> pure (Just slot)
                    Nothing -> do
                        mImportedField <- tryImportedFieldSlot mods bareName
                        case mImportedField of
                            Just slot -> pure (Just slot)
                            Nothing -> do
                                -- Constructors first: 'tryAnyModuleBareSlot'
                                -- can spuriously match a constructor name
                                -- via 'findOrResolveLhs' (it sees @TextNode
                                -- !Text@ in a data decl and parses it as a
                                -- binding LHS), then trigger
                                -- 'discoverInModule' which produces a
                                -- bogus slot.  Constructors live in
                                -- 'lmDataReg' which is the authoritative
                                -- source — check there first.
                                mCtor <- tryAnyModuleCtorSlot mods bareName
                                case mCtor of
                                  Just slot -> pure (Just slot)
                                  Nothing -> do
                                   -- When we know the owning module of the
                                   -- closure being evaluated, scope the
                                   -- bare-name lookup to that module's
                                   -- actual import declarations (Haskell
                                   -- 2010 §5.5).  Without an owner —
                                   -- transient lookups, entry-boundary
                                   -- bootstrap, or builtins env-build —
                                   -- fall back to the unscoped global scan.
                                   mAny <- case mOwner of
                                       Just o ->
                                           tryImportScopedBareSlot mods o bareName
                                       Nothing ->
                                           tryAnyModuleBareSlot mods bareName
                                   case mAny of
                                    Just slot -> pure (Just slot)
                                    Nothing -> do
                                        searchPath <- readIORef globalSearchPathRef
                                        includeMap <- readIORef globalIncludeMapRef
                                        transientReg <- newIORef (Map.map Loaded mods)
                                        let ownerName = fromMaybe (BC.pack "Prelude")
                                                (preludeDirectOwner bareName)
                                        loaded <- try (loadModule transientReg searchPath includeMap ownerName)
                                                    :: IO (Either SomeException LoadedModule)
                                        case loaded of
                                            Left _ -> pure Nothing
                                            Right preludeLm -> do
                                                _ <- try (discoverInModule transientReg
                                                            searchPath includeMap preludeLm bareName)
                                                        :: IO (Either SomeException ())
                                                reg <- readIORef transientReg
                                                let newMods = Map.fromList
                                                        [ (n, lm) | (n, Loaded lm) <- Map.toList reg ]
                                                modifyIORef' globalLoadedModulesRef
                                                    (Map.union newMods)
                                                mods' <- readIORef globalLoadedModulesRef
                                                buildSlotFromOwner mods'
                                                    (Map.findWithDefault preludeLm ownerName mods')
                                                    bareName

    -- | Scan every loaded module's 'lmBodies' for @bareName@.  If the
    -- body isn't materialised yet but the source contains a top-level
    -- binding for @bareName@, trigger 'discoverInModule' to force it
    -- into the cache.  This covers the streaming-commons
    -- @bindPortTCP@ / @bindPortGen@ pattern: warp imports
    -- @bindPortTCP@, IHC discovers its body INTO the user's main
    -- module, but the body's free variable @bindPortGen@ — a
    -- same-DSN-module helper — never gets discovered into
    -- DSN.lmBodies.
    --
    -- Walks modules in priority order: Prelude-derived first (GHC.*,
    -- Prelude, Data.List, Data.Maybe, Data.Either), then everything
    -- else.  Otherwise, when warp's path eagerly loads
    -- @Data.ByteString@ before @GHC.List@ (alphabetical Map ordering),
    -- a bare @filter@ binds to @Data.ByteString.filter@ and crashes
    -- with a non-exhaustive @PCon BS@ pattern when the caller passes
    -- a non-ByteString list.  This is the same "Prelude scope wins
    -- for unqualified names" rule that GHC's import resolver bakes
    -- in via 'import Prelude' being implicit.  Probe LHS via
    -- 'findOrResolveLhs' (cheap source scan) and only commit to
    -- discovery on a match.
    tryAnyModuleBareSlot mods bareName = bareSlotIn mods (Map.toList mods) bareName

    -- | Scoped variant: walk only modules that the @owner@ actually
    -- imports unqualified (or has in scope via implicit Prelude), plus
    -- the owner module itself (so locally-defined bindings the initial
    -- discovery walk skipped — e.g. `foo` in `main = print foo` —
    -- still resolve).  Mirrors the resolution order Haskell 2010 §5.5
    -- specifies for unqualified names, instead of treating every
    -- loaded module as if it were imported into the calling scope.
    tryImportScopedBareSlot mods ownerName bareName =
        case Map.lookup ownerName mods of
            Nothing    -> tryAnyModuleBareSlot mods bareName
            Just owner -> do
                let imports = mhImports (lmHeader owner)
                    -- Modules imported unqualified for which @bareName@
                    -- passes the import spec (open, listed, or
                    -- @hiding@-allowed).
                    visibleViaImport =
                        [ impModule imp
                        | imp <- imports
                        , not (impQualified imp)
                        , specAllows (impSpec imp) bareName
                        ]
                    -- Implicit Prelude.  Approximate "what Prelude
                    -- exports" by the set of modules IHC pre-loads as
                    -- core Prelude scope (see 'coreInstanceModules' in
                    -- 'loadProgramFromSource').  Skipped only when the
                    -- owner module sets @NoImplicitPrelude@.
                    implicit
                        | hasNoImplicitPrelude (lmSource owner) = []
                        | otherwise = preludeScope
                    candidateNames = visibleViaImport ++ implicit
                    importCandidates =
                        [ (n, lm)
                        | n  <- candidateNames
                        , Just lm <- [Map.lookup n mods]
                        ]
                    -- Local bindings shadow imports (H2010 §5.5.1):
                    -- search the owner module's own source first.
                    candidates = (ownerName, owner) : importCandidates
                bareSlotIn mods candidates bareName

    preludeScope :: [ModuleName]
    preludeScope =
        [ BC.pack "Prelude"
        , BC.pack "GHC.Internal.Base"
        , BC.pack "GHC.Internal.List"
        , BC.pack "GHC.List"
        , BC.pack "GHC.Internal.Show"
        , BC.pack "GHC.Internal.Enum"
        , BC.pack "GHC.Internal.Ix"
        , BC.pack "GHC.Internal.Num"
        , BC.pack "GHC.Internal.Real"
        , BC.pack "GHC.Internal.Maybe"
        , BC.pack "GHC.Internal.IO"
        , BC.pack "GHC.Maybe"
        ]

    -- Shared body-or-discover walker used by both the scoped and
    -- unscoped bare-name fallbacks.  Walks @candidates@ in order; for
    -- each module, first checks 'lmBodies', then probes the source via
    -- 'findOrResolveLhs' and triggers discovery on a hit.
    bareSlotIn mods candidates bareName = go candidates
      where
        go [] = pure Nothing
        go ((_, owner) : rest) = do
            bodies <- readIORef (lmBodies owner)
            if Map.member bareName bodies
                then buildSlotFromOwner mods owner bareName
                else do
                    mLhs <- findOrResolveLhs (lmSource owner) (lmKnown owner) bareName
                    case mLhs of
                        Just _ -> do
                            searchPath <- readIORef globalSearchPathRef
                            includeMap <- readIORef globalIncludeMapRef
                            transientReg <- newIORef (Map.map Loaded mods)
                            _ <- try (discoverInModule transientReg
                                        searchPath includeMap owner bareName)
                                    :: IO (Either SomeException ())
                            bodies' <- readIORef (lmBodies owner)
                            if Map.member bareName bodies'
                                then buildSlotFromOwner mods owner bareName
                                else go rest
                        Nothing -> go rest

    -- | Scan every loaded module's 'lmDataReg' for a constructor named
    -- @bareName@.  When @import M (T(..))@ brings constructors into
    -- scope, the scheduler unions all 'lmDataReg's into a process-wide
    -- 'conEnv' at fresh-evaluation time — but lazily-loaded modules
    -- whose ctors only appear AFTER 'conEnv' was built (and any
    -- constructors used in eval contexts that didn't refresh 'conEnv')
    -- still need a fallback path.  Build a one-off 'Thunk' that
    -- materialises the same 'VCon' / 'VFun' chain that 'buildConEnv'
    -- would have created for the constructor.
    tryAnyModuleCtorSlot mods bareName = go (Map.toList mods)
      where
        go [] = pure Nothing
        go ((_, owner) : rest) =
            case Map.lookup bareName (lmDataReg owner) of
                Just (_tyName, arity, _idx) -> do
                    slot <- mkCtorSlot bareName arity
                    pure (Just slot)
                Nothing -> go rest

        mkCtorSlot name 0 = newWHNFThunk (VCon name [])
        mkCtorSlot name arity =
            newLazyBuiltinThunk (pure (buildLam name arity []))

        buildLam name 0 acc = VCon name (reverse acc)
        buildLam name left acc = VFun $ \t ->
            pure (buildLam name (left - 1) (t : acc))

    preludeDirectOwner bareName
        | bareName `elem` [ "elem", "filter" ] = Just (BC.pack "GHC.List")
        | bareName == BC.pack "defaultSettings" = Just (BC.pack "Network.Wai.Handler.Warp.Settings")
        | otherwise = Nothing

    registerSharedDerivedEnumBounded loaded = do
        mReg <- readIORef sharedClassRegRef
        case mReg of
            Nothing  -> pure ()
            Just reg -> registerDerivedEnumBoundedInstances reg loaded

    splitQualifiedByLoadedModule mods name =
        case candidates of
            []     -> Nothing
            (x:xs) -> Just (foldl longer x xs)
      where
        candidates =
            [ (modName, BC.drop (BC.length prefix) name)
            | modName <- Map.keys mods
            , let prefix = modName <> BC.pack "."
            , prefix `BC.isPrefixOf` name
            , BC.length name > BC.length prefix
            ]
        longer a@(ma, _) b@(mb, _)
            | BC.length ma >= BC.length mb = a
            | otherwise = b

    splitQualifiedDottedOperator name =
        case candidates of
            []     -> Nothing
            (x:xs) -> Just (foldl longer x xs)
      where
        parts = BC.split '.' name
        candidates =
            [ (BC.intercalate (BC.pack ".") modParts, op)
            | i <- [1 .. length parts - 1]
            , let (modParts, opParts) = splitAt i parts
            , all validModulePart modParts
            , let op = BC.intercalate (BC.pack ".") opParts
            , not (BC.null op)
            , isSymbol (BC.head op)
            ]
        validModulePart p =
            not (BC.null p)
            && let h = BC.head p in h >= 'A' && h <= 'Z'
        isSymbol c =
            not ((c >= 'a' && c <= 'z')
              || (c >= 'A' && c <= 'Z')
              || (c >= '0' && c <= '9')
              || c == '_' || c == '\'')
        longer a@(ma, _) b@(mb, _)
            | BC.length ma >= BC.length mb = a
            | otherwise = b

    buildSlotFromOwner mods owner bareName = do
        registerSharedDerivedEnumBounded (Map.elems mods)
        bodies <- readIORef (lmBodies owner)
        case Map.lookup bareName bodies of
            Just expr | not (isSelfAlias owner bareName expr) -> do
                -- Synthesize a transient registry from the global
                -- catalogue so 'buildImportRewrites' can walk the
                -- owner's imports to resolve bare references inside
                -- the body to FQNs the fallback can then look up
                -- recursively.
                transientReg <- newIORef (Map.map Loaded mods)
                searchPath <- readIORef globalSearchPathRef
                includeMap <- readIORef globalIncludeMapRef
                rw <- buildImportRewrites True transientReg searchPath includeMap owner Set.empty
                        `catch` (\(_ :: SomeException) -> pure Map.empty)
                baseEnv <- readIORef envBaseForFallbackRef
                extraRw <- buildTargetedImportRewrites
                                transientReg searchPath includeMap owner baseEnv rw expr
                let rwAll = Map.union rw extraRw
                    expr' = if Map.null rwAll
                              then expr
                              else rewriteExpr rwAll expr
                -- Augment with constructors + field accessors from
                -- ALL globally loaded modules — body may reference
                -- constructors (like 'Text') defined in its own
                -- module that weren't in the original base env
                -- (which predates the user's imports).
                let unionedData =
                        unionDataRegistries (map lmDataReg (Map.elems mods))
                    (publicFields, unionedFields) =
                        partitionFieldRegistries (Map.elems mods)
                conEnvAll   <- buildConEnv unionedData
                fieldEnvAll <- buildFieldAccessorEnv
                                    (Map.elems mods) publicFields unionedFields
                ffiEnvAll <- buildForeignEnv (Map.elems mods) searchPath
                slot <- newIORef (BlackHole "<fallback-placeholder>")
                let selfKey = lmName owner <> BC.pack "." <> bareName
                ownerLocalEnv <- buildOwnerLocalEnv owner bodies bareName slot
                -- Stamp the closure's env with the owning module via
                -- the @"$$owner"@ sentinel so 'IHC.Eval.currentOwner'
                -- can scope unqualified-name fallback to this module's
                -- import declarations (Haskell 2010 §5.5).  Sub-closures
                -- that extend this env (lambdas, lets) inherit the
                -- sentinel automatically.
                ownerThunk <- newWHNFThunk (VStr (lmName owner))
                let richEnv = Map.insert ownerSentinelKey ownerThunk
                            $ Map.union ownerLocalEnv
                            $ Map.union baseEnv
                                (Map.unions [conEnvAll, fieldEnvAll, ffiEnvAll])
                writeIORef slot
                    (Unevaluated (Closure richEnv emptyIPMap expr'))
                modifyIORef' envFallbackCache
                    (Map.insert name slot . Map.insert selfKey slot)
                pure (Just slot)
            _ -> do
                mImport <- tryImportAliasSlot mods owner bareName
                case mImport of
                    Just slot -> do
                        modifyIORef' envFallbackCache (Map.insert name slot)
                        pure (Just slot)
                    Nothing -> do
                        mClassMethod <- tryClassMethodSlot owner bareName
                        case mClassMethod of
                            Just slot -> do
                                modifyIORef' envFallbackCache (Map.insert name slot)
                                pure (Just slot)
                            Nothing -> do
                                mBase <- tryBaseBareSlot bareName
                                case mBase of
                                    Just slot -> do
                                        modifyIORef' envFallbackCache (Map.insert name slot)
                                        pure (Just slot)
                                    Nothing -> do
                                        mCon <- tryConstructorSlot mods owner bareName
                                        case mCon of
                                            Just slot -> do
                                                modifyIORef' envFallbackCache (Map.insert name slot)
                                                pure (Just slot)
                                            Nothing -> tryFieldSlot mods owner bareName

    isSelfAlias owner bareName (EVar n) =
        n == bareName || n == lmName owner <> BC.pack "." <> bareName
    isSelfAlias _ _ _ = False

    buildOwnerLocalEnv owner bodies bareName selfSlot = do
        scanned <- scanAllTopLevelNames (lmSource owner)
            `catch` (\(_ :: SomeException) -> pure [])
        classDecls <- scanClassDecls (lmSource owner)
            `catch` (\(_ :: SomeException) -> pure [])
        let classMethods =
                [ method
                | ClassDecl _ methods _ <- classDecls
                , method <- methods
                ]
            localNames = nubBS (Map.keys bodies ++ scanned ++ classMethods)
        pairs <- concat <$> mapM mkLocal localNames
        pure (Map.fromList pairs)
      where
        ownerName = lmName owner
        mkLocal localName
            | localName == bareName =
                pure (entries localName selfSlot)
            | otherwise = do
                let fqn = ownerName <> BC.pack "." <> localName
                slot <- newLazyBuiltinThunk $ do
                    mSlot <- resolveFallback (Just ownerName) fqn
                    case mSlot of
                        Just targetSlot -> force targetSlot
                        Nothing -> error
                            ("fallback: unresolved same-module binding "
                             <> BC.unpack fqn)
                pure (entries localName slot)
        entries localName slot =
            [ (localName, slot)
            , (ownerName <> BC.pack "." <> localName, slot)
            ]

    tryConstructorSlot mods owner bareName = do
        let unionedData =
                unionDataRegistries (lmDataReg owner : map lmDataReg (Map.elems mods))
        conEnv <- buildConEnv unionedData
        pure (Map.lookup bareName conEnv)

    buildTargetedImportRewrites transientReg searchPath includeMap owner baseEnv existingRw expr = do
        pairs <- concat <$> mapM resolveOne candidates
        pure (Map.fromList pairs)
      where
        candidates =
            [ fv
            | fv <- nubBS (freeVars expr)
            , not (BC.elem '.' fv)
            , not (Map.member fv existingRw)
            , not (Map.member fv baseEnv)
            ]

        resolveOne fv = do
            mProvider <- resolveImport transientReg searchPath includeMap owner fv
                            `catch` (\(_ :: SomeException) -> pure Nothing)
            case mProvider of
                Just provider ->
                    pure [(fv, provider <> BC.pack "." <> fv)]
                Nothing ->
                    pure []

    tryBaseBareSlot bareName = do
        baseEnv <- readIORef envBaseForFallbackRef
        case Map.lookup bareName baseEnv of
            Nothing   -> pure Nothing
            Just slot -> do
                -- An ImportOnly alias for a data constructor is stored in
                -- the entry env as @bareName -> thunk whose body is
                -- @EVar "Mod.bareName"@. When the fallback for the
                -- qualified FQN walks this path, returning the alias would
                -- create a self-loop: the caller is already forcing that
                -- very slot.  Reject BlackHoled and explicitly self-
                -- referential slots so the fallback continues on to
                -- 'tryConstructorSlot' / 'tryFieldSlot' instead.  See the
                -- GHC.Internal.Arr.Array repro in the unit #5 fix.
                st <- readIORef slot
                case st of
                    BlackHole _ -> pure Nothing
                    Unevaluated (Closure _ _ (EVar target))
                        | target == name -> pure Nothing
                    _ -> pure (Just slot)

    tryGlobalFieldSlot mods bareName = do
        let loaded = Map.elems mods
            (publicFields, unionedFields) = partitionFieldRegistries loaded
        fieldEnvAll <- buildFieldAccessorEnv loaded publicFields unionedFields
        pure (Map.lookup bareName fieldEnvAll)

    tryImportedFieldSlot mods bareName = do
        searchPath <- readIORef globalSearchPathRef
        includeMap <- readIORef globalIncludeMapRef
        transientReg <- newIORef (Map.map Loaded mods)
        let candidateModules =
                nubBS
                    [ impModule imp
                    | owner <- Map.elems mods
                    , imp <- mhImports (lmHeader owner)
                    , not (impQualified imp)
                    , impModule imp /= BC.pack "Prelude"
                    , specAllows (impSpec imp) bareName
                    ]
        go transientReg searchPath includeMap candidateModules
      where
        go _ _ _ [] = pure Nothing
        go transientReg searchPath includeMap (modName:rest) = do
            loaded <- try (loadModule transientReg searchPath includeMap modName)
                        :: IO (Either SomeException LoadedModule)
            case loaded of
                Right lm
                    | Map.member bareName (lmFieldReg lm)
                    , not (lmNoFieldSelectors lm)
                    , exportsName lm bareName -> do
                        reg <- readIORef transientReg
                        let newMods = Map.fromList
                                [ (n, loadedLm)
                                | (n, Loaded loadedLm) <- Map.toList reg
                                ]
                        modifyIORef' globalLoadedModulesRef (Map.union newMods)
                        mods' <- readIORef globalLoadedModulesRef
                        tryGlobalFieldSlot mods' bareName
                _ -> go transientReg searchPath includeMap rest

    tryClassMethodSlot owner bareName = do
        decls <- scanClassDecls (lmSource owner)
        case [ cls | ClassDecl cls methods _ <- decls, bareName `elem` methods ] of
            []      -> pure Nothing
            (cls:_) -> do
                mSharedReg <- readIORef sharedClassRegRef
                case mSharedReg of
                    Nothing -> pure Nothing
                    Just classReg -> do
                        mods <- readIORef globalLoadedModulesRef
                        searchPath <- readIORef globalSearchPathRef
                        includeMap <- readIORef globalIncludeMapRef
                        baseEnv <- readIORef envBaseForFallbackRef
                        transientReg <- newIORef (Map.map Loaded mods)
                        let loaded = Map.elems mods
                            typeCtors = foldr Map.union Map.empty
                                (map lmTypeCtorReg (owner : loaded))
                        classTable <- buildClassMethodTable (owner : loaded)
                        registerInstancesFrom transientReg searchPath includeMap
                            classReg typeCtors classTable baseEnv owner
                        Just <$> newWHNFThunk
                            (classMethodDispatcher classReg cls bareName)

    tryImportAliasSlot mods owner bareName = do
        searchPath <- readIORef globalSearchPathRef
        includeMap <- readIORef globalIncludeMapRef
        transientReg <- newIORef (Map.map Loaded mods)
        mProvider <- resolveImport transientReg searchPath includeMap owner bareName
                        `catch` (\(_ :: SomeException) -> pure Nothing)
        case mProvider of
            Nothing -> pure Nothing
            Just providerMod -> do
                reg <- readIORef transientReg
                let newMods = Map.fromList
                        [ (n, lm) | (n, Loaded lm) <- Map.toList reg ]
                modifyIORef' globalLoadedModulesRef (Map.union newMods)
                let providerName = providerMod <> BC.pack "." <> bareName
                mSlot <- resolveFallback (Just (lmName owner)) providerName
                case mSlot of
                    Just slot -> do
                        modifyIORef' envFallbackCache (Map.insert name slot)
                        pure (Just slot)
                    Nothing -> pure Nothing

    tryFieldSlot mods owner bareName = do
        let loaded = Map.elems mods
            (publicFields, unionedFields) = partitionFieldRegistries loaded
        fieldEnvAll <- buildFieldAccessorEnv loaded publicFields unionedFields
        let fqn = lmName owner <> BC.pack "." <> bareName
        case Map.lookup fqn fieldEnvAll <|> Map.lookup bareName fieldEnvAll of
            Just slot -> do
                modifyIORef' envFallbackCache (Map.insert name slot)
                pure (Just slot)
            Nothing -> pure Nothing

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
-- | Narrow re-export shims in @base@ that our demand-driven import
-- walker needs to look through.  These bare @GHC.X@ modules are
-- normally blocked during re-export chasing (to avoid pulling in
-- @GHC.Base@'s big transitive subgraph), but they're actually thin
-- wrappers over the corresponding @GHC.Internal.X@ module — no heavier
-- than loading the internal target directly.  Skipping them means e.g.
-- @Data.Array (bounds)@ / @Data.IORef (newIORef)@ fail as "unbound"
-- when accessed lazily.
isAllowedTargetedGhc :: ModuleName -> Bool
isAllowedTargetedGhc n =
       n == BC.pack "GHC.Arr"
    || n == BC.pack "GHC.IORef"
    || n == BC.pack "GHC.STRef"
    || n == BC.pack "GHC.ST"
    || n == BC.pack "GHC.List"
    || n == BC.pack "GHC.MVar"
    || n == BC.pack "GHC.Exception"
    || n == BC.pack "GHC.Ix"

isBuiltinBackedModule :: ModuleName -> Bool
isBuiltinBackedModule n =
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
    -- builtins for splice execution.  Most submodules are stubbed because
    -- their content is replaced by IHC.TH's synthetic primops.
    -- HOWEVER: 'Language.Haskell.TH.Quote' is a small pure module that
    -- declares 'data QuasiQuoter = QuasiQuoter { quoteExp, … }'.  The
    -- QQ-dispatch path needs that constructor (record-construction in
    -- ihp-hsx etc.) and it has no primops backing it — interpret it
    -- from source.
    || ("Language.Haskell.TH" `BC.isPrefixOf` n && n /= BC.pack "Language.Haskell.TH.Quote")

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
        , lmForeignDecls     = []
        , lmTypeSigs         = Map.empty
        , lmTypeSynonyms     = Map.empty
        }

buildLoadedModule :: ModuleName -> Bool -> ModuleHeader -> Source -> IO LoadedModule
buildLoadedModule name isEntry header src = do
    known               <- emptyKnownSymbols
    (dataR, fldR, tCtR) <- scanDataDecls src
    tfReg               <- scanTypeFamilyDecls src
    foreigns            <- scanForeignImports src
    sigs                <- scanTypeSigs src
    synonyms            <- scanTypeSynonyms src
    bodies              <- newIORef Map.empty
    -- Pre-populate 'lmBodies' with a sentinel @EVar ffiSynthKey@ for
    -- every scanned 'foreign import ccall' decl.  discoverInModule will
    -- short-circuit on these (the name is already in bodies) and the
    -- synth key resolves to a host VFun thunk installed into the base
    -- env before knot-tying (see 'buildForeignEnv').  The prefix
    -- @__ffi.@ keeps the key out of the way of any plausible Haskell
    -- identifier / qualified-module name.
    forM_ foreigns $ \decl ->
        modifyIORef' bodies
            (Map.insert (FFI.fdName decl)
                        (EVar (ffiSynthKey name (FFI.fdName decl))))
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
        , lmForeignDecls     = foreigns
        , lmTypeSigs         = Map.fromList sigs
        , lmTypeSynonyms     = Map.fromList synonyms
        }

-- | Synthetic env key under which a foreign import's dispatch 'Val' is
-- registered.  Derived from @(moduleName, haskellName)@ so collisions
-- between different modules that import the same C symbol under
-- different Haskell names are impossible.
ffiSynthKey :: ModuleName -> ByteString -> ByteString
ffiSynthKey modName nm = BC.pack "__ffi." <> modName <> BC.pack "." <> nm

-- | Build an 'Env' containing one thunk per foreign import across every
-- loaded module.  Each thunk, when forced, produces a curried 'VFun'
-- that dispatches the real C symbol via libffi on application (see
-- 'FFI.makeForeignVal').
--
-- The thunks are lazy — most programs never reference most of the
-- foreign imports their transitively-loaded modules declare, so we
-- don't pay the cost unless the user actually calls one.
--
-- Before building the env, the function also walks every loaded
-- module's enclosing package and calls 'FFI.registerLibrary' for each
-- @extra-libraries:@ entry declared in its @.cabal@ file.  This is how
-- Hackage libraries like @postgresql-libpq@ (which needs @libpq@) become
-- reachable — the bare @dlsym(RTLD_DEFAULT, ...)@ path only sees
-- @libSystem@ unless we @dlopen@ the extras first.  Failures to
-- @dlopen@ are silently swallowed; the error only surfaces later when
-- the user actually calls a symbol that would have resolved from the
-- missing library.
buildForeignEnv :: [LoadedModule] -> [FilePath] -> IO Env
buildForeignEnv lms searchPath = do
    registerPackageExtras lms searchPath
    pairs <- concat <$> mapM perModule lms
    pure (Map.fromList pairs)
  where
    perModule lm = mapM (mkPair lm) (lmForeignDecls lm)
    mkPair lm decl = do
        t <- newLazyBuiltinThunk (FFI.makeForeignVal decl)
        pure (ffiSynthKey (lmName lm) (FFI.fdName decl), t)

-- | @dlopen@ every @extra-libraries:@ entry declared by every package
-- known to the source-root walker.  Reads the session-memoised
-- 'cachedPackageTable' (a single pass over the three source roots —
-- nix bundle, user cache, cabal tarball) and takes the union of every
-- package's @pkgExtraLibs@.  The per-module ad-hoc walk that used to
-- live here was superseded by 'cachedPackageTable' (added upstream in
-- commit 8048e3c) — using it removes a redundant cabal-file parse per
-- module and guarantees we see every declared extra, even for
-- packages no module in the current run has imported (harmless, and
-- the @dlopen@ cost for extras a program doesn't use is negligible).
registerPackageExtras :: [LoadedModule] -> [FilePath] -> IO ()
registerPackageExtras lms _searchPath
    -- Fast path: if no loaded module declares any 'foreign import' the
    -- whole purpose of this function (making host-side symbols
    -- discoverable for the FFI dispatcher) is moot.  On a cold cache,
    -- enumerating every package's @extra-libraries:@ stanza takes ~190 ms
    -- (cabal-file parsing dominates) — pure overhead for FFI-free
    -- programs.  Even with the on-disk fingerprint cache and the
    -- in-process CAF memo restored in 7eb5037, this remains the single
    -- biggest non-FFI startup cost: 'cachedPackageTable' on a warm cache
    -- is still tens of ms, and we hit it once per 'loadProgramFromSource'.
    --
    -- 'lmForeignDecls' is populated by 'scanForeignImports', which has a
    -- byte-level early-out for sources that don't contain the literal
    -- string @\"foreign\"@, so 'all (null . lmForeignDecls)' is itself
    -- O(modules) cheap byte-scans for FFI-free runs.
    --
    -- bang_pattern_acc fixture (4 lines, 14 loaded modules, 0 foreign
    -- decls): trims another ~150 ms off total runtime.
    | all (null . lmForeignDecls) lms = pure ()
    | otherwise = do
        table <- cachedPackageTable
        let libs = Set.toList (Set.fromList
                       [ lib
                       | (_, info) <- table
                       , lib       <- pkgExtraLibs info
                       ])
        mapM_ FFI.registerLibrary libs

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
        && not (isSmallGhcWrapper name)
        then pure False
        else do
            -- Locate the module to check if it's a local file.
            mPath <- (Just <$> locateModule searchPath name)
                        `catch` (\(_ :: ModuleNotFound) -> pure Nothing)
            case mPath of
                Nothing -> pure False
                Just _  -> pure True   -- found anywhere is OK (we blocked bare GHC.* above)
  where
    -- Small public wrappers with real source that primarily re-export their
    -- GHC.Internal implementation. These are safe to load demand-driven for
    -- explicit named re-export chains such as Data.Array -> GHC.Arr.
    isSmallGhcWrapper m =
        m `elem` map BC.pack
            [ "GHC.Arr"
            ]

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
    | Just (qual, bareName) <- splitQualified name = do
        case qualifiedBuiltinAlias lm qual bareName builtins of
            Just rhs ->
                modifyIORef' (lmBodies lm) (Map.insert name rhs)
            Nothing -> do
                mTarget <- resolveQualifiedName registry searchPath includeMap lm qual bareName
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
                            -- If bareName is in target's bodies, use target's FQN
                            -- (the actual source-loaded binding). Else prefer an
                            -- FQN-keyed builtin over a bare-name fallback so
                            -- `BS.pack` routes to `Data.ByteString.pack` rather
                            -- than Prelude's polymorphic `pack`.
                            rhs
                              | Map.member bareName targetBodies = EVar fqn
                              | Set.member fqn builtins          = EVar fqn
                              | otherwise                        = EVar bareName
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
                                        Just srcMod ->
                                            -- Point at the actual providing
                                            -- module's FQN, not a bare-name
                                            -- self-reference (which would
                                            -- self-loop at eval time).
                                            modifyIORef' (lmBodies lm)
                                                (Map.insert name
                                                    (EVar (srcMod <> BC.pack "." <> name)))
                                        Nothing ->
                                            pure ()
                            Just expr0 -> do
                                visibleFields <-
                                    if needsRecordFields expr0
                                        then visibleFieldRegistry registry searchPath includeMap lm
                                        else pure (lmFieldReg lm)
                                let expr = desugarRecordPats visibleFields
                                             (desugarRecordCons visibleFields expr0)
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
                                mapM_ discoverFreeVar
                                    (nubBS (discoveryFreeVars expr
                                            ++ extraDiscoveryFreeVars lm name))
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
                                Just srcMod ->
                                    -- Memoize: point at the actual providing
                                    -- module's FQN so eval-time lookup goes
                                    -- through thunkByKey to the real binding
                                    -- (foreign-import sentinel or source body).
                                    modifyIORef' (lmBodies lm)
                                        (Map.insert name
                                            (EVar (srcMod <> BC.pack "." <> name)))
                                Nothing ->
                                    -- Assume a builtin; let the evaluator
                                    -- complain if truly missing.
                                    pure ()

qualifiedBuiltinAlias
    :: LoadedModule
    -> ByteString
    -> ByteString
    -> Set ByteString
    -> Maybe Expr
qualifiedBuiltinAlias lm qual bareName builtins =
    case [ impModule imp
         | imp <- mhImports (lmHeader lm)
         , importMatchesQual qual imp
         , builtinExceptionModule (impModule imp)
         , builtinExceptionName bareName
         ] of
        targetMod : _ ->
            let fqn = targetMod <> BC.pack "." <> bareName
            in Just (EVar (if Set.member fqn builtins then fqn else bareName))
        [] -> Nothing
  where
    builtinExceptionModule m =
        m `elem` map BC.pack
            [ "Control.Exception"
            , "GHC.Internal.Control.Exception"
            , "GHC.IO"
            , "GHC.Internal.IO"
            ]

    builtinExceptionName n =
        n `elem` map BC.pack
            [ "mask", "mask_", "uninterruptibleMask", "uninterruptibleMask_"
            , "block", "unblock", "unsafeUnmask"
            , "allowInterrupt", "interruptible"
            , "bracket", "bracket_", "bracketOnError", "finally", "onException"
            , "catch", "handle", "try", "evaluate"
            ]

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

resolveQualifiedName
    :: ModuleRegistry
    -> [FilePath]
    -> Map FilePath [FilePath]
    -> LoadedModule
    -> ByteString
    -> ByteString
    -> IO (Maybe LoadedModule)
resolveQualifiedName registry searchPath includeMap lm qual bareName = do
    let imports = filter (importMatchesQual qual) (mhImports (lmHeader lm))
    go imports
  where
    go [] = pure Nothing
    go (imp:rest) = do
        loaded <- try (loadModule registry searchPath includeMap (impModule imp))
                    :: IO (Either SomeException LoadedModule)
        case loaded of
            Left _ -> go rest
            Right targetLm -> do
                provides <- qualifiedImportProvides targetLm bareName
                if provides
                    then pure (Just targetLm)
                    else go rest

    qualifiedImportProvides targetLm bareName = do
        bodies <- readIORef (lmBodies targetLm)
        local <- case Map.lookup bareName bodies of
            Just expr -> pure (not (isSelfAliasIn targetLm bareName expr))
            Nothing
                | Map.member bareName (lmFieldReg targetLm)
                , not (lmNoFieldSelectors targetLm) -> pure True
                | otherwise ->
                    isJust <$> findOrResolveLhs (lmSource targetLm) (lmKnown targetLm) bareName
        pure $ case mhExports (lmHeader targetLm) of
            ExportAll -> local
            -- An explicit export list may name a binding re-exported from
            -- the module's own imports (e.g. GHC.Arr.newSTArray from
            -- GHC.Internal.Arr). Accept it here; discoverInModule on the
            -- target will chase the named re-export and memoize the alias.
            ExportList _ -> local || exportsName targetLm bareName

importMatchesQual :: ByteString -> ImportDecl -> Bool
importMatchesQual qual imp =
    case impAlias imp of
        Just a  -> a == qual
        Nothing -> impModule imp == qual

isSelfAliasIn :: LoadedModule -> ByteString -> Expr -> Bool
isSelfAliasIn tm n (EVar v) =
    v == n || v == lmName tm <> BC.pack "." <> n
isSelfAliasIn _ _ _ = False

specialSelfAliasTarget :: LoadedModule -> ByteString -> Expr -> Maybe ByteString
specialSelfAliasTarget lm n expr
    | isSelfAliasIn lm n expr
    , lmName lm == BC.pack "Network.Socket.SockAddr"
    , n == BC.pack "bind"
    = Just (BC.pack "Network.Socket.Syscall.bind")
    | isSelfAliasIn lm n expr
    , lmName lm == BC.pack "Network.Socket.Info"
    , n == BC.pack "getAddrInfo"
    = Just (BC.pack "getAddrInfo")
    | isSelfAliasIn lm n expr
    , lmName lm == BC.pack "Network.Socket.BufferPool"
    , n `elem` map BC.pack ["newBufferPool", "withBufferPool", "mallocBS", "copy"]
    = Just (BC.pack "Network.Socket.BufferPool.Buffer." <> n)
    | isSelfAliasIn lm n expr
    , lmName lm == BC.pack "Network.Socket.BufferPool"
    , n `elem` map BC.pack ["receive", "makeRecvN"]
    = Just (BC.pack "Network.Socket.BufferPool.Recv." <> n)
specialSelfAliasTarget _ _ _ = Nothing

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
    -> IO (Maybe ModuleName)
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
                            tgtBodies <- readIORef (lmBodies targetLm)
                            let ffiKey = ffiSynthKey (lmName targetLm) name
                                isFfi  = case Map.lookup name tgtBodies of
                                            Just (EVar k) -> k == ffiKey
                                            _             -> False
                            if isFfi && exportsName targetLm name
                              then do
                                discoverInModule registry searchPath includeMap targetLm name
                                pure (Just (lmName targetLm))
                              else do
                                mLhs <- findOrResolveLhs (lmSource targetLm)
                                                         (lmKnown targetLm) name
                                case mLhs of
                                    Just _ ->
                                        if exportsName targetLm name
                                            then do
                                                discoverInModule registry searchPath includeMap targetLm name
                                                pure (Just (lmName targetLm))
                                            else tryImports rest
                                    Nothing -> do
                                        isClassMethod <- exportsClassMethod targetLm name
                                        if isClassMethod
                                            then pure (Just (lmName targetLm))
                                            -- Record-field accessor (e.g. `runIdentity` of `Identity(..)`):
                                            -- the type's data registry has the field, the export
                                            -- list admits it via `T(..)`. No separate binding; the
                                            -- later-built fieldEnv will synthesize it.
                                            else if Map.member name (lmFieldReg targetLm)
                                                 && exportsName targetLm name
                                                then pure (Just (lmName targetLm))
                                                -- The name isn't defined locally in targetLm.
                                                else continueMissing targetLm rest

    continueMissing targetLm rest
        | exportsMissingName targetLm name = followNamedReexport targetLm rest
        | otherwise = followModuleReexports targetLm rest

    moduleClassMethods targetLm = do
        decls <- scanClassDecls (lmSource targetLm)
        pure [ method | ClassDecl _ methods _ <- decls, method <- methods ]

    exportsClassMethod targetLm methodName = do
        decls <- scanClassDecls (lmSource targetLm)
        let classes =
                [ (className, methods)
                | ClassDecl className methods _ <- decls
                , methodName `elem` methods
                ]
            exportedBy className methods = case mhExports (lmHeader targetLm) of
                ExportAll -> True
                ExportList items -> any (itemExports className methods) items
            itemExports className methods item = case item of
                ExportType n Nothing ->
                    n == className && methodName `elem` methods
                ExportType n (Just []) ->
                    n == className && methodName `elem` methods
                ExportType n (Just subs) ->
                    n == className && methodName `elem` subs
                _ -> False
        pure (any (uncurry exportedBy) classes)

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
                Just m  -> pure (Just m)
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
                        && not (isAllowedTargetedGhc (impModule imp))
        case Map.lookup (impModule imp) reg of
            Just (Loaded srcLm) -> do
                mLhs  <- findOrResolveLhs (lmSource srcLm) (lmKnown srcLm) name
                case mLhs of
                    Just _ ->
                        if exportsName srcLm name
                            then do
                                discoverInModule registry searchPath includeMap srcLm name
                                pure (Just (lmName srcLm))
                            else tryViaImports moreImps depth rest
                    Nothing -> do
                        -- The provider may export a class method through
                        -- Class(..) rather than a top-level binding.
                        exportsMethod <- exportsClassMethod srcLm name
                        if exportsMethod
                            then pure (Just (lmName srcLm))
                            -- srcLm might itself re-export via unqualified imports
                            -- (go one level deeper, decrementing the depth cap).
                            else if exportsMissingName srcLm name
                                then followNamedReexportD (depth - 1) srcLm rest >>= \case
                                        Just m  -> pure (Just m)
                                        Nothing -> tryViaImports moreImps depth rest
                                else tryViaImports moreImps depth rest
            _ ->
                -- Module not yet loaded.
                if isBlockedGhc
                    then tryViaImports moreImps depth rest
                    else do
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
                                            pure (Just (lmName srcLm))
                                        else do
                                            modifyIORef' registry (Map.delete (impModule imp))
                                            tryViaImports moreImps depth rest
                                Nothing -> do
                                    exportsMethod <- exportsClassMethod srcLm name
                                    if exportsMethod
                                        then pure (Just (lmName srcLm))
                                        else if exportsMissingName srcLm name && depth > 1
                                            then followNamedReexportD (depth - 1) srcLm rest >>= \case
                                                    Just m  -> pure (Just m)
                                                    Nothing -> do
                                                        modifyIORef' registry (Map.delete (impModule imp))
                                                        tryViaImports moreImps depth rest
                                            else do
                                                modifyIORef' registry (Map.delete (impModule imp))
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
                                            Right () -> pure (Just (lmName reLm))
                                            Left  _  -> tryReexports visited mods rest
                                    else tryReexports visited mods rest
                            Nothing ->
                                -- Go one level deeper if reLm itself has module re-exports.
                                -- Mark modName as visited to prevent cycles.
                                followModuleReexportsV (modName : visited) reLm [] >>= \case
                                    Just m   -> pure (Just m)
                                    Nothing  -> tryReexports visited mods rest

specAllows :: ImportSpec -> ByteString -> Bool
specAllows ImportAll         _ = True
specAllows (ImportOnly ns)   n =
    -- The literal-name match handles ordinary
    -- @import M (foo, bar)@.  The @$dotdot@ sentinel comes from
    -- @import M (T(..))@: we couldn't enumerate T's constructors at
    -- parse time (M wasn't loaded yet), so we left a wildcard.  Any
    -- constructor name passes once the wildcard is present — the
    -- normal cross-module ctor resolution path then picks it up.
    n `elem` ns
        || BC.pack "$dotdot" `elem` ns
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
    fields = lmFieldReg lm  -- field name → [(ctor, idx)]

    -- Is @n@ a record-field accessor for any constructor of @typeHead@?
    -- Used for the @T(..)@ export form, which implicitly exports every
    -- field selector of T's constructors (e.g. @Identity(..)@ exports
    -- @runIdentity@, not just the @Identity@ constructor).
    isFieldOfType typeHead =
        let ctorsOfT = Map.findWithDefault [] typeHead tCtors
        in case Map.lookup n fields of
            Just ctorIdx -> any (\(c, _) -> c `elem` ctorsOfT) ctorIdx
            Nothing      -> False

    matchExport (ExportName m)            = n == m
    matchExport (ExportType m Nothing)    = n == m
    matchExport (ExportType m (Just [])) =
        -- The `T(..)` form: the type head plus every constructor of T
        -- that the scanner saw in this module's source, plus every
        -- record-field accessor defined by those constructors.
        n == m
        || n `elem` Map.findWithDefault [] m tCtors
        || isFieldOfType m
    matchExport (ExportType m (Just subs)) =
        -- The `T(Ctor1, Ctor2, field1, ...)` form: the type head plus
        -- every explicitly-listed sub-name.
        n == m || n `elem` subs
    -- `module Foo` re-export: the scheduler follows this dynamically;
    -- here we conservatively return True so the name is not filtered
    -- out before the dynamic check in resolveImport.
    matchExport (ExportModule _)  = True

-- | True only when a missing local binding may still be supplied by an
-- explicit named/module re-export.  An @ExportAll@ module exports its own
-- top-level declarations, not arbitrary names from its imports, so a missing
-- local name in that case must not trigger a transitive import walk.
exportsMissingName :: LoadedModule -> ByteString -> Bool
exportsMissingName lm n = case mhExports (lmHeader lm) of
    ExportAll     -> False
    ExportList xs -> any matchExport xs
  where
    tCtors = lmTypeCtorReg lm
    fields = lmFieldReg lm

    isFieldOfType typeHead =
        let ctorsOfT = Map.findWithDefault [] typeHead tCtors
        in case Map.lookup n fields of
            Just ctorIdx -> any (\(c, _) -> c `elem` ctorsOfT) ctorIdx
            Nothing      -> False

    matchExport (ExportName m)            = n == m
    matchExport (ExportType m Nothing)    = n == m
    matchExport (ExportType m (Just [])) =
        n == m
        || n `elem` Map.findWithDefault [] m tCtors
        || isFieldOfType m
    matchExport (ExportType m (Just subs)) = n == m || n `elem` subs
    matchExport (ExportModule _)           = True

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
            -- The tail may be a lowercase identifier (qualified
            -- value, e.g. @M.sort@), an underscore-prefixed value
            -- (e.g. @Data.Word8._lf@), an uppercase identifier
            -- (qualified data constructor, e.g. @M.Nothing@), or a
            -- symbolic operator (e.g. @M.<$>@).  Without the operator
            -- case, rewritten imported operators never reach fallback
            -- resolution and fail as unbound FQNs.
            , let h = BC.head tailPart in isLower h || isUpper h || h == '_' || isSymbol h
            , all (isUpper . BC.head) rest ->
                Just (BC.intercalate (BC.pack ".") (reverse rest), tailPart)
        _ -> Nothing
  where
    isLower c = c >= 'a' && c <= 'z'
    isUpper c = c >= 'A' && c <= 'Z'
    isSymbol c = not (isLower c || isUpper c || (c >= '0' && c <= '9') || c == '_' || c == '\'')

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
        -- QuasiQuoter: the QQ function name is a free var that must be
        -- discovered so the dispatch sees the imported QuasiQuoter value.
        EQuasiQuote n _
            | n `elem` bound -> []
            | otherwise      -> [n]
        ELabel _        -> []   -- Phase 3.5: labels have no free variables
        ETyApp inner _  -> goAll bound inner   -- value-level @T: inner expr contributes free vars
        ETypedMethod{}  -> []   -- elaborator product; no EVar refs
        EGuardFail      -> []

    -- A do-block introduces bindings left-to-right; each SBind/SLet
    -- extends the bound set for subsequent stmts.
    goStmts _     []                  = []
    goStmts bound (SExpr e   : rest)  = goAll bound e ++ goStmts bound rest
    goStmts bound (SBind n e : rest)  = goAll bound e ++ goStmts (n : bound) rest
    goStmts bound (SBangBind n e : rest) = goAll bound e ++ goStmts (n : bound) rest
    goStmts bound (SLet bs   : rest)  =
        let names  = map fst bs
            bound' = names ++ bound
        in concatMap (\(_, rhs) -> goAll bound' rhs) bs
           ++ goStmts bound' rest
    goStmts bound (SImplicitLet bs : rest) =
        concatMap (\(_, rhs) -> goAll bound rhs) bs
           ++ goStmts bound rest

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

needsRecordFields :: Expr -> Bool
needsRecordFields = goExpr
  where
    goExpr = \case
        EVar _       -> False
        ELit _       -> False
        EApp f x     -> goExpr f || goExpr x
        ELam _ e     -> goExpr e
        ELet bs e    -> any (goExpr . snd) bs || goExpr e
        ECase s as   -> goExpr s || any goAlt as
        EIf c t e    -> goExpr c || goExpr t || goExpr e
        EDo stmts    -> any goStmt stmts
        ENeg e       -> goExpr e
        ETuple es    -> any goExpr es
        ERecordCon{} -> True
        ERecordWild{} -> True
        ERecordUpdate{} -> True
        EImplicitRef _ -> False
        EImplicitLet bs e -> any (goExpr . snd) bs || goExpr e
        ESplice e    -> goExpr e
        EQuote _     -> False
        EQuasiQuote{} -> False   -- QQ body is opaque bytes, no record syntax to descend into
        ELabel _     -> False
        ETyApp e _   -> goExpr e
        ETypedMethod{} -> False
        EGuardFail    -> False

    goStmt = \case
        SExpr e         -> goExpr e
        SBind _ e       -> goExpr e
        SBangBind _ e   -> goExpr e
        SLet bs         -> any (goExpr . snd) bs
        SImplicitLet bs -> any (goExpr . snd) bs

    goAlt (Alt p e) = goPat p || goExpr e

    goPat = \case
        PVar _        -> False
        PWild         -> False
        PLit _        -> False
        PCon _ ps     -> any goPat ps
        PAs _ p       -> goPat p
        PBang p       -> goPat p
        PTuple ps     -> any goPat ps
        PRecord{}     -> True
        PRecordWild{} -> True
        PView e p     -> goExpr e || goPat p

-- | Free variables that must be available to start evaluating a binding.
--
-- This is intentionally narrower than 'freeVars'. The scheduler's first
-- discovery pass should not chase work hidden behind lazy boundaries: lambda
-- bodies, let RHSs, case alternatives, if branches, or data-constructor
-- fields. Those bodies are rewritten/exported with the full 'freeVars'
-- machinery later and can be resolved through the demand-driven fallback when
-- they are actually forced.
discoveryFreeVars :: Expr -> [ByteString]
discoveryFreeVars = go []
  where
    go bound = \case
        EVar n
            | n `elem` bound -> []
            | otherwise      -> [n]
        ELit _      -> []
        EApp (EApp (EVar op) action) rest
            | op == BC.pack ">>" ->
                [op] ++ go bound action ++ go bound rest
            | op == BC.pack ">>=" ->
                [op] ++ go bound action ++ goContinuation bound rest
        app@(EApp _ _) ->
            let (headExpr, args) = appSpine app
            in case headExpr of
                EVar op | isDiscoveryStrictControl op ->
                    [op] ++ concatMap (go bound) args
                _ -> go bound headExpr
        ELam _ _    -> []
        ELet bs e   ->
            let names = map fst bs
            in go (names ++ bound) e
        ECase s _   -> go bound s
        EIf c _ _   -> go bound c
        EDo stmts   -> goStmts bound stmts
        ENeg e      -> go bound e
        ETuple es   -> concatMap (go bound) es
        ERecordCon _ _ -> []
        ERecordWild _  -> []
        ERecordUpdate e _ -> go bound e
        EImplicitRef _ -> []
        EImplicitLet bs e ->
            let names = map fst bs
            in go (names ++ bound) e
        ESplice inner  -> go bound inner
        EQuote _       -> []
        -- QuasiQuoter: make the QQ fn name a discovery free var so the
        -- scheduler pre-loads the defining module before eval fires.
        EQuasiQuote n _
            | n `elem` bound -> []
            | otherwise      -> [n]
        ELabel _       -> []
        ETyApp inner _ -> go bound inner
        ETypedMethod{} -> []
        EGuardFail     -> []

    goStmts _     []                  = []
    goStmts bound (SExpr e   : rest)  = go bound e ++ goStmts bound rest
    goStmts bound (SBind n e : rest)  = go bound e ++ goStmts (n : bound) rest
    goStmts bound (SBangBind n e : rest) = go bound e ++ goStmts (n : bound) rest
    goStmts bound (SLet bs   : rest)  =
        let names = map fst bs
        in goStmts (names ++ bound) rest
    goStmts bound (SImplicitLet bs : rest) =
        let names = map fst bs
        in goStmts (names ++ bound) rest

    goContinuation bound = \case
        ELam n body -> go (n : bound) body
        other       -> go bound other

    appSpine expr = goSpine expr []
      where
        goSpine (EApp f x) args = goSpine f (x : args)
        goSpine other args      = (other, args)

    isDiscoveryStrictControl op =
        bareName op `elem`
            [ BC.pack "bracket"
            , BC.pack "bracketOnError"
            , BC.pack "finally"
            , BC.pack "onException"
            ]

    bareName name =
        case BC.elemIndexEnd (toEnum (fromEnum '.')) name of
            Just idx -> BC.drop (idx + 1) name
            Nothing  -> name

-- | Small demand hints for library entry points whose operationally strict
-- calls sit behind top-level lambdas. The main discovery walk intentionally
-- avoids recursively chasing arbitrary lambda bodies; these hints keep known
-- control-flow entry points on the normal tied-env path instead of forcing
-- them through the eval-time fallback.
extraDiscoveryFreeVars :: LoadedModule -> ByteString -> [ByteString]
extraDiscoveryFreeVars lm name
    | lmName lm == BC.pack "Network.Wai.Handler.Warp.Run"
    , name == BC.pack "run" =
        [BC.pack "runSettings"]
    | lmName lm == BC.pack "Network.Wai.Handler.Warp.Run"
    , name == BC.pack "runSettings" =
        [ BC.pack "bindPortTCP"
        , BC.pack "setSocketCloseOnExec"
        , BC.pack "settingsAccept"
        , BC.pack "runSettingsSocket"
        ]
    | otherwise =
        []

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
            errExpr i = EApp (EVar "error")
                             (ELit (LStr (BC.pack
                                ( "record construction: missing field "
                               <> BC.unpack conName <> "#" <> show i))))
            fallbackExpr i
                | i < length fields = go (snd (fields !! i))
                | otherwise         = errExpr i
            args = [ Map.findWithDefault (fallbackExpr i) i byIndex
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
            -- If none of the updated field names are in the FieldRegistry
            -- (neither locally nor in any loaded module), we cannot desugar
            -- this update — emit a runtime 'error' rather than silently
            -- discarding the update (which would make the bug invisible).
            missingFieldsErr =
                let names = BC.intercalate (BC.pack ", ")
                              [ fname | (fname, _) <- updatesG ]
                in EApp (EVar "error")
                        (ELit (LStr (BC.pack "record update: field(s) "
                                  <> names
                                  <> BC.pack " not in registry")))
        in if null alts
               then missingFieldsErr
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

    goStmt (SExpr e)         = SExpr (go e)
    goStmt (SBind n e)       = SBind n (go e)
    goStmt (SBangBind n e)   = SBangBind n (go e)
    goStmt (SLet bs)         = SLet [(n, go b) | (n, b) <- bs]
    goStmt (SImplicitLet bs) = SImplicitLet [(n, go b) | (n, b) <- bs]

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

    goStmt (SExpr e)         = SExpr (goExpr e)
    goStmt (SBind n e)       = SBind n (goExpr e)
    goStmt (SBangBind n e)   = SBangBind n (goExpr e)
    goStmt (SLet bs)         = SLet [(n, goExpr b) | (n, b) <- bs]
    goStmt (SImplicitLet bs) = SImplicitLet [(n, goExpr b) | (n, b) <- bs]

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
