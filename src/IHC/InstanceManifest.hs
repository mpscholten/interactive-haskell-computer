-- | In-memory instance manifest — IHC's analogue of GHC's @InstEnv@,
-- built directly from the source cache at first use.
--
-- The problem this solves: when an IHC fixture runs, the scheduler
-- needs to know which modules contain typeclass instances for the
-- classes the user's program references.  Dispatching @fmap@ on @[]@
-- requires @instance Functor []@, which lives in @GHC.Internal.Base@,
-- but demand-driven FV discovery short-circuits at builtin-shimmed
-- names like @fmap@ and never reaches Prelude's re-export chain.
--
-- A hardcoded seven-module force-load used to paper over this in
-- 'IHC.Scheduler.loadProgramFromSource'.  This module replaces it with
-- a derived index built from real Haskell source: walk
-- @~/.cache/ihc/sources/@, run the existing tokeniser-only scanners
-- ('IHC.Scan.scanClassDecls' / 'IHC.Scan.scanInstanceDecls' /
-- 'IHC.Scan.scanDataDecls'), and produce four lookup maps:
--
--   * 'miClassProviders' — class name → set of modules with instances
--   * 'miClassHeads'     — class name → instance head-type names
--   * 'miMethodOwner'    — method name → owning class
--   * 'miTypeProviders'  — type-ctor name → defining module
--
-- The index is built once per process and held in 'manifestIndexRef'.
-- No on-disk persistence: the scan takes ~1–2 s for the full source
-- cache, and the test suite (one process, 1596 fixtures) amortises
-- that cost into noise.  CLI invocations (a fresh process per file)
-- pay it on every cold start, but that's a 1–2 s regression vs the
-- previous force-load list, not the kind of cost a binary @.hi@
-- artefact would warrant.  If startup latency ever becomes a real
-- issue, persistence can come back as a pure optimisation layer.
module IHC.InstanceManifest
    ( -- * Types
      PackageManifest (..)
    , ModuleManifest  (..)
    , ClassEntry      (..)
    , InstanceEntry   (..)
    , ManifestIndex   (..)
      -- * Generation
    , generatePackageManifest
      -- * Process-level index
    , manifestIndex
    , providersForClass
    , classForMethod
    , providerModulesForMethods
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Directory
    ( XdgDirectory (XdgCache)
    , doesDirectoryExist
    , getXdgDirectory
    , listDirectory
    )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified IHC.Cpp as Cpp
import qualified IHC.ModuleHeader as MH
import IHC.Lexer (startCursor)
import qualified IHC.Scan as Scan
import qualified IHC.Source as Src

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data PackageManifest = PackageManifest
    { pmPackageId :: !ByteString
    , pmModules   :: !(Map ByteString ModuleManifest)
    } deriving (Show)

data ModuleManifest = ModuleManifest
    { mmName      :: !ByteString
    , mmFilePath  :: !FilePath
    , mmImports   :: ![ByteString]
    , mmClasses   :: ![ClassEntry]
    , mmInstances :: ![InstanceEntry]
    , mmTypeCtors :: ![ByteString]   -- ^ type constructors declared in this module
    } deriving (Show)

data ClassEntry = ClassEntry
    { ceClassName    :: !ByteString
    , ceMethodNames  :: ![ByteString]
    , ceSuperclasses :: ![ByteString]
    } deriving (Show)

data InstanceEntry = InstanceEntry
    { ieClassName :: !ByteString
    , ieTypeNames :: ![ByteString]
    } deriving (Show)

-- | The denormalised lookup tables used at runtime.  Built once per
-- process from a scan of the source cache and cached in
-- 'manifestIndexRef'.
data ManifestIndex = ManifestIndex
    { miClassProviders :: !(Map ByteString (Set ByteString))
        -- ^ class name → modules that declare instances for it
    , miClassHeads     :: !(Map ByteString [ByteString])
        -- ^ class name → list of head-type names appearing in instances
        --   (used to follow @instance Monad Maybe@ → load @Maybe@'s
        --   defining module via 'miTypeProviders').
    , miMethodOwner    :: !(Map ByteString ByteString)
        -- ^ method name → owning class
    , miTypeProviders  :: !(Map ByteString ByteString)
        -- ^ type-constructor name → module that declares it.
        --   Loading a module's instance heads (@instance Monad Maybe@
        --   in @GHC.Internal.Base@) also pulls in the modules that
        --   DEFINE the head types (@data Maybe = Nothing | Just@ in
        --   @GHC.Internal.Maybe@), without which dispatch can't
        --   resolve the type tag.
    } deriving (Show)

emptyIndex :: ManifestIndex
emptyIndex = ManifestIndex Map.empty Map.empty Map.empty Map.empty

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Build a 'PackageManifest' for one source-cache directory.  Walks
-- every @.hs@ under @root@, parses just the header + class/instance
-- declarations, and packages the result.  Body parsing and evaluation
-- are deliberately avoided — the manifest is metadata only.
generatePackageManifest :: ByteString -> FilePath -> IO PackageManifest
generatePackageManifest pkgId root = do
    files <- collectHaskellFiles root
    let sortedFiles = List.sort files
    modules <- foldM (\acc fp -> do
                         m <- scanModule fp
                         case m of
                             Nothing -> pure acc
                             Just mm -> pure (Map.insert (mmName mm) mm acc))
                     Map.empty
                     sortedFiles
    pure PackageManifest
        { pmPackageId = pkgId
        , pmModules   = modules
        }

-- | Recursively collect every @.hs@ file under a directory, skipping
-- @dist@ / @dist-newstyle@ / hidden directories.
collectHaskellFiles :: FilePath -> IO [FilePath]
collectHaskellFiles root = do
    exists <- doesDirectoryExist root
    if not exists
        then pure []
        else go root
  where
    go dir = do
        entries <- listDirectory dir
        let visible = filter (not . isHidden) entries
        concat <$> mapM (classify dir) visible

    classify dir name = do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then if isSkippable name then pure [] else go path
            else if isHaskellSource name then pure [path] else pure []

    isHidden ('.' : _) = True
    isHidden _         = False

    isSkippable n = n `elem`
        [ "dist", "dist-newstyle", "_build", ".stack-work", "stack-work" ]

    isHaskellSource n = ".hs" `List.isSuffixOf` n

-- | Read one source file and produce a 'ModuleManifest'.  Errors are
-- swallowed (a malformed file just contributes nothing to the manifest).
scanModule :: FilePath -> IO (Maybe ModuleManifest)
scanModule fp = do
    r <- try (scanModuleUnsafe fp) :: IO (Either SomeException (Maybe ModuleManifest))
    case r of
        Right m  -> pure m
        Left _   -> pure Nothing

scanModuleUnsafe :: FilePath -> IO (Maybe ModuleManifest)
scanModuleUnsafe fp = do
    src0 <- Src.readSourceFile fp
    -- Run CPP so files using @#if MIN_VERSION_base(...)@ etc. parse.
    -- We don't have package include-dirs at manifest-generation time
    -- (those come from cabal metadata that's process-scoped in the
    -- scheduler), but 'defaultCppContext' covers the macros every
    -- ghc-internal file actually consults.
    bs' <- Cpp.cppPreprocessWithIncludes [] Cpp.defaultCppContext
                                         (Src.srcName src0) (Src.srcBytes src0)
    let src = Src.withBytes src0 bs'
    (mHdr, _) <- MH.parseModuleHeader src startCursor
    case mHdr >>= MH.mhName of
        Nothing -> pure Nothing
        Just modName -> do
            let imports = maybe [] (map MH.impModule . MH.mhImports) mHdr
            classes <- map fromClassDecl <$> Scan.scanClassDecls src
            instances <- map fromInstanceDecl <$> Scan.scanInstanceDecls src
            (_, _, typeCtorReg) <- Scan.scanDataDecls src
            let typeCtors = Map.keys typeCtorReg
            pure $ Just ModuleManifest
                { mmName      = modName
                , mmFilePath  = fp
                , mmImports   = imports
                , mmClasses   = classes
                , mmInstances = instances
                , mmTypeCtors = typeCtors
                }
  where
    fromClassDecl c = ClassEntry
        { ceClassName    = Scan.classClassName c
        , ceMethodNames  = Scan.classMethodNames c
        , ceSuperclasses = Scan.classSuperclasses c
        }
    fromInstanceDecl i = InstanceEntry
        { ieClassName = Scan.instClassName i
        , ieTypeNames = Scan.instTypeNames i
        }

--------------------------------------------------------------------------------
-- Process-level index
--------------------------------------------------------------------------------

-- | The manifest index, evaluated at most once per process.
--
-- This is a top-level CAF: GHC's runtime guarantees that a NOINLINE
-- top-level binding under 'unsafePerformIO' is forced exactly once
-- and its result shared by every subsequent reference.  No IORef, no
-- 'Maybe'-wrapping, no @get-or-build@ ceremony — callers just use
-- 'manifestIndex' as a value.
--
-- The first reference triggers the source-cache scan (~1–2 s for
-- ~233 ghc-internal modules); subsequent references are constant-time
-- map lookups against the cached value.  The test suite's 1596
-- fixtures share one scan.
{-# NOINLINE manifestIndex #-}
manifestIndex :: ManifestIndex
manifestIndex = unsafePerformIO buildIndexFromCache

-- | Walk source-cache package directories, scan every @.hs@ file once,
-- and merge into a single index.
buildIndexFromCache :: IO ManifestIndex
buildIndexFromCache = do
    sourceDirs <- sourcesCacheDirs
    packageDirs <- concat <$> mapM packagesUnder sourceDirs
    case packageDirs of
        [] -> pure emptyIndex
        _  -> do
            manifests <- mapM
                (\(pkg, dir) -> generatePackageManifest (BC.pack pkg) dir)
                (List.sort packageDirs)
            pure (buildIndex (concatMap (Map.elems . pmModules) manifests))
  where
    packagesUnder sourcesDir = do
        sourcesExist <- doesDirectoryExist sourcesDir
        if not sourcesExist
            then pure []
            else do
                entries <- listDirectory sourcesDir
                let candidates = filter (not . isPrefixOfDot) entries
                packages <- filterM (\p -> doesDirectoryExist (sourcesDir </> p)) candidates
                pure [ (p, sourcesDir </> p) | p <- packages ]

    isPrefixOfDot ('.' : _) = True
    isPrefixOfDot _         = False

sourcesCacheDirs :: IO [FilePath]
sourcesCacheDirs = do
    h <- getXdgDirectory XdgCache ""
    mNix <- lookupEnv "IHC_NIX_SOURCE_DIR"
    pure (List.nub (maybe id (:) mNix [h </> "ihc" </> "sources"]))

-- | Fold a list of 'ModuleManifest' into the denormalised lookup
-- tables.  Multiple modules can provide instances for the same class;
-- multiple classes can declare the same method (rare but possible).
buildIndex :: [ModuleManifest] -> ManifestIndex
buildIndex ms = ManifestIndex
    { miClassProviders = providers
    , miClassHeads     = classHeads
    , miMethodOwner    = methodOwners
    , miTypeProviders  = typeProviders
    }
  where
    providers = foldr addProviders Map.empty ms
    addProviders mm acc = foldr (addProv (mmName mm)) acc (mmInstances mm)
    addProv modName ie =
        Map.insertWith Set.union (ieClassName ie) (Set.singleton modName)

    classHeads = foldr addHeads Map.empty ms
    addHeads mm acc = foldr addHead acc (mmInstances mm)
    -- The instance head's first type name (the head constructor) is
    -- what dispatch ultimately tags on.  @instTypeNames@ may be longer
    -- for MPTC; the first slot is the principal head.
    addHead ie acc = case ieTypeNames ie of
        []      -> acc
        (h : _) -> Map.insertWith (++) (ieClassName ie) [h] acc

    methodOwners = foldr addMethods Map.empty ms
    addMethods mm acc = foldr addClass acc (mmClasses mm)
    addClass ce acc0 = foldr (\m -> Map.insert m (ceClassName ce)) acc0
                             (ceMethodNames ce)

    -- Two old/new versions of @base@ may both ship a definition for
    -- the same type (e.g. @data Maybe@ in both @GHC.Maybe@ from old
    -- @base@ and @GHC.Internal.Maybe@ from new @ghc-internal@).
    -- Prefer the modern @GHC.Internal.*@ home so dispatch resolves
    -- against the canonical type tag.
    typeProviders = foldr addTypes Map.empty ms
    addTypes mm acc = foldr (addType (mmName mm)) acc (mmTypeCtors mm)
    addType modName tyName acc =
        case Map.lookup tyName acc of
            Nothing -> Map.insert tyName modName acc
            Just existing
                | isInternal modName && not (isInternal existing) ->
                    Map.insert tyName modName acc
                | otherwise -> acc
    isInternal m = BC.pack "GHC.Internal." `BC.isPrefixOf` m

--------------------------------------------------------------------------------
-- Convenience queries
--------------------------------------------------------------------------------

-- | Modules that declare an @instance ClassName T@ for the given class,
-- across the whole source cache.  Empty if the class is unknown.
providersForClass :: ManifestIndex -> ByteString -> Set ByteString
providersForClass idx cls =
    Map.findWithDefault Set.empty cls (miClassProviders idx)

-- | Class name a method belongs to, if known.  @Nothing@ if no
-- 'class C ... where method ...' was scanned for this name.
classForMethod :: ManifestIndex -> ByteString -> Maybe ByteString
classForMethod idx m = Map.lookup m (miMethodOwner idx)

-- | Given a set of identifiers (typically the union of free variables
-- across the user's entry source), pick out the ones that are class
-- methods according to the manifest, look up each method's class, and
-- return the union of provider modules across all referenced classes.
-- Also pulls in modules that DEFINE the head types of those instances
-- (so loading @instance Monad Maybe@ from @GHC.Internal.Base@ also
-- loads @data Maybe@ from @GHC.Internal.Maybe@), without which dispatch
-- can't resolve the instance head's type tag.
providerModulesForMethods :: ManifestIndex -> Set ByteString -> Set ByteString
providerModulesForMethods idx names = providers `Set.union` headTypeModules
  where
    classes = [ cls | n <- Set.toList names, Just cls <- [classForMethod idx n] ]
    providers = Set.unions [ providersForClass idx cls | cls <- classes ]
    -- For each referenced class, look up its instance heads and add
    -- the defining modules of those head types.  Handles
    -- @data Maybe@ in @GHC.Internal.Maybe@ vs. @instance Monad Maybe@
    -- in @GHC.Internal.Base@ split.
    headTypeModules = Set.fromList
        [ definingMod
        | cls <- classes
        , tyName <- Map.findWithDefault [] cls (miClassHeads idx)
        , Just definingMod <- [Map.lookup tyName (miTypeProviders idx)]
        ]
