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
-- The index is built once per process and reused.  Two rules keep
-- @main = putStrLn "ok"@ from walking Warp:
--
--   1. Only *boot* packages (ghc-internal, ghc-prim, ghc-bignum,
--      ghc-boot*) are scanned.  The scheduler already filters eager
--      providers to @GHC.Internal.*@; Hackage instances arrive via the
--      regular import path ('triggerRegisterInstances'), not this
--      index.  Scanning warp/megaparsec/ihp on every hello was a
--      mistake, not a requirement.
--   2. The resulting index is persisted under
--      @~\/.cache\/ihc\/instance-manifest.bin@, keyed by a cheap
--      mtime fingerprint of those boot package dirs.  A cache hit
--      is a file read; a miss rebuilds and rewrites.  Disable with
--      @IHC_NO_INSTANCE_MANIFEST_CACHE=1@.
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
    , buildIndex
    , providersForClass
    , providersForClassHead
    , classForMethod
    , providerModulesForMethods
    ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, foldM, unless, when)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import System.Directory
    ( XdgDirectory (XdgCache)
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getHomeDirectory
    , getModificationTime
    , getXdgDirectory
    , listDirectory
    , renameFile
    )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
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
    , iePrincipalHead :: !ByteString
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
    , miClassHeadProviders :: !(Map (ByteString, ByteString) (Set ByteString))
        -- ^ (class, normalized instance-head tag) → declaring modules
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
emptyIndex = ManifestIndex Map.empty Map.empty Map.empty Map.empty Map.empty

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
        , iePrincipalHead = Scan.instTypeName i
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
-- First reference: on-disk cache hit (boot-package fingerprint match)
-- or a scan of boot libraries only.  Subsequent references are
-- constant-time map lookups.  The test suite's fixtures share one
-- load; CLI processes share the on-disk file.
{-# NOINLINE manifestIndex #-}
manifestIndex :: ManifestIndex
manifestIndex = unsafePerformIO loadOrBuildIndex

-- | Load the persisted index when the boot-package fingerprint still
-- matches; otherwise scan boot libraries and rewrite the cache.
loadOrBuildIndex :: IO ManifestIndex
loadOrBuildIndex = do
    disabled <- cacheDisabled
    fp <- bootFingerprint
    path <- cacheFilePath
    if disabled
        then rebuildAndMaybeStore False path fp
        else do
            cached <- tryReadCache path fp
            case cached of
                Just idx -> pure idx
                Nothing  -> rebuildAndMaybeStore True path fp

rebuildAndMaybeStore :: Bool -> FilePath -> ByteString -> IO ManifestIndex
rebuildAndMaybeStore writeIt path fp = do
    idx <- buildIndexFromBootSources
    when writeIt $ writeCache path fp idx
    pure idx

-- | Walk *boot* package directories only, scan every @.hs@ file once,
-- and merge into a single index.  Warp / megaparsec / IHP are not
-- boot libraries and must not be scanned for @main = putStrLn "ok"@.
buildIndexFromBootSources :: IO ManifestIndex
buildIndexFromBootSources = do
    sourceDirs <- sourcesCacheDirs
    packageDirs <- concat <$> mapM bootPackagesUnder sourceDirs
    case packageDirs of
        [] -> pure emptyIndex
        _  -> do
            manifests <- mapM
                (\(pkg, dir) -> generatePackageManifest (BC.pack pkg) dir)
                (List.sort packageDirs)
            pure (buildIndex (concatMap (Map.elems . pmModules) manifests))

-- | Package directories that define the instances hello-world actually
-- needs (the ones 'loadProgramFromSource' eagerly filters to
-- @GHC.Internal.*@).  Name prefixes, not a module-name list.
isBootPackageName :: FilePath -> Bool
isBootPackageName name =
    any (`List.isPrefixOf` name)
        [ "ghc-internal-"
        , "ghc-prim-"
        , "ghc-bignum-"
        , "ghc-boot-th-"
        , "ghc-boot-"
        ]

bootPackagesUnder :: FilePath -> IO [(String, FilePath)]
bootPackagesUnder sourcesDir = do
    sourcesExist <- doesDirectoryExist sourcesDir
    if not sourcesExist
        then pure []
        else do
            entries <- listDirectory sourcesDir
            let candidates =
                    [ e | e <- entries
                    , not (isPrefixOfDot e)
                    , isBootPackageName e
                    ]
            packages <- filterM (\p -> doesDirectoryExist (sourcesDir </> p)) candidates
            pure [ (p, sourcesDir </> p) | p <- packages ]

isPrefixOfDot :: FilePath -> Bool
isPrefixOfDot ('.' : _) = True
isPrefixOfDot _         = False

sourcesCacheDirs :: IO [FilePath]
sourcesCacheDirs = do
    h <- getXdgDirectory XdgCache ""
    mNix <- lookupEnv "IHC_NIX_SOURCE_DIR"
    pure (List.nub (maybe id (:) mNix [h </> "ihc" </> "sources"]))

--------------------------------------------------------------------------------
-- On-disk cache
--------------------------------------------------------------------------------

cacheDisabled :: IO Bool
cacheDisabled = do
    m <- lookupEnv "IHC_NO_INSTANCE_MANIFEST_CACHE"
    pure $ case m of
        Just s | not (null s) && s /= "0" -> True
        _ -> False

cacheFilePath :: IO FilePath
cacheFilePath = do
    m <- lookupEnv "IHC_INSTANCE_MANIFEST_CACHE"
    case m of
        Just p | not (null p) -> pure p
        _ -> do
            home <- getHomeDirectory
            pure (home </> ".cache" </> "ihc" </> "instance-manifest.bin")

-- | Cheap fingerprint: nix source dir + each boot package directory's
-- name and mtime.  Adding/removing a boot package or pointing
-- @IHC_NIX_SOURCE_DIR@ at a new store path invalidates the cache.
-- Content hashing is avoided: stat of ~4 dirs is milliseconds.
bootFingerprint :: IO ByteString
bootFingerprint = do
    nix <- fromMaybe "" <$> lookupEnv "IHC_NIX_SOURCE_DIR"
    sourceDirs <- sourcesCacheDirs
    pkgs <- concat <$> mapM bootPackagesUnder sourceDirs
    bits <- mapM pkgBit (sort pkgs)
    pure $ BC.pack $ List.intercalate ";" (nix : bits)
  where
    pkgBit (name, dir) = do
        r <- try (getModificationTime dir) :: IO (Either SomeException UTCTime)
        let t = case r of
                Right u -> show (toRational (utcTimeToPOSIXSeconds u))
                Left _  -> "missing"
        pure (name <> "=" <> t)

tryReadCache :: FilePath -> ByteString -> IO (Maybe ManifestIndex)
tryReadCache path fp = do
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            r <- try (BS.readFile path) :: IO (Either SomeException ByteString)
            case r of
                Left _   -> pure Nothing
                Right bs -> pure (decodeCache fp bs)

writeCache :: FilePath -> ByteString -> ManifestIndex -> IO ()
writeCache path fp idx = do
    let payload = encodeCache fp idx
        tmp     = path <> ".tmp"
    r <- try (do
            createDirectoryIfMissing True (takeDirectory path)
            BS.writeFile tmp payload
            renameFile tmp path) :: IO (Either SomeException ())
    case r of
        Right () -> pure ()
        Left _   -> pure ()  -- cache is best-effort

cacheMagic :: ByteString
cacheMagic = BC.pack "IHCIM1\n"

encodeCache :: ByteString -> ManifestIndex -> ByteString
encodeCache fp idx =
    cacheMagic <> fp <> BC.singleton '\n' <> encodeIndex idx

decodeCache :: ByteString -> ByteString -> Maybe ManifestIndex
decodeCache expectedFp bs = do
    rest0 <- BS.stripPrefix cacheMagic bs
    let (fp, rest1) = BS.break (== 10) rest0  -- '\n'
    rest2 <- BS.stripPrefix (BC.singleton '\n') rest1
    if fp /= expectedFp
        then Nothing
        else decodeIndex rest2

encodeIndex :: ManifestIndex -> ByteString
encodeIndex idx =
    BL.toStrict $ BB.toLazyByteString $
        putMapSet (miClassProviders idx)
        <> putMapList (miClassHeads idx)
        <> putPairMapSet (miClassHeadProviders idx)
        <> putMapBS (miMethodOwner idx)
        <> putMapBS (miTypeProviders idx)

decodeIndex :: ByteString -> Maybe ManifestIndex
decodeIndex bs0 = do
    (providers, bs1) <- getMapSet bs0
    (heads, bs2)     <- getMapList bs1
    (headProvs, bs3) <- getPairMapSet bs2
    (owners, bs4)    <- getMapBS bs3
    (types, rest)    <- getMapBS bs4
    unless (BS.null rest) Nothing
    pure ManifestIndex
        { miClassProviders = providers
        , miClassHeads = heads
        , miClassHeadProviders = headProvs
        , miMethodOwner = owners
        , miTypeProviders = types
        }

putWord32 :: Word32 -> BB.Builder
putWord32 = BB.word32BE

putBS :: ByteString -> BB.Builder
putBS b = putWord32 (fromIntegral (BS.length b)) <> BB.byteString b

putMapSet :: Map ByteString (Set ByteString) -> BB.Builder
putMapSet m =
    putWord32 (fromIntegral (Map.size m))
    <> Map.foldMapWithKey
        (\k s -> putBS k
              <> putWord32 (fromIntegral (Set.size s))
              <> foldMap putBS (Set.toAscList s))
        m

putMapList :: Map ByteString [ByteString] -> BB.Builder
putMapList m =
    putWord32 (fromIntegral (Map.size m))
    <> Map.foldMapWithKey
        (\k xs -> putBS k
               <> putWord32 (fromIntegral (length xs))
               <> foldMap putBS xs)
        m

putPairMapSet :: Map (ByteString, ByteString) (Set ByteString) -> BB.Builder
putPairMapSet m =
    putWord32 (fromIntegral (Map.size m))
    <> Map.foldMapWithKey
        (\(a, b) s -> putBS a <> putBS b
                   <> putWord32 (fromIntegral (Set.size s))
                   <> foldMap putBS (Set.toAscList s))
        m

putMapBS :: Map ByteString ByteString -> BB.Builder
putMapBS m =
    putWord32 (fromIntegral (Map.size m))
    <> Map.foldMapWithKey (\k v -> putBS k <> putBS v) m

getWord32 :: ByteString -> Maybe (Word32, ByteString)
getWord32 bs
    | BS.length bs < 4 = Nothing
    | otherwise =
        let w =  (fromIntegral (BS.index bs 0) :: Word32) `shiftL` 24
             .|. (fromIntegral (BS.index bs 1) :: Word32) `shiftL` 16
             .|. (fromIntegral (BS.index bs 2) :: Word32) `shiftL` 8
             .|.  fromIntegral (BS.index bs 3)
        in Just (w, BS.drop 4 bs)

getBS :: ByteString -> Maybe (ByteString, ByteString)
getBS bs = do
    (n, rest) <- getWord32 bs
    let i = fromIntegral n
    if BS.length rest < i
        then Nothing
        else Just (BS.take i rest, BS.drop i rest)

getCount :: ByteString -> Maybe (Int, ByteString)
getCount bs = do
    (n, rest) <- getWord32 bs
    Just (fromIntegral n, rest)

replicateGet :: Int -> (ByteString -> Maybe (a, ByteString))
             -> ByteString -> Maybe ([a], ByteString)
replicateGet n0 f = go n0
  where
    go 0 bs = Just ([], bs)
    go n bs = do
        (x, rest) <- f bs
        (xs, rest') <- go (n - 1) rest
        Just (x : xs, rest')

getMapSet :: ByteString -> Maybe (Map ByteString (Set ByteString), ByteString)
getMapSet bs = do
    (n, rest) <- getCount bs
    (kvs, rest') <- replicateGet n getEntry rest
    Just (Map.fromList kvs, rest')
  where
    getEntry b = do
        (k, b1) <- getBS b
        (m, b2) <- getCount b1
        (vs, b3) <- replicateGet m getBS b2
        Just ((k, Set.fromList vs), b3)

getMapList :: ByteString -> Maybe (Map ByteString [ByteString], ByteString)
getMapList bs = do
    (n, rest) <- getCount bs
    (kvs, rest') <- replicateGet n getEntry rest
    Just (Map.fromList kvs, rest')
  where
    getEntry b = do
        (k, b1) <- getBS b
        (m, b2) <- getCount b1
        (vs, b3) <- replicateGet m getBS b2
        Just ((k, vs), b3)

getPairMapSet
    :: ByteString
    -> Maybe (Map (ByteString, ByteString) (Set ByteString), ByteString)
getPairMapSet bs = do
    (n, rest) <- getCount bs
    (kvs, rest') <- replicateGet n getEntry rest
    Just (Map.fromList kvs, rest')
  where
    getEntry b = do
        (a, b1) <- getBS b
        (c, b2) <- getBS b1
        (m, b3) <- getCount b2
        (vs, b4) <- replicateGet m getBS b3
        Just (((a, c), Set.fromList vs), b4)

getMapBS :: ByteString -> Maybe (Map ByteString ByteString, ByteString)
getMapBS bs = do
    (n, rest) <- getCount bs
    (kvs, rest') <- replicateGet n getEntry rest
    Just (Map.fromList kvs, rest')
  where
    getEntry b = do
        (k, b1) <- getBS b
        (v, b2) <- getBS b1
        Just ((k, v), b2)

-- | Fold a list of 'ModuleManifest' into the denormalised lookup
-- tables.  Multiple modules can provide instances for the same class;
-- multiple classes can declare the same method (rare but possible).
buildIndex :: [ModuleManifest] -> ManifestIndex
buildIndex ms = ManifestIndex
    { miClassProviders = providers
    , miClassHeads     = classHeads
    , miClassHeadProviders = headProviders
    , miMethodOwner    = methodOwners
    , miTypeProviders  = typeProviders
    }
  where
    providers = foldr addProviders Map.empty ms
    addProviders mm acc = foldr (addProv (mmName mm)) acc (mmInstances mm)
    addProv modName ie =
        Map.insertWith Set.union (ieClassName ie) (Set.singleton modName)

    headProviders = foldr addHeadProviders Map.empty ms
    addHeadProviders mm acc = foldr (addOne (mmName mm)) acc (mmInstances mm)
    addOne modName ie =
        Map.insertWith Set.union (ieClassName ie, iePrincipalHead ie)
            (Set.singleton modName)

    classHeads = foldr addHeads Map.empty ms
    addHeads mm acc = foldr addHead acc (mmInstances mm)
    -- The instance head's first type name (the head constructor) is
    -- what dispatch ultimately tags on.  @instTypeNames@ may be longer
    -- for MPTC; the first slot is the principal head.
    addHead ie = Map.insertWith (++) (ieClassName ie) [iePrincipalHead ie]

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

providersForClassHead :: ManifestIndex -> ByteString -> ByteString -> Set ByteString
providersForClassHead idx cls headTag =
    Map.findWithDefault Set.empty (cls, headTag) (miClassHeadProviders idx)

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
