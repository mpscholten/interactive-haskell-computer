-- | Phase 2.7 — Cabal project detection and dependency resolution.
--
-- Given a Haskell file path, walk up the directory tree looking for a
-- @cabal.project@ or @*.cabal@ file. If found, parse the project's
-- local @.cabal@ stanza to get its @hs-source-dirs@ and
-- @default-extensions@, and read @cabal.project.freeze@ (or fall back
-- to @cabal v2-build --dry-run@) to get the pinned @(pkg, version)@
-- list of every transitive Hackage dependency.
--
-- The @PackageInfo@ table is later consumed by 'IHC.PackageStore' to
-- materialise per-package source directories into a flat
-- 'seSearchPath' that Phase 2.5's scheduler can use as-is.
--
-- Philosophy: be optimistic. Unparseable @.cabal@ → empty extensions,
-- log + skip. Missing freeze file → hard error with a helpful
-- message. 'resolve' never refuses to return *some* list; the caller
-- decides how fatal any individual miss is.
module IHC.CabalProject
    ( -- * Types
      PackageInfo(..)
    , SearchEnv(..)
    , CabalProjectError(..)
      -- * Project detection
    , detectProjectRoot
      -- * Dependency resolution
    , resolve
      -- * Parsing a single .cabal file
    , parseCabalFile
      -- * Utilities (exposed for testing)
    , parseFreezeFile
    , findLocalCabalFile
      -- * Cache-wide search path
    , cachedPackageSearchPath
    , cabalTarballSearchPath
    ) where

import Control.Exception (Exception, throwIO, try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getHomeDirectory
    , listDirectory
    )
import System.Environment (lookupEnv)
import System.FilePath
    ( (</>)
    , takeDirectory
    , takeExtension
    , splitDirectories
    , isDrive
    )

import Distribution.PackageDescription
    ( GenericPackageDescription(..)
    , PackageDescription(..)
    , CondTree(..)
    , Library(..)
    , BuildInfo(..)
    , library
    )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import qualified Distribution.Types.PackageId as CabalPkgId
import qualified Distribution.Types.PackageName as CabalPkgName
import qualified Distribution.Types.Version as CabalVer
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Pretty as CabalPretty
import qualified Distribution.Utils.Path as CabalPath

--------------------------------------------------------------------------------
-- Public types
--------------------------------------------------------------------------------

-- | Everything we care about from a single @.cabal@ file for the
-- purposes of module loading. See the Phase 2.7 plan for context.
data PackageInfo = PackageInfo
    { pkgName       :: !ByteString
    , pkgVersion    :: !ByteString
    , pkgSourceDirs :: ![FilePath]
      -- ^ Absolute paths to the package's @hs-source-dirs@. Defaults
      -- to the package root itself if the @.cabal@ file omits
      -- @hs-source-dirs@.
    , pkgExtensions :: ![ByteString]
      -- ^ Library's @default-extensions@, verbatim.
    , pkgCppOptions :: ![ByteString]
      -- ^ Library's @cpp-options@, verbatim.
    }
    deriving stock (Show, Eq)

-- | The result of resolving a Cabal project: a flat search path (in
-- priority order: local dirs first, then every transitive dependency)
-- plus a map from each source directory back to the owning package.
data SearchEnv = SearchEnv
    { seSearchPath   :: ![FilePath]
    , sePackageTable :: !(Map FilePath PackageInfo)
    }
    deriving stock (Show)

data CabalProjectError
    = NoFreezeFile !FilePath
    | CabalParseFailed !FilePath !String
    deriving stock (Show)
instance Exception CabalProjectError

--------------------------------------------------------------------------------
-- Project root detection
--------------------------------------------------------------------------------

-- | Walk up from the given directory looking for a @cabal.project@ or
-- a @*.cabal@ file. Returns the first ancestor directory that
-- contains one, or 'Nothing' if we hit the filesystem root without
-- finding anything.
detectProjectRoot :: FilePath -> IO (Maybe FilePath)
detectProjectRoot start = go =<< canonicalise start
  where
    canonicalise :: FilePath -> IO FilePath
    canonicalise p = do
        e <- doesDirectoryExist p
        if e then pure p else pure (takeDirectory p)

    go dir
        | isDrive dir || null (splitDirectories dir) = pure Nothing
        | otherwise = do
            hasProject <- doesFileExist (dir </> "cabal.project")
            if hasProject
                then pure (Just dir)
                else do
                    mCabal <- findLocalCabalFile dir
                    case mCabal of
                        Just _  -> pure (Just dir)
                        Nothing ->
                            let up = takeDirectory dir
                            in if up == dir then pure Nothing else go up

-- | Return the path of any @*.cabal@ file sitting directly in @dir@
-- (not recursively). If there are multiple (rare), the first one
-- alphabetically wins.
findLocalCabalFile :: FilePath -> IO (Maybe FilePath)
findLocalCabalFile dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure Nothing
        else do
            entries <- getDirectoryContents dir
            let cabals = [ dir </> e
                         | e <- entries
                         , takeExtension e == ".cabal"
                         ]
            case cabals of
                (p:_) -> pure (Just p)
                []    -> pure Nothing

--------------------------------------------------------------------------------
-- Dependency resolution
--------------------------------------------------------------------------------

-- | Produce a @(package, version)@ list for every transitive Hackage
-- dependency of the project rooted at @projectRoot@. Strategy:
--
--   1. If @cabal.project.freeze@ exists, parse it (accurate, fast,
--      the common case).
--   2. Otherwise, shell out to @cabal v2-build --dry-run@ and scrape
--      the output. (Not yet implemented — hard error for now, per
--      the Phase 2.7 plan.)
--
-- Missing freeze file raises 'NoFreezeFile'.
resolve :: FilePath -> IO [(ByteString, ByteString)]
resolve projectRoot = do
    let freeze = projectRoot </> "cabal.project.freeze"
    hasFreeze <- doesFileExist freeze
    if hasFreeze
        then parseFreezeFile freeze
        else throwIO (NoFreezeFile freeze)

-- | Parse a @cabal.project.freeze@ file and extract the
-- @(name, version)@ pairs from its @constraints:@ stanza.
--
-- The file format is effectively a single block of the form:
--
-- @
-- constraints: any.foo ==1.2.3,
--              any.bar ==4.5.6,
--              baz +flag,
--              ...
-- @
--
-- We only care about entries with @==@; flag constraints and other
-- stanzas are ignored.
parseFreezeFile :: FilePath -> IO [(ByteString, ByteString)]
parseFreezeFile path = do
    bs <- BS.readFile path
    pure (extractPinnedVersions bs)

-- | Given the raw bytes of a freeze file, pull out every @any.NAME ==VER@
-- entry. Robust to whitespace, commas, and line continuations.
extractPinnedVersions :: ByteString -> [(ByteString, ByteString)]
extractPinnedVersions bs =
    -- Split on commas, then on "==" within each fragment; pick out
    -- the ones that look like @any.NAME ==VER@ or @NAME ==VER@.
    let fragments = BC.split ',' (stripComments bs)
    in mapMaybe extract fragments
  where
    stripComments = BC.unlines
        . map (stripLineComment . killConstraintsPrefix)
        . BC.lines
    -- Strip a Haskell-style line comment ("--" onwards), being careful
    -- NOT to strip on a lone '-' — package names may contain hyphens.
    stripLineComment bs =
        case BC.breakSubstring (BC.pack "--") bs of
            (before, _) -> before
    -- Drop a leading @constraints:@ token so the first entry is as
    -- uniform as the others.
    killConstraintsPrefix line =
        let line' = BC.dropWhile (== ' ') line
            prefix = BC.pack "constraints:"
        in if prefix `BC.isPrefixOf` line'
               then BC.drop (BC.length prefix) line'
               else line

    extract frag =
        let cleaned = BC.dropWhile isSpaceOrNL
                    . BC.reverse
                    . BC.dropWhile isSpaceOrNL
                    . BC.reverse
                    $ frag
        in case BC.breakSubstring (BC.pack "==") cleaned of
            (lhs, rest) | not (BC.null rest) ->
                let name = trimName (stripAny lhs)
                    ver  = trimVer (BC.drop 2 rest)
                in if BC.null name || BC.null ver
                       then Nothing
                       else Just (name, ver)
            _ -> Nothing

    stripAny s =
        let t = BC.dropWhile isSpaceOrNL s
            anyP = BC.pack "any."
        in if anyP `BC.isPrefixOf` t
               then BC.drop (BC.length anyP) t
               else t

    trimName = BC.takeWhile isNameChar . BC.dropWhile isSpaceOrNL
    trimVer  = BC.takeWhile isVerChar  . BC.dropWhile isSpaceOrNL

    isSpaceOrNL c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
    isNameChar c =
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c == '-' || c == '_'
    isVerChar c =
        (c >= '0' && c <= '9') || c == '.'

--------------------------------------------------------------------------------
-- Parsing a single .cabal file
--------------------------------------------------------------------------------

-- | Parse a @.cabal@ file into a 'PackageInfo'. The @packageRoot@ is
-- used to turn relative @hs-source-dirs@ into absolute paths.
--
-- If parsing fails catastrophically, returns 'Nothing' (optimistic:
-- caller logs + skips). Missing library stanza yields a PackageInfo
-- with an empty @pkgSourceDirs@.
parseCabalFile :: FilePath -> FilePath -> IO (Maybe PackageInfo)
parseCabalFile packageRoot cabalPath = do
    r <- try (BS.readFile cabalPath) :: IO (Either SomeException ByteString)
    case r of
        Left _   -> pure Nothing
        Right bs ->
            case snd (runParseResult (parseGenericPackageDescription bs)) of
                Left _         -> pure Nothing
                Right gpd      -> pure (Just (gpdToPackageInfo packageRoot gpd))

-- | Extract the bits we care about from a parsed
-- 'GenericPackageDescription'. We only look at the unconditional
-- library stanza; conditional branches (@if impl(...)@) are flattened
-- by the caller if needed, but for the interpreter's purposes the
-- base library is enough.
gpdToPackageInfo :: FilePath -> GenericPackageDescription -> PackageInfo
gpdToPackageInfo packageRoot gpd =
    let pkgId   = package (packageDescription gpd)
        name    = BC.pack (CabalPkgName.unPackageName (CabalPkgId.pkgName pkgId))
        version = BC.pack (CabalPretty.prettyShow
                             (CabalPkgId.pkgVersion pkgId :: CabalVer.Version))
        mLib    = fmap condTreeData (condLibrary gpd)
        bi      = fromMaybe emptyBuildInfoStub (fmap libBuildInfo mLib)
        srcDirs0 = hsSourceDirs bi
        srcDirs  = if null srcDirs0
                       then [packageRoot]
                       else [ packageRoot </> CabalPath.getSymbolicPath sd
                            | sd <- srcDirs0
                            ]
        exts     = [ BC.pack (CabalPretty.prettyShow e)
                   | e <- defaultExtensions bi
                   ]
        cppOpts  = [ BC.pack s | s <- cppOptions bi ]
    in PackageInfo
        { pkgName       = name
        , pkgVersion    = version
        , pkgSourceDirs = srcDirs
        , pkgExtensions = exts
        , pkgCppOptions = cppOpts
        }

-- Local stub so we don't depend on a specific Cabal BuildInfo default
-- constructor name (which has shifted across Cabal versions).
emptyBuildInfoStub :: BuildInfo
emptyBuildInfoStub = libBuildInfo emptyLib
  where
    -- Using mempty works for all Cabal versions from 3.0+.
    emptyLib :: Library
    emptyLib = mempty

--------------------------------------------------------------------------------
-- Cache-wide search path
--------------------------------------------------------------------------------

-- | Enumerate every package cached under @~\/.cache\/ihc\/sources\/@
-- and (if the @IHC_NIX_SOURCE_DIR@ environment variable is set) under
-- the nix-provided source tree, returning the actual source roots
-- (respecting @hs-source-dirs@ from each package's @.cabal@ file).
--
-- Algorithm for each subdirectory @\<pkg\>-\<ver\>@:
--
--   1. Find the @.cabal@ file (via 'findLocalCabalFile').
--   2. Parse it with 'parseCabalFile' to get @pkgSourceDirs@.
--   3. Append those absolute paths to the result.
--
-- If no @.cabal@ file is found or it fails to parse, fall back to
-- checking whether a @src\/@ subdirectory exists (common convention);
-- otherwise use the package root itself.
--
-- Priority order (highest first):
--
--   1. Nix-pinned source tree (@IHC_NIX_SOURCE_DIR@) — reproducible.
--   2. User-managed @~\/.cache\/ihc\/sources\/@ — @cabal get@ overrides.
--   3. Cabal tarball cache @~\/.cabal\/packages\/hackage.haskell.org\/@ —
--      already-extracted tarballs, broadest pool.
--
-- Returns an empty list if none of the directories exist.
cachedPackageSearchPath :: IO [FilePath]
cachedPackageSearchPath = do
    nixDirs     <- nixSourceDirs
    userDirs    <- enumerateSourceDir =<< userCacheDir
    cabalDirs   <- cabalTarballSearchPath
    pure (nixDirs ++ userDirs ++ cabalDirs)
  where
    userCacheDir :: IO FilePath
    userCacheDir = do
        home <- getHomeDirectory
        pure (home </> ".cache" </> "ihc" </> "sources")

    -- Enumerate the nix-pinned source tree exported by the devShell.
    -- If the variable is unset or the path does not exist, returns [].
    nixSourceDirs :: IO [FilePath]
    nixSourceDirs = do
        mDir <- lookupEnv "IHC_NIX_SOURCE_DIR"
        case mDir of
            Nothing  -> pure []
            Just dir -> enumerateSourceDir dir

    enumerateSourceDir :: FilePath -> IO [FilePath]
    enumerateSourceDir sourcesDir = do
        exists <- doesDirectoryExist sourcesDir
        if not exists
            then pure []
            else do
                entries <- listDirectory sourcesDir
                concat <$> mapM (dirsForEntry sourcesDir) entries

    dirsForEntry sourcesDir entry = do
        let pkgDir = sourcesDir </> entry
        isDir <- doesDirectoryExist pkgDir
        if not isDir
            then pure []
            else do
                mCabal <- findLocalCabalFile pkgDir
                case mCabal of
                    Just cabalPath -> do
                        mInfo <- parseCabalFile pkgDir cabalPath
                        case mInfo of
                            Just info -> pure (pkgSourceDirs info)
                            Nothing   -> fallback pkgDir
                    Nothing -> fallback pkgDir

    -- When we can't parse a .cabal, try the conventional src/ dir first,
    -- then fall back to the package root.
    fallback pkgDir = do
        let srcDir = pkgDir </> "src"
        hasSrc <- doesDirectoryExist srcDir
        pure [if hasSrc then srcDir else pkgDir]

-- | Enumerate already-extracted source trees from Cabal's local tarball
-- cache at @~\/.cabal\/packages\/hackage.haskell.org\/@.
--
-- Cabal stores downloaded tarballs as:
--
-- @
-- ~\/.cabal\/packages\/hackage.haskell.org\/\<pkg\>\/\<ver\>\/\<pkg\>-\<ver\>.tar.gz
-- @
--
-- When a user runs @cabal get \<pkg\>@ (or cabal extracts it for inspection),
-- the unpacked tree appears as a sibling directory:
--
-- @
-- ~\/.cabal\/packages\/hackage.haskell.org\/\<pkg\>\/\<ver\>\/\<pkg\>-\<ver\>\/
-- @
--
-- This function enumerates those already-extracted directories only —
-- it never extracts tarballs itself. That keeps startup overhead
-- proportional to the number of @cabal get@ invocations the user has
-- actually done, not the total number of packages in the store.
--
-- Returns an empty list if the cabal package store does not exist or
-- contains no extracted source trees.
cabalTarballSearchPath :: IO [FilePath]
cabalTarballSearchPath = do
    home <- getHomeDirectory
    let cabalRoot = home </> ".cabal" </> "packages" </> "hackage.haskell.org"
    exists <- doesDirectoryExist cabalRoot
    if not exists
        then pure []
        else do
            pkgDirs <- listDirectory cabalRoot
            fmap concat $ mapM (dirsForPkg cabalRoot) pkgDirs
  where
    dirsForPkg cabalRoot pkg = do
        let pkgRoot = cabalRoot </> pkg
        isDir <- doesDirectoryExist pkgRoot
        if not isDir
            then pure []
            else do
                verDirs <- listDirectory pkgRoot
                fmap concat $ mapM (dirsForVer pkgRoot pkg) verDirs

    dirsForVer pkgRoot pkg ver = do
        let verRoot   = pkgRoot </> ver
            extracted = verRoot </> (pkg <> "-" <> ver)
        hasDir <- doesDirectoryExist extracted
        if not hasDir
            then pure []
            else resolveHsSourceDirs extracted

    -- | Resolve the actual @hs-source-dirs@ for a package root by
    -- parsing its @.cabal@ file. Falls back to a @src\/@ check and then
    -- the package root itself, matching the behaviour of 'dirsForEntry'
    -- in 'cachedPackageSearchPath'.
    resolveHsSourceDirs :: FilePath -> IO [FilePath]
    resolveHsSourceDirs pkgDir = do
        mCabal <- findLocalCabalFile pkgDir
        case mCabal of
            Just cabalPath -> do
                mInfo <- parseCabalFile pkgDir cabalPath
                case mInfo of
                    Just info -> pure (pkgSourceDirs info)
                    Nothing   -> fallbackDir pkgDir
            Nothing -> fallbackDir pkgDir

    fallbackDir pkgDir = do
        let srcDir = pkgDir </> "src"
        hasSrc <- doesDirectoryExist srcDir
        pure [if hasSrc then srcDir else pkgDir]
