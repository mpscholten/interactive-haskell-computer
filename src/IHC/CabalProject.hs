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
    , cachedPackageSearchPathWithIncludes
    , cabalTarballSearchPath
    ) where

import Control.Exception (Exception, throwIO, try, SomeException, evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (isSuffixOf, sortBy)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import GHC.IO (unsafePerformIO)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , getHomeDirectory
    , getModificationTime
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
import System.IO (hPutStrLn, stderr)

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
import qualified Language.Haskell.Extension as CabalLang

--------------------------------------------------------------------------------
-- Public types
--------------------------------------------------------------------------------

-- | Everything we care about from a single @.cabal@ file for the
-- purposes of module loading. See the Phase 2.7 plan for context.
data PackageInfo = PackageInfo
    { pkgName        :: !ByteString
    , pkgVersion     :: !ByteString
    , pkgSourceDirs  :: ![FilePath]
      -- ^ Absolute paths to the package's @hs-source-dirs@. Defaults
      -- to the package root itself if the @.cabal@ file omits
      -- @hs-source-dirs@.
    , pkgExtensions  :: ![ByteString]
      -- ^ Library's @default-extensions@, verbatim.
    , pkgCppOptions  :: ![ByteString]
      -- ^ Library's @cpp-options@, verbatim.
    , pkgIncludeDirs :: ![FilePath]
      -- ^ Absolute paths from the library's @include-dirs:@ stanza.
      -- These are the directories the C preprocessor searches when
      -- resolving @#include \"file.h\"@ directives.
    , pkgExtraLibs   :: ![ByteString]
      -- ^ @extra-libraries:@ stanza — bare library names (e.g. @"pq"@
      -- for @hasql -> postgresql-libpq@, @"z"@ for @zlib@).  The FFI
      -- dispatcher (see 'IHC.FFI.registerLibrary') consumes these at
      -- load time to @dlopen@ the corresponding shared library so its
      -- C symbols become resolvable from @foreign import ccall@ decls.
    , pkgPkgConfig   :: ![ByteString]
      -- ^ @pkgconfig-depends:@ stanza — package-config names (e.g.
      -- @"libpq"@).  Resolved to concrete lib names via @pkg-config
      -- --libs@ by the FFI loader.
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
        -- Expand @default-language: GHC2021@ (etc.) into the implied
        -- extension set.  Cabal itself does not do this expansion — it
        -- stores the language separately and leaves it to the compiler
        -- to interpret.  Downstream consumers of 'pkgExtensions' only
        -- ask "is extension X enabled for this package?", so we bake
        -- the implied set in here.
        langExts  = languageImpliedExtensions (defaultLanguage bi)
        declared  = [ BC.pack (CabalPretty.prettyShow e)
                    | e <- defaultExtensions bi
                    ]
        exts      = dedupPreserveOrder (langExts ++ declared)
        cppOpts   = [ BC.pack s | s <- cppOptions bi ]
        incDirs   = [ packageRoot </> d | d <- includeDirs bi ]
        extraLbs  = [ BC.pack s | s <- extraLibs bi ]
        pkgCfg    = [ BC.pack (CabalPretty.prettyShow d)
                    | d <- pkgconfigDepends bi
                    ]
    in PackageInfo
        { pkgName        = name
        , pkgVersion     = version
        , pkgSourceDirs  = srcDirs
        , pkgExtensions  = exts
        , pkgCppOptions  = cppOpts
        , pkgIncludeDirs = incDirs
        , pkgExtraLibs   = extraLbs
        , pkgPkgConfig   = pkgCfg
        }

-- Local stub so we don't depend on a specific Cabal BuildInfo default
-- constructor name (which has shifted across Cabal versions).
emptyBuildInfoStub :: BuildInfo
emptyBuildInfoStub = libBuildInfo emptyLib
  where
    -- Using mempty works for all Cabal versions from 3.0+.
    emptyLib :: Library
    emptyLib = mempty

-- | Expand a @default-language:@ setting into the extensions it
-- implicitly enables.  Cabal itself stores the language as a separate
-- field (see 'Distribution.PackageDescription.defaultLanguage') and
-- does *not* fold its implied set into 'defaultExtensions' — that
-- expansion is normally done by GHC at compile time.  Since ihc
-- consumes 'pkgExtensions' directly to answer "is this extension on?"
-- queries, we have to do the expansion here.
--
-- The implied sets are intentionally hard-coded against the
-- documented GHC user's guide (ghc/docs/users_guide/exts/
-- control.rst).  Keeping them here rather than deriving them from the
-- @Cabal@ library insulates us from future Cabal releases adding or
-- removing entries under our feet.
--
-- Missing / @UnknownLanguage@ is treated as the empty set.  The
-- caller (Scheduler / parser-driver) falls back to the usual
-- interpreter defaults in that case.
languageImpliedExtensions :: Maybe CabalLang.Language -> [ByteString]
languageImpliedExtensions Nothing                = []
languageImpliedExtensions (Just CabalLang.Haskell98) =
    -- Haskell98 implies nothing that's relevant to our parser; leave
    -- empty.  Old-but-explicit packages rely on GHC's built-in
    -- defaults, which ihc matches.
    []
languageImpliedExtensions (Just CabalLang.Haskell2010) = haskell2010ImpliedExtensions
languageImpliedExtensions (Just CabalLang.GHC2021)     = ghc2021ImpliedExtensions
languageImpliedExtensions (Just lang)
    -- GHC2024 and any future-named language land here; unknown
    -- languages fall through to the empty list.  We match the
    -- pretty-printed name so we don't break if a newer Cabal is
    -- linked against us without the constructor yet.
    | name == "GHC2024" = ghc2024ImpliedExtensions
    | otherwise         = []
  where
    name = CabalPretty.prettyShow lang

-- | Extensions implicitly enabled by @default-language: Haskell2010@.
-- Taken verbatim from the GHC user's guide; Cabal's equivalent
-- constant lives in 'Distribution.Compat.Prelude' but is not exposed.
haskell2010ImpliedExtensions :: [ByteString]
haskell2010ImpliedExtensions =
    [ BC.pack e
    | e <-
        [ "DatatypeContexts"
        , "DoAndIfThenElse"
        , "EmptyDataDecls"
        , "ForeignFunctionInterface"
        , "ImplicitPrelude"
        , "MonomorphismRestriction"
        , "PatternGuards"
        , "RelaxedPolyRec"
        , "TraditionalRecordSyntax"
        ]
    ]

-- | Extensions implicitly enabled by @default-language: GHC2021@.
-- Pinned against the ~45 entries listed in the GHC 9.2+ user's guide
-- (chapter "control", section "GHC2021").  Packages that use
-- @GHC2021@ rely on *all* of these being on.
ghc2021ImpliedExtensions :: [ByteString]
ghc2021ImpliedExtensions =
    [ BC.pack e
    | e <-
        [ "BangPatterns"
        , "BinaryLiterals"
        , "ConstrainedClassMethods"
        , "ConstraintKinds"
        , "DeriveDataTypeable"
        , "DeriveFoldable"
        , "DeriveFunctor"
        , "DeriveGeneric"
        , "DeriveLift"
        , "DeriveTraversable"
        , "DoAndIfThenElse"
        , "EmptyCase"
        , "EmptyDataDecls"
        , "EmptyDataDeriving"
        , "ExistentialQuantification"
        , "ExplicitForAll"
        , "FieldSelectors"
        , "FlexibleContexts"
        , "FlexibleInstances"
        , "ForeignFunctionInterface"
        , "GADTSyntax"
        , "GeneralisedNewtypeDeriving"
        , "HexFloatLiterals"
        , "ImplicitPrelude"
        , "ImportQualifiedPost"
        , "InstanceSigs"
        , "KindSignatures"
        , "MonomorphismRestriction"
        , "MultiParamTypeClasses"
        , "NamedFieldPuns"
        , "NamedWildCards"
        , "NumericUnderscores"
        , "PatternGuards"
        , "PolyKinds"
        , "PostfixOperators"
        , "RankNTypes"
        , "RelaxedPolyRec"
        , "ScopedTypeVariables"
        , "StandaloneDeriving"
        , "StandaloneKindSignatures"
        , "StarIsType"
        , "TraditionalRecordSyntax"
        , "TupleSections"
        , "TypeApplications"
        , "TypeOperators"
        , "TypeSynonymInstances"
        ]
    ]

-- | Extensions implicitly enabled by @default-language: GHC2024@.
-- GHC2024 is a superset of GHC2021 plus the extensions listed in the
-- GHC 9.10+ user's guide (chapter "control", section "GHC2024").
-- We expand the full closure here (GHC2021 ∪ GHC2024-new) so
-- callers only need to look at one list.
ghc2024ImpliedExtensions :: [ByteString]
ghc2024ImpliedExtensions =
    dedupPreserveOrder (ghc2021ImpliedExtensions ++ newInGhc2024)
  where
    newInGhc2024 =
        [ BC.pack e
        | e <-
            [ "DataKinds"
            , "DerivingStrategies"
            , "DisambiguateRecordFields"
            , "ExplicitNamespaces"
            , "GADTs"
            , "LambdaCase"
            , "MonoLocalBinds"
            , "RoleAnnotations"
            ]
        ]

-- | De-duplicate a list while preserving first-occurrence order.  The
-- user-declared 'default-extensions' take precedence over the
-- language-implied set only in the sense that they're evaluated in
-- order; the actual semantic precedence (an explicit @No...@ disabling
-- an implied extension) is deferred to whatever downstream consumer
-- interprets the resulting ByteString list.
dedupPreserveOrder :: [ByteString] -> [ByteString]
dedupPreserveOrder = go []
  where
    go _    []     = []
    go seen (x:xs)
        | x `elem` seen = go seen xs
        | otherwise     = x : go (x:seen) xs

--------------------------------------------------------------------------------
-- Cache-wide search path
--
-- Enumerating the cache-wide search path requires parsing every
-- .cabal file under three source roots (IHC_NIX_SOURCE_DIR, the user
-- ihc cache, the cabal tarball cache).  On a typical devshell that's
-- ~350 .cabal files and dominates ihc startup (~100ms of a ~150ms
-- "main = putStrLn \"hi\"" run).
--
-- The result is deterministic for a given filesystem state, so we
-- memoise it at two levels:
--
--   * Process-wide IORef memo: the first call does the real work,
--     every subsequent call in the same process is O(1).  Multiple
--     Scheduler call sites (loadProgramFromSource, loadImportIntoEnv,
--     loadImportOnlyIntoEnv, loadFileIntoEnv) all hit the same memo.
--
--   * On-disk cache at ~/.cache/ihc/search-path.cache: keyed by a
--     cheap mtime signature of the three root dirs plus their
--     immediate children.  If no package has been added / removed /
--     modified since the cache was written, the cache is reused and
--     we skip the entire .cabal parse pass across runs.
--
-- Invalidation signal:
--   * Root dir mtime changes when an immediate child is added / removed.
--   * Each child dir's mtime changes when its own contents are touched
--     (e.g. a .cabal file is edited in place after a re-nix).
-- We combine both so renaming or editing any package dir invalidates
-- the cache.  Content hashing is avoided deliberately — stat'ing ~350
-- dirs is ~1ms, vs reading and hashing hundreds of KB of .cabal data.
--------------------------------------------------------------------------------

-- | Process-wide memo for 'cachedPackageSearchPathWithIncludes'.
--
-- 'Nothing' = not yet computed.  'Just pairs' = computed, reuse
-- verbatim.  Thread-safe via 'atomicModifyIORef''.
searchPathMemoRef :: IORef (Maybe [(FilePath, [FilePath])])
searchPathMemoRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE searchPathMemoRef #-}

-- | Clear the in-process search-path memo.  Exposed for tests that
-- mutate the underlying directories and expect a fresh read.  Not
-- currently wired up; kept as a private helper for future use.
_resetSearchPathMemo :: IO ()
_resetSearchPathMemo = writeIORef searchPathMemoRef Nothing

-- | Fingerprint summarising the state of the source directories the
-- search path depends on.  Two fingerprints agreeing means "nothing
-- relevant changed" and the cached result is still valid.
--
-- Each entry is @(rootAbsPath, rootMtime, [(childName, childMtime)])@.
-- Root mtime catches add/remove of direct children; child mtime
-- catches edits inside the package (e.g. .cabal rewrites).
newtype SearchPathFingerprint = SearchPathFingerprint [(FilePath, POSIXTime, [(FilePath, POSIXTime)])]
    deriving stock (Eq, Show)

-- | Compute a fingerprint for all three source roots (or the subset
-- that exists).  Missing roots are elided, matching the semantics of
-- 'cachedPackageSearchPathWithIncludes'.
computeSearchPathFingerprint :: IO SearchPathFingerprint
computeSearchPathFingerprint = do
    home <- getHomeDirectory
    mNix <- lookupEnv "IHC_NIX_SOURCE_DIR"
    let userCache = home </> ".cache" </> "ihc" </> "sources"
        cabalRoot = home </> ".cabal" </> "packages" </> "hackage.haskell.org"
        roots = maybe id (:) mNix [userCache, cabalRoot]
    entries <- mapM fingerprintRoot roots
    pure (SearchPathFingerprint (mapMaybe id entries))
  where
    fingerprintRoot :: FilePath -> IO (Maybe (FilePath, POSIXTime, [(FilePath, POSIXTime)]))
    fingerprintRoot root = do
        exists <- doesDirectoryExist root
        if not exists
            then pure Nothing
            else do
                rootMtime <- mtime root
                children  <- listDirectory root
                childPairs <- mapM (childEntry root) children
                -- Sort for deterministic comparison.
                let sorted = sortBy (\(a, _) (b, _) -> compare a b)
                                    (mapMaybe id childPairs)
                pure (Just (root, rootMtime, sorted))

    childEntry root c = do
        let p = root </> c
        r <- try (mtime p) :: IO (Either SomeException POSIXTime)
        case r of
            Right t -> pure (Just (c, t))
            Left _  -> pure Nothing

    mtime :: FilePath -> IO POSIXTime
    mtime p = utcTimeToPOSIXSeconds <$> getModificationTime p

-- | Serialise a fingerprint to the on-disk cache format.  Line-based,
-- human-inspectable: one line per (root, mtime, [child, mtime]), with
-- NUL separators between fields.  The cabal tarball cache can contain
-- names with unusual characters but never NULs, so this is unambiguous.
encodeFingerprint :: SearchPathFingerprint -> ByteString
encodeFingerprint (SearchPathFingerprint rs) =
    BC.intercalate (BC.pack "\n") (map encodeRoot rs) <> BC.pack "\n"
  where
    encodeRoot (root, rootM, kids) =
        BC.intercalate (BC.pack "\0")
            ( BC.pack "R"
            : BC.pack root
            : BC.pack (show (toRational rootM))
            : concatMap (\(c, m) -> [BC.pack c, BC.pack (show (toRational m))]) kids
            )

-- | Serialise the computed search path (list of (srcDir, includeDirs))
-- to the on-disk cache format.  One record per line:
-- @"P\0<srcDir>\0<inc1>\0<inc2>..."@.
encodeResult :: [(FilePath, [FilePath])] -> ByteString
encodeResult pairs =
    BC.intercalate (BC.pack "\n") (map encodePair pairs) <> BC.pack "\n"
  where
    encodePair (sd, incs) =
        BC.intercalate (BC.pack "\0")
            (BC.pack "P" : BC.pack sd : map BC.pack incs)

-- | Parse the on-disk cache back into a list of (srcDir, includeDirs).
-- Silently returns 'Nothing' on malformed input — the caller then
-- recomputes and overwrites.
decodeResult :: ByteString -> Maybe [(FilePath, [FilePath])]
decodeResult bs =
    let ls = filter (not . BC.null) (BC.lines bs)
    in mapM decodeLine ls
  where
    decodeLine l = case BC.split '\0' l of
        (tag : sd : incs)
            | tag == BC.pack "P" -> Just (BC.unpack sd, map BC.unpack incs)
        _ -> Nothing

-- | Location of the on-disk cache file.
--
-- Stored next to the other ihc cache contents so the whole directory
-- can be wiped to force a rebuild.
cacheFilePath :: IO FilePath
cacheFilePath = do
    home <- getHomeDirectory
    pure (home </> ".cache" </> "ihc" </> "search-path.cache")

-- | On-disk cache is considered disabled when @IHC_NO_SEARCH_PATH_CACHE@
-- is set in the environment.  Useful for benchmarking the slow path
-- and for test isolation.
searchPathCacheDisabled :: IO Bool
searchPathCacheDisabled = do
    m <- lookupEnv "IHC_NO_SEARCH_PATH_CACHE"
    pure (case m of
              Just v | not (null v) && v /= "0" -> True
              _                                 -> False)

-- | Separator line between the fingerprint section and the payload
-- section of the on-disk cache file.  Chosen so the file is readable
-- in a text editor when debugging.
cacheSectionSeparator :: ByteString
cacheSectionSeparator = BC.pack "===SEP===\n"

-- | Attempt to load the on-disk cache IFF its fingerprint matches
-- the current filesystem state.  Returns 'Nothing' on any failure
-- (missing file, bad format, fingerprint mismatch) so the caller
-- falls back to the slow path and rewrites the cache.
tryLoadOnDiskCache :: SearchPathFingerprint -> IO (Maybe [(FilePath, [FilePath])])
tryLoadOnDiskCache fp = do
    disabled <- searchPathCacheDisabled
    if disabled
        then pure Nothing
        else do
            path <- cacheFilePath
            r <- try (BS.readFile path) :: IO (Either SomeException ByteString)
            case r of
                Left _   -> pure Nothing
                Right bs ->
                    case BC.breakSubstring cacheSectionSeparator bs of
                        (fpEnc, rest)
                            | not (BC.null rest) ->
                                let payload = BC.drop (BC.length cacheSectionSeparator) rest
                                    expected = encodeFingerprint fp
                                in if fpEnc == expected
                                     then do
                                         traceCache "hit"
                                         pure (decodeResult payload)
                                     else do
                                         traceCache "fingerprint mismatch"
                                         pure Nothing
                        _ -> do
                            traceCache "malformed (no separator)"
                            pure Nothing

-- | Persist the computed search path to the on-disk cache.  Best
-- effort: any I/O error is swallowed (we log to stderr if
-- @IHC_TRACE@ is set, but otherwise stay silent).
writeOnDiskCache :: SearchPathFingerprint -> [(FilePath, [FilePath])] -> IO ()
writeOnDiskCache fp pairs = do
    disabled <- searchPathCacheDisabled
    if disabled
        then pure ()
        else do
            path <- cacheFilePath
            let payload = encodeFingerprint fp
                      <> cacheSectionSeparator
                      <> encodeResult pairs
            r <- try (do
                        createDirectoryIfMissing True (takeDirectory path)
                        BS.writeFile path payload)
                   :: IO (Either SomeException ())
            case r of
                Right () -> traceCache ("wrote (" <> show (BS.length payload) <> " bytes)")
                Left e   -> traceCache ("write failed: " <> show e)

-- | Emit a one-line trace message when @IHC_TRACE@ is enabled,
-- prefixed with @[ihc:cache]@ so cache events are easy to grep.
traceCache :: String -> IO ()
traceCache msg = do
    trace <- lookupEnv "IHC_TRACE"
    case trace of
        Just v | not (null v) && v /= "0" ->
            hPutStrLn stderr ("[ihc:cache] search-path: " <> msg)
        _ -> pure ()

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
--
-- Implementation: delegates to the cached
-- 'cachedPackageSearchPathWithIncludes' and strips the include-dirs,
-- so both entry points share one memo / one disk cache.
cachedPackageSearchPath :: IO [FilePath]
cachedPackageSearchPath = map fst <$> cachedPackageSearchPathWithIncludes

-- | Like 'cachedPackageSearchPath' but returns @(srcDir, includeDirs)@
-- pairs so callers can look up the @include-dirs@ for the package that
-- owns a given source file.
--
-- For each package directory we return one entry per @hs-source-dirs@
-- entry, all sharing the same @pkgIncludeDirs@.  When a package has no
-- @include-dirs@ the list is empty (no overhead for callers).
--
-- Priority order matches 'cachedPackageSearchPath':
-- nix-pinned → user cache → cabal tarball cache.
--
-- Caching strategy (see the "Cache-wide search path" section header
-- above for rationale):
--
--   1. In-process memo: the first call does the work, subsequent calls
--      reuse the result via 'searchPathMemoRef'.
--   2. On-disk cache at @~\/.cache\/ihc\/search-path.cache@ keyed by a
--      cheap mtime fingerprint of the source roots and their immediate
--      children.  Survives across ihc invocations.
cachedPackageSearchPathWithIncludes :: IO [(FilePath, [FilePath])]
cachedPackageSearchPathWithIncludes = do
    memo <- readIORef searchPathMemoRef
    case memo of
        Just pairs -> pure pairs
        Nothing    -> computeAndStore
  where
    computeAndStore = do
        -- Compute the fingerprint *before* touching the slow path so we
        -- can decide whether to reuse the on-disk cache.
        fp <- computeSearchPathFingerprint
        mDisk <- tryLoadOnDiskCache fp
        pairs <- case mDisk of
            Just cached -> pure cached
            Nothing     -> do
                fresh <- computeSearchPathFresh
                -- Force the list spine so subsequent readers never
                -- trigger the slow path lazily.
                _     <- evaluate (length fresh)
                writeOnDiskCache fp fresh
                pure fresh
        -- Install the memo.  We use atomicModifyIORef' so a racing
        -- second caller doesn't end up with a different list.  The
        -- race is harmless correctness-wise (both branches produce
        -- the same result); the atomic CAS just avoids redundant work.
        atomicModifyIORef' searchPathMemoRef $ \cur ->
            case cur of
                Just existing -> (Just existing, existing)
                Nothing       -> (Just pairs, pairs)

-- | The "slow path" used on cold cache: actually walk every source
-- directory and parse every .cabal file.  Kept as a separate function
-- so the caching wrapper above stays small and easy to read.
computeSearchPathFresh :: IO [(FilePath, [FilePath])]
computeSearchPathFresh = do
    home <- getHomeDirectory
    let userCache = home </> ".cache" </> "ihc" </> "sources"
    nixPairs   <- nixIncludePairs
    userPairs  <- enumerateWithIncludes userCache
    cabalPairs <- cabalTarballIncludePairs
    pure (nixPairs ++ userPairs ++ cabalPairs)
  where
    nixIncludePairs :: IO [(FilePath, [FilePath])]
    nixIncludePairs = do
        mDir <- lookupEnv "IHC_NIX_SOURCE_DIR"
        case mDir of
            Nothing  -> pure []
            Just dir -> enumerateWithIncludes dir

    enumerateWithIncludes :: FilePath -> IO [(FilePath, [FilePath])]
    enumerateWithIncludes sourcesDir = do
        exists <- doesDirectoryExist sourcesDir
        if not exists
            then pure []
            else do
                entries <- listDirectory sourcesDir
                -- Sort descending so the highest version of each package is
                -- found first (matches the ordering in cachedPackageSearchPath).
                let sortedEntries = sortBy (\a b -> compare (Down a) (Down b)) entries
                concat <$> mapM (pairsForEntry sourcesDir) sortedEntries

    pairsForEntry sourcesDir entry = do
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
                            Just info ->
                                pure [ (sd, pkgIncludeDirs info)
                                     | sd <- pkgSourceDirs info
                                     ]
                            Nothing -> fallbackPair pkgDir
                    Nothing -> fallbackPair pkgDir

    -- No .cabal to parse → no include-dirs known; emit an empty pair.
    fallbackPair pkgDir = do
        let srcDir = pkgDir </> "src"
        hasSrc <- doesDirectoryExist srcDir
        let sd = if hasSrc then srcDir else pkgDir
        pure [(sd, [])]

    cabalTarballIncludePairs :: IO [(FilePath, [FilePath])]
    cabalTarballIncludePairs = do
        home <- getHomeDirectory
        let cabalRoot = home </> ".cabal" </> "packages" </> "hackage.haskell.org"
        exists <- doesDirectoryExist cabalRoot
        if not exists
            then pure []
            else do
                pkgDirs <- listDirectory cabalRoot
                fmap concat $ mapM (pkgPairs cabalRoot) pkgDirs

    pkgPairs cabalRoot pkg = do
        let pkgRoot = cabalRoot </> pkg
        isDir <- doesDirectoryExist pkgRoot
        if not isDir
            then pure []
            else do
                verDirs <- listDirectory pkgRoot
                fmap concat $ mapM (verPairs pkgRoot pkg) verDirs

    verPairs pkgRoot pkg ver = do
        let extracted = pkgRoot </> ver </> (pkg <> "-" <> ver)
        hasDir <- doesDirectoryExist extracted
        if not hasDir
            then pure []
            else do
                mCabal <- findLocalCabalFile extracted
                case mCabal of
                    Just cabalPath -> do
                        mInfo <- parseCabalFile extracted cabalPath
                        case mInfo of
                            Just info ->
                                pure [ (sd, pkgIncludeDirs info)
                                     | sd <- pkgSourceDirs info
                                     ]
                            Nothing -> fallbackPair extracted
                    Nothing -> fallbackPair extracted

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
