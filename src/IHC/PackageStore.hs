-- | Phase 2.7 — per-package source acquisition + search-env
-- materialisation.
--
-- Given a resolved list of @(package, version)@ pairs, this module
--
--   1. makes sure the source tree for each one is on disk under
--      @~/.cache/ihc/sources/<pkg>-<ver>/@ (shelling out to
--      @cabal get@ for any cache miss), then
--   2. reads each package's @.cabal@ file to extract its
--      @hs-source-dirs@, and
--   3. assembles a 'SearchEnv' — a flat list of source directories in
--      priority order plus a back-reference table from each dir to
--      its owning 'PackageInfo'.
--
-- The result feeds straight into Phase 2.5's
-- @loadProgramFromSource :: [FilePath] -> Source -> IO ...@.
module IHC.PackageStore
    ( -- * Cache paths
      cacheRoot
    , cacheRootOverride
    , packageSourceDir
      -- * Acquisition
    , acquire
    , acquireWithRoot
      -- * Search-env assembly
    , buildSearchEnv
    , buildSearchEnvWithRoot
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getHomeDirectory
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (proc, readCreateProcessWithExitCode, CreateProcess(..))

import IHC.CabalProject

--------------------------------------------------------------------------------
-- Cache paths
--------------------------------------------------------------------------------

-- | @~/.cache/ihc/sources@ — where we stash @cabal get@ output so
-- subsequent runs don't re-download.
cacheRoot :: IO FilePath
cacheRoot = do
    home <- getHomeDirectory
    pure (home </> ".cache" </> "ihc" </> "sources")

-- | Allow tests (or a future @--source-cache@ flag) to point at a
-- private cache instead of the user's home dir.
cacheRootOverride :: Maybe FilePath -> IO FilePath
cacheRootOverride (Just p) = pure p
cacheRootOverride Nothing  = cacheRoot

-- | @\<root\>/\<pkg\>-\<ver\>@ — the directory @cabal get@ unpacks
-- into.
packageSourceDir :: FilePath -> ByteString -> ByteString -> FilePath
packageSourceDir root name version =
    root </> (BC.unpack name <> "-" <> BC.unpack version)

--------------------------------------------------------------------------------
-- Acquisition
--------------------------------------------------------------------------------

-- | Ensure every @(pkg, ver)@ in the input list has its source
-- unpacked under the default cache root. Cache hits (directory
-- already exists) are skipped. Misses shell out to
-- @cabal get \<pkg\>-\<ver\> -d \<cache-root\>@.
--
-- Concurrency: up to 8 @cabal get@ processes run simultaneously.
-- Implemented with a simple 'MVar' semaphore rather than pulling in
-- @async@ (which isn't in the boot library set).
acquire :: [(ByteString, ByteString)] -> IO ()
acquire pkgs = do
    root <- cacheRoot
    acquireWithRoot root pkgs

-- | Like 'acquire' but with an explicit cache root. Used by tests.
acquireWithRoot :: FilePath -> [(ByteString, ByteString)] -> IO ()
acquireWithRoot root pkgs = do
    createDirectoryIfMissing True root
    -- Filter out cache hits first so we only parallelise the actual
    -- network-bound downloads.
    misses <- fmap catMaybes $ forM pkgs $ \(n, v) -> do
        hit <- doesDirectoryExist (packageSourceDir root n v)
        pure (if hit then Nothing else Just (n, v))

    when (not (null misses)) $
        hPutStrLn stderr
            ("ihc: fetching " <> show (length misses) <> " package(s) via cabal get")

    sem <- newQSemN 8
    done <- newEmptyMVar :: IO (MVar ())
    let total = length misses
    forM_ misses $ \(n, v) -> do
        waitQSemN sem 1
        _ <- forkIO $ do
            r <- try (cabalGet root n v) :: IO (Either SomeException ())
            case r of
                Left e  -> hPutStrLn stderr
                    ("ihc: cabal get " <> BC.unpack n <> "-" <> BC.unpack v
                     <> " failed: " <> show e)
                Right () -> pure ()
            signalQSemN sem 1
            putMVar done ()
        pure ()
    -- Wait for all forks to finish.
    forM_ [1 .. total] $ \_ -> takeMVar done

-- | Bare-bones counting semaphore built on an 'MVar' — enough for the
-- simple concurrency pattern above.
data QSemN = QSemN !(MVar Int)

newQSemN :: Int -> IO QSemN
newQSemN n = QSemN <$> newMVar n

waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN m) k = do
    -- Busy-wait until at least k tokens available. Not pretty, but
    -- "fine" because k == 1 and contention is low (<200 packages).
    let loop = do
            v <- takeMVar m
            if v >= k
                then putMVar m (v - k)
                else do
                    putMVar m v
                    -- Yield and retry. In practice the first taker
                    -- releases within milliseconds.
                    loop
    loop

signalQSemN :: QSemN -> Int -> IO ()
signalQSemN (QSemN m) k = do
    v <- takeMVar m
    putMVar m (v + k)

cabalGet :: FilePath -> ByteString -> ByteString -> IO ()
cabalGet root name version = do
    let pkg = BC.unpack name <> "-" <> BC.unpack version
        cp  = (proc "cabal" ["get", pkg, "-d", root]) { cwd = Nothing }
    (exit, _out, err) <- readCreateProcessWithExitCode cp ""
    case exit of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            hPutStrLn stderr ("ihc: cabal get " <> pkg <> " stderr: " <> err)

--------------------------------------------------------------------------------
-- Search env
--------------------------------------------------------------------------------

-- | Assemble a 'SearchEnv' from the project root plus a list of
-- resolved @(pkg, ver)@ pairs. For each dependency we:
--
--   1. locate its unpacked source dir under the cache,
--   2. parse its @.cabal@ file to get its @hs-source-dirs@,
--   3. append those dirs to the flat search path, and
--   4. record the reverse mapping in 'sePackageTable'.
--
-- The local project's own @.cabal@ file is parsed first so that
-- user-defined modules always shadow Hackage packages.
buildSearchEnv :: FilePath -> [(ByteString, ByteString)] -> IO SearchEnv
buildSearchEnv projectRoot deps = do
    root <- cacheRoot
    buildSearchEnvWithRoot root projectRoot deps

buildSearchEnvWithRoot
    :: FilePath                        -- ^ cache root
    -> FilePath                        -- ^ project root
    -> [(ByteString, ByteString)]      -- ^ resolved deps
    -> IO SearchEnv
buildSearchEnvWithRoot root projectRoot deps = do
    -- Local package.
    localInfos <- do
        mLocal <- findLocalCabalFile projectRoot
        case mLocal of
            Nothing -> pure []
            Just cp -> do
                mPi <- parseCabalFile projectRoot cp
                case mPi of
                    Just pi' -> pure [pi']
                    Nothing  -> do
                        hPutStrLn stderr
                            ("ihc: failed to parse local " <> cp <> " — skipping")
                        pure []

    -- Dependencies.
    depInfos <- fmap catMaybes $ forM deps $ \(n, v) -> do
        let dir = packageSourceDir root n v
        exists <- doesDirectoryExist dir
        if not exists
            then do
                hPutStrLn stderr
                    ("ihc: no source dir for " <> BC.unpack n
                     <> "-" <> BC.unpack v <> " — skipping")
                pure Nothing
            else do
                mCabal <- findLocalCabalFile dir
                case mCabal of
                    Nothing -> do
                        hPutStrLn stderr
                            ("ihc: no .cabal in " <> dir <> " — skipping")
                        pure Nothing
                    Just cp -> parseCabalFile dir cp

    let allInfos  = localInfos ++ depInfos
        paths     = concatMap pkgSourceDirs allInfos
        pkgTable  = Map.fromList
            [ (d, info)
            | info <- allInfos
            , d    <- pkgSourceDirs info
            ]
    pure SearchEnv
        { seSearchPath   = paths
        , sePackageTable = pkgTable
        }
