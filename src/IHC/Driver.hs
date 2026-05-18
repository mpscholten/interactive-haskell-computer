-- | End-to-end pipeline (Phase 2): read source -> AST -> evaluate
-- @main@ -> if it's an IO action, run it. No JIT, no W^X dance, no
-- codesign needed at runtime.
--
-- Phase 2.7 addition: 'runFile' now tries to detect an enclosing
-- Cabal project and feed the scheduler a search path that spans
-- every transitive dependency's @hs-source-dirs@. On failure we
-- fall back to the Phase 2.5 single-file behaviour.
module IHC.Driver
    ( runFile
    , runSource
    , resolveSearchPathFor
    ) where

import Control.Exception (SomeException, catch, finally, try)
import Data.ByteString (ByteString)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import IHC.Builtins (reapSpawnedThreads)
import IHC.CabalProject
    ( CabalProjectError(..)
    , SearchEnv(..)
    , detectProjectRoot
    , resolve
    )
import IHC.Classes (legacyHooks)
import IHC.Diagnostics (warnStub)
import IHC.Eval (force)
import IHC.PackageStore (acquire, buildSearchEnv)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source
import IHC.Val (Val(..))

runFile :: FilePath -> IO Int
runFile path = do
    src <- readSourceFile path
    searchPath <- resolveSearchPathFor path
    runWithSearchPath searchPath src

-- | Figure out which directories the module loader should look in
-- when resolving @import@s for the given entry file.
--
-- If the entry lives inside a cabal project (some ancestor dir
-- contains @cabal.project@ or a @*.cabal@ file), we parse the
-- project's freeze file, @cabal get@ every transitive dep, and hand
-- the scheduler a search path that spans the local project plus
-- every pinned Hackage package's @hs-source-dirs@.
--
-- If anything goes wrong (no cabal project, missing freeze file,
-- unparseable cabal, cabal get failure) we log to stderr and fall
-- back to the Phase 2.5 single-file behaviour: just the entry
-- file's own directory. This keeps every existing test green.
resolveSearchPathFor :: FilePath -> IO [FilePath]
resolveSearchPathFor path = do
    let entryDir = takeDirectory path
        fallback = [entryDir]
    mRoot <- detectProjectRoot entryDir
    case mRoot of
        Nothing   -> pure fallback
        Just root -> do
            r <- try (resolve root)
                    :: IO (Either CabalProjectError [(ByteString, ByteString)])
            case r of
                Left err -> do
                    -- Common fallback: project has no freeze file (or the
                    -- cabal file failed to parse). We still drop to
                    -- single-file mode so that Phase 2.5 fixtures keep
                    -- working, but we log a one-line diagnostic so
                    -- downstream 'UnresolvedName' errors don't appear
                    -- out of nowhere. Silenceable via IHC_WARN_STUBS=0.
                    case err of
                        NoFreezeFile freezePath ->
                            warnStub
                                ("cabal.project.freeze not found at "
                                 <> freezePath
                                 <> "; single-file mode (entry dir: "
                                 <> entryDir <> ")")
                        CabalParseFailed cabalPath msg ->
                            warnStub
                                ("failed to parse " <> cabalPath
                                 <> " (" <> msg
                                 <> "); single-file mode (entry dir: "
                                 <> entryDir <> ")")
                    pure fallback
                Right deps -> do
                    acqR <- try (acquire deps) :: IO (Either SomeException ())
                    case acqR of
                        Left e ->
                            hPutStrLn stderr
                                ("ihc: cabal get failed (" <> show e
                                 <> "); continuing with whatever's cached")
                        Right () -> pure ()
                    envR <- try (buildSearchEnv root deps)
                              :: IO (Either SomeException SearchEnv)
                    case envR of
                        Left e -> do
                            hPutStrLn stderr
                                ("ihc: buildSearchEnv failed (" <> show e
                                 <> "); falling back to single-file mode")
                            pure fallback
                        Right env ->
                            -- Always keep the entry dir first in case
                            -- the project has no cabal library stanza.
                            pure (entryDir : seSearchPath env)

runSource :: Source -> IO Int
runSource = runWithSearchPath []

-- | Force @main@'s thunk; if it's a 'VIO' action, run the bind chain
-- to completion. The exit code falls out of:
--
-- * an explicit @exitWith ExitFailure n@ in user code      -> n
-- * 'ExitSuccess' / a successful run of any 'IO ()'         -> 0
-- * a residual 'VInt' (pure-value @main@ for old fixtures) -> that int
-- End-of-run thread reaping ('finally', so it covers the normal
-- return, the caught 'ExitCode', and any other exception): an
-- interpreted @main@ can leave background threads alive (warp accept
-- loop, System.TimeManager, async, bare forkIO).  'resetPerRunGlobals'
-- reaps the *prior* run's threads at the *next*
-- 'loadProgramFromSource', which bounds cross-fixture accumulation —
-- but the LAST run in a process has no next run, so its leaked threads
-- keep a capability busy-spinning and GHC's threaded RTS never
-- quiesces on @main@ return: the ~600-example test binary completed
-- the suite then HUNG at shutdown.  Reaping here makes every run
-- self-cleaning so the process can exit (and is the correct contract
-- for any embedder/REPL too).
runWithSearchPath :: [FilePath] -> Source -> IO Int
runWithSearchPath searchPath src =
    (runImpl `catch` \e -> case e of
        ExitSuccess   -> pure 0
        ExitFailure n -> pure n)
      `finally` reapSpawnedThreads
  where
    runImpl = do
        (_env, mainT) <- loadProgramFromSource searchPath src
        v             <- force legacyHooks mainT
        final         <- runIO v
        case final of
            VInt n -> pure (fromIntegral n)
            _      -> pure 0

-- | Recursively unwrap nested 'VIO's, executing each action in turn.
-- Non-IO values pass through unchanged so the driver can extract a
-- final 'VInt' for fixtures whose @main@ is a pure number.
runIO :: Val -> IO Val
runIO (VIO io) = io >>= runIO
runIO v        = pure v
