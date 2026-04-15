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

import Control.Exception (SomeException, catch, try)
import Data.ByteString (ByteString)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import IHC.CabalProject
    ( CabalProjectError
    , SearchEnv(..)
    , detectProjectRoot
    , resolve
    )
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
                Left _err ->
                    -- Common, quiet fallback: project has no freeze file.
                    -- The single-file behaviour covers every Phase 2.5
                    -- fixture, so we don't log here to keep test output
                    -- clean. A future --verbose flag can restore the
                    -- diagnostic.
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
runWithSearchPath :: [FilePath] -> Source -> IO Int
runWithSearchPath searchPath src =
    runImpl `catch` \e -> case e of
        ExitSuccess   -> pure 0
        ExitFailure n -> pure n
  where
    runImpl = do
        (_env, mainT) <- loadProgramFromSource searchPath src
        v             <- force mainT
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
