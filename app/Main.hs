module Main (main) where

import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import IHC (version)
import IHC.CabalProject (detectProjectRoot, resolve)
import IHC.Repl (runRepl)
import Daemon (runDaemonForeground, runViaDaemonOrLocal)

usage :: String
usage = unlines
    [ "ihc — Interactive Haskell Computer (Phase 0)"
    , ""
    , "USAGE:"
    , "    ihc --help           show this message"
    , "    ihc --version        show version"
    , "    ihc --check-cabal P  resolve cabal deps for file/dir P, print them"
    , "    ihc run FILE.hs      (Phase 1+) run a Haskell file"
    , "    ihc daemon           long-lived process: warm module + instance cache"
    , "    ihc repl             (later)   start the REPL"
    , ""
    , "ihc run talks to a daemon if one is up (or starts one) so leftover"
    , "probes reuse scanned library modules. IHC_NO_DAEMON=1 runs in-process."
    ]

main :: IO ()
main = getArgs >>= \case
    []               -> hPutStrLn stderr usage >> exitFailure
    ["--help"]       -> putStrLn usage
    ["-h"]           -> putStrLn usage
    ["--version"]    -> putStrLn version
    ["--check-cabal", path] -> do
        mRoot <- detectProjectRoot (takeDirectory path)
        case mRoot of
            Nothing -> do
                putStrLn ("no cabal project detected for " <> path)
                exitSuccess
            Just root -> do
                putStrLn ("project root: " <> root)
                deps <- resolve root
                putStrLn ("resolved " <> show (length deps) <> " dependencies:")
                mapM_ (\(n, v) -> putStrLn
                        ("  " <> BC.unpack n <> " " <> BC.unpack v)) deps
    ["run", path]    -> do
        n <- runViaDaemonOrLocal path
        -- Convention: an IO-action `main` returns 0 (unit). Suppress
        -- the trailing print in that case so `putStrLn "..."` programs
        -- look natural; pure-Int `main`s still print their value.
        if n == 0 then pure () else print n
    ("run":_)        -> do
        hPutStrLn stderr "usage: ihc run FILE.hs"
        exitFailure
    ["daemon"]       -> runDaemonForeground
    ("repl":_)       -> runRepl
    args             -> do
        hPutStrLn stderr ("ihc: unknown arguments: " <> unwords args)
        hPutStrLn stderr usage
        exitFailure
