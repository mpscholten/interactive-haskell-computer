module Main (main) where

import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import IHC (version)
import IHC.CabalProject (detectProjectRoot, resolve)
import IHC.Driver (runFile)
import IHC.Jit (codesignCheck)
import IHC.Repl (runRepl)

usage :: String
usage = unlines
    [ "ihc — Interactive Haskell Computer (Phase 0)"
    , ""
    , "USAGE:"
    , "    ihc --help           show this message"
    , "    ihc --version        show version"
    , "    ihc --check-jit      verify MAP_JIT pages work on this binary"
    , "    ihc --check-cabal P  resolve cabal deps for file/dir P, print them"
    , "    ihc run FILE.hs      (Phase 1+) run a Haskell file"
    , "    ihc repl             (later)   start the REPL"
    ]

main :: IO ()
main = getArgs >>= \case
    []               -> hPutStrLn stderr usage >> exitFailure
    ["--help"]       -> putStrLn usage
    ["-h"]           -> putStrLn usage
    ["--version"]    -> putStrLn version
    ["--check-jit"]  -> do
        rc <- codesignCheck
        if rc == 0
            then putStrLn "JIT check OK"
            else do
                hPutStrLn stderr ("JIT check failed, errno=" <> show rc)
                hPutStrLn stderr "Did you forget to ad-hoc codesign with com.apple.security.cs.allow-jit?"
                exitFailure
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
        n <- runFile path
        -- Convention: an IO-action `main` returns 0 (unit). Suppress
        -- the trailing print in that case so `putStrLn "..."` programs
        -- look natural; pure-Int `main`s still print their value.
        if n == 0 then pure () else print n
    ("run":_)        -> do
        hPutStrLn stderr "usage: ihc run FILE.hs"
        exitFailure
    ("repl":_)       -> runRepl
    args             -> do
        hPutStrLn stderr ("ihc: unknown arguments: " <> unwords args)
        hPutStrLn stderr usage
        exitFailure
