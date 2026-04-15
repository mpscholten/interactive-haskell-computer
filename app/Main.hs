module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import IHC (version)
import IHC.Driver (runFile)
import IHC.Jit (codesignCheck)

usage :: String
usage = unlines
    [ "ihc — Interactive Haskell Computer (Phase 0)"
    , ""
    , "USAGE:"
    , "    ihc --help           show this message"
    , "    ihc --version        show version"
    , "    ihc --check-jit      verify MAP_JIT pages work on this binary"
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
    ["run", path]    -> do
        n <- runFile path
        print n
    ("run":_)        -> do
        hPutStrLn stderr "usage: ihc run FILE.hs"
        exitFailure
    ("repl":_)       -> do
        hPutStrLn stderr "ihc repl: not implemented yet."
        exitFailure
    args             -> do
        hPutStrLn stderr ("ihc: unknown arguments: " <> unwords args)
        hPutStrLn stderr usage
        exitFailure
