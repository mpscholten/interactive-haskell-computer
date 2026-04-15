-- | End-to-end Phase 1.0 pipeline: read a source file, find @main@ on
-- demand, emit aarch64 into a JIT page, execute it, return the Int it
-- produces.
module IHC.Driver
    ( runFile
    , runSource
    ) where

import Control.Exception (throwIO)

import IHC.CodeBuffer
import IHC.Lexer
import IHC.Parser
import IHC.Scan
import IHC.Source

-- | Convenience: read from disk and run.
runFile :: FilePath -> IO Int
runFile path = do
    src <- readSourceFile path
    runSource src

runSource :: Source -> IO Int
runSource src = do
    known <- emptyKnownSymbols
    mspan <- findBinding src known "main"
    case mspan of
        Nothing   -> throwIO (ParseError ("no `main` binding in " <> srcName src))
        Just body -> do
            cb <- newCodeBuffer 4096
            entry <- withWritable cb $ \cb' -> do
                p <- currentAddr cb'
                parseBody src body cb'
                pure p
            result <- callInt entry
            freeCodeBuffer cb
            pure result
