-- | End-to-end pipeline: read a source file, discover + compile
-- @main@ plus every binding it transitively references, finalize the
-- JIT page (W -> X + I-cache flush), call @main@, return its Int.
module IHC.Driver
    ( runFile
    , runSource
    ) where

import Control.Exception (bracket)

import IHC.CodeBuffer (callInt)
import IHC.Scheduler
import IHC.Source

runFile :: FilePath -> IO Int
runFile path = readSourceFile path >>= runSource

runSource :: Source -> IO Int
runSource src = bracket (newScheduler src) freeScheduler $ \sched -> do
    mainAddr <- compileRoot sched "main"
    callInt mainAddr
