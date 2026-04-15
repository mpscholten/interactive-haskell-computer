-- | End-to-end pipeline (Phase 2): read source -> AST -> evaluate
-- @main@. No JIT, no W^X dance, no codesign needed at runtime.
module IHC.Driver
    ( runFile
    , runSource
    ) where

import IHC.Eval (force)
import IHC.Scheduler (loadProgram)
import IHC.Source
import IHC.Val (Val(..))

runFile :: FilePath -> IO Int
runFile path = readSourceFile path >>= runSource

-- | Force @main@'s thunk. If the result is an Int, return it
-- (preserves the Phase-1 contract for fixtures that compare numeric
-- output). 'VUnit' (the IO () result of @putStrLn@-style programs)
-- becomes 0.
runSource :: Source -> IO Int
runSource src = do
    (_env, mainT) <- loadProgram src
    v <- force mainT
    case v of
        VInt n -> pure (fromIntegral n)
        VUnit  -> pure 0
        _      -> pure 0
