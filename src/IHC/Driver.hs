-- | End-to-end pipeline (Phase 2): read source -> AST -> evaluate
-- @main@ -> if it's an IO action, run it. No JIT, no W^X dance, no
-- codesign needed at runtime.
module IHC.Driver
    ( runFile
    , runSource
    ) where

import Control.Exception (catch)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)

import IHC.Eval (force)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source
import IHC.Val (Val(..))

runFile :: FilePath -> IO Int
runFile path = do
    src <- readSourceFile path
    -- Phase 2.5: the entry file's directory becomes the first
    -- (and currently only) module-search-path entry. `import Foo`
    -- looks for `Foo.hs` here; `import Data.ByteString.Lazy` for
    -- `Data/ByteString/Lazy.hs`. Phase 2.7 will extend this with
    -- Cabal-resolved package source dirs.
    runWithSearchPath [takeDirectory path] src

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
