-- | The standard environment that every program starts in.
--
-- Each builtin is a Haskell function returning @IO Val@, taking its
-- arguments as 'Thunk's so it can be lazy if it wants. Most are
-- strict in their numeric arguments (force first), since the
-- arithmetic operators need actual numbers.
--
-- These replace the Phase-1 'IHC.Stdlib' C-ABI shims. There is no
-- @foreign export@; the evaluator and the builtins are both Haskell
-- code in the same process, so calls are direct.
module IHC.Builtins
    ( builtinEnv
    , buildConEnv
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)

import IHC.AST  (Name)
import IHC.Eval (force)
import IHC.Scan (DataRegistry)
import IHC.Val

-- | Build the initial environment containing every well-known name.
builtinEnv :: IO Env
builtinEnv = do
    pairs <- mapM (\(n, mkV) -> do { v <- mkV; t <- newWHNFThunk v; pure (n, t) })
                  builtins
    pure (extendEnvMany pairs emptyEnv)

builtins :: [(Name, IO Val)]
builtins =
    -- Arithmetic
    [ ("+",        binOpInt (+))
    , ("-",        binOpInt (-))
    , ("*",        binOpInt (*))
    , ("mod",      binOpInt mod)
    , ("div",      binOpInt div)
    , ("negate",   unaryOpInt negate)
    , ("abs",      unaryOpInt abs)
    , ("signum",   unaryOpInt signum)
    , ("succ",     unaryOpInt (+1))
    , ("pred",     unaryOpInt (subtract 1))
    , ("min",      binOpInt min)
    , ("max",      binOpInt max)
    , ("gcd",      binOpInt gcd)
    -- Comparisons (return 0/1 for now; real Bool arrives in Phase 2.1)
    , ("==",       cmpInt (==))
    , ("/=",       cmpInt (/=))
    , ("<",        cmpInt (<))
    , ("<=",       cmpInt (<=))
    , (">",        cmpInt (>))
    , (">=",       cmpInt (>=))
    , ("even",     unaryOpInt (\n -> if even n then 1 else 0))
    , ("odd",      unaryOpInt (\n -> if odd  n then 1 else 0))
    , ("not",      unaryOpInt (\n -> if n == 0 then 1 else 0))
    -- Boolean (bitwise on 0/1 values until real Bool)
    , ("&&",       binOpInt (\a b -> if a /= 0 && b /= 0 then 1 else 0))
    , ("||",       binOpInt (\a b -> if a /= 0 || b /= 0 then 1 else 0))
    -- Strings
    , ("++",       strConcat)
    , ("show",     showInt)
    , ("length",   lengthStr)
    -- IO
    , ("putStrLn", putStrLnB)
    , ("print",    printB)
    , ("putChar",  putCharB)
    , ("getLine",  getLineB)
    -- Misc
    , ("error",    errorB)
    ]

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

binOpInt :: (Int64 -> Int64 -> Int64) -> IO Val
binOpInt op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (op x y))
        _ -> error ("binOp: non-Int args: "
                    <> showValForDebug av <> ", " <> showValForDebug bv)

unaryOpInt :: (Int64 -> Int64) -> IO Val
unaryOpInt op = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt x -> pure (VInt (op x))
        _ -> error ("unaryOp: non-Int arg: " <> showValForDebug av)

cmpInt :: (Int64 -> Int64 -> Bool) -> IO Val
cmpInt op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (if op x y then 1 else 0))
        _ -> error ("cmp: non-Int args: "
                    <> showValForDebug av <> ", " <> showValForDebug bv)

--------------------------------------------------------------------------------
-- Strings (raw ByteString for now; replaced by [Char] in Phase 2.2)
--------------------------------------------------------------------------------

strConcat :: IO Val
strConcat = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VStr x, VStr y) -> pure (VStr (x <> y))
        _ -> error ("(++): non-string args: "
                    <> showValForDebug av <> ", " <> showValForDebug bv)

showInt :: IO Val
showInt = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VStr (BC.pack (show n)))
        VStr s -> pure (VStr (BC.pack (show (BC.unpack s))))
        _ -> error ("show: unsupported value: " <> showValForDebug av)

lengthStr :: IO Val
lengthStr = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VStr s -> pure (VInt (fromIntegral (BC.length s)))
        _ -> error ("length: not a string: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

putStrLnB :: IO Val
putStrLnB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VStr s -> do
            BC.putStrLn s
            hFlush stdout
            pure VUnit
        _ -> error ("putStrLn: not a string: " <> showValForDebug av)

printB :: IO Val
printB = pure $ VFun $ \a -> do
    av <- force a
    putStrLn (case av of VInt n -> show n; v -> showValForDebug v)
    hFlush stdout
    pure VUnit

putCharB :: IO Val
putCharB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt c -> do { putChar (toEnum (fromIntegral c)); hFlush stdout; pure VUnit }
        _ -> error ("putChar: not an Int: " <> showValForDebug av)

getLineB :: IO Val
getLineB = pure $ VFun $ \_ -> do
    -- Note: takes a dummy arg in Phase-1's convention. We'll fix when
    -- proper IO actions arrive in Phase 2.4.
    s <- getLine
    pure (VStr (BC.pack s))

errorB :: IO Val
errorB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VStr s -> error ("ihc: " <> BC.unpack s)
        _      -> error ("ihc: error called with non-string")

--------------------------------------------------------------------------------
-- User-defined constructors
--------------------------------------------------------------------------------

-- | Build an environment binding every user-declared constructor to a
-- function (or value, for nullary) that produces a 'VCon'. Arity-0
-- constructors become WHNF thunks holding @VCon name []@; arity-n
-- constructors become a curried chain of @VFun@s that accumulate the
-- argument thunks, then produce @VCon name args@ at saturation.
--
-- The argument thunks are stored unevaluated — a 'VCon' field is lazy.
buildConEnv :: DataRegistry -> IO Env
buildConEnv reg = do
    pairs <- mapM mkBinding (Map.toList reg)
    pure (extendEnvMany pairs emptyEnv)
  where
    mkBinding (name, arity) = do
        v <- mkCon name arity
        t <- newWHNFThunk v
        pure (name, t)

    -- arity 0: the VCon itself (wrapped later in a thunk).
    -- arity n: a chain of n VFuns that accumulate thunks in reverse, then
    --          return a saturated VCon.
    mkCon :: Name -> Int -> IO Val
    mkCon name 0 = pure (VCon name [])
    mkCon name n = pure (buildLam name n [])

    buildLam :: Name -> Int -> [Thunk] -> Val
    buildLam name 0    acc = VCon name (reverse acc)
    buildLam name left acc = VFun $ \t ->
        pure (buildLam name (left - 1) (t : acc))
