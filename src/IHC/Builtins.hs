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
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)

import IHC.AST  (Name)
import IHC.Eval (force)
import IHC.Scan (DataRegistry)
import IHC.Val

-- | Build the initial environment containing every well-known name.
--
-- This also registers the built-in list constructors @[]@ and @(:)@
-- — lists are Phase 2.2's first taste of a built-in ADT. We treat
-- them exactly like user-declared constructors from 'buildConEnv':
-- arity-0 nil is a bare @VCon "[]" []@; arity-2 cons is a curried
-- function that accumulates two thunks and returns @VCon ":" [h, t]@.
builtinEnv :: IO Env
builtinEnv = do
    pairs <- mapM (\(n, mkV) -> do { v <- mkV; t <- newWHNFThunk v; pure (n, t) })
                  builtins
    -- Built-in list constructors.
    nilT  <- newWHNFThunk (VCon "[]" [])
    consT <- newWHNFThunk consV
    let listCtors = [("[]", nilT), (":", consT)]
    -- Guard sugar: `| otherwise = ...` desugars to an `if otherwise`.
    -- The EIf evaluator treats any non-zero VInt as truthy, so VInt 1
    -- is the right representation while we still carry 0/1 Bools.
    otherT <- newWHNFThunk (VInt 1)
    trueT  <- newWHNFThunk (VInt 1)
    falseT <- newWHNFThunk (VInt 0)
    let boolish = [("otherwise", otherT), ("True", trueT), ("False", falseT)]
    pure (extendEnvMany (pairs ++ listCtors ++ boolish) emptyEnv)
  where
    consV = VFun $ \h -> pure $ VFun $ \t -> pure (VCon ":" [h, t])

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
    -- Strings / lists (strings are [Char] from Phase 2.2 onward)
    , ("++",       listConcat)
    , ("show",     showB)
    , ("length",   lengthB)
    -- IO
    , ("putStrLn", putStrLnB)
    , ("putStr",   putStrB)
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
-- Lists as user-facing strings / generic containers
--
-- In Phase 2.2 a string literal desugars to a cons-chain of VChar, so
-- "Hi" is @VCon ":" [VChar 'H', VCon ":" [VChar 'i', VCon "[]" []]]@.
-- The built-ins below walk such chains explicitly. We keep a VStr
-- fallback so the transition is gradual — some legacy code paths may
-- still produce VStr, and the list builtins accept it.
--------------------------------------------------------------------------------

-- | Force a cons-list all the way to @[]@ and collect its elements as
-- WHNF 'Val's. Each element is forced before being returned.
forceList :: Val -> IO [Val]
forceList (VCon "[]" _) = pure []
forceList (VCon ":"  [h, t]) = do
    hv <- force h
    tv <- force t
    rest <- forceList tv
    pure (hv : rest)
forceList other =
    error ("forceList: not a list: " <> showValForDebug other)

-- | Force a @[Char]@ value down to a host 'String'. Accepts either a
-- cons-chain of VChar or a transitional VStr.
valToString :: Val -> IO String
valToString (VStr s) = pure (BC.unpack s)
valToString v = do
    xs <- forceList v
    mapM extractChar xs
  where
    extractChar (VChar c) = pure c
    extractChar (VInt  n) = pure (toEnum (fromIntegral n))  -- tolerate mixed use
    extractChar other =
        error ("expected Char in [Char]: " <> showValForDebug other)

-- | Is this WHNF value a @[Char]@? Used to decide whether to render a
-- list as a double-quoted string or with the @[a,b,c]@ syntax.
isCharList :: Val -> IO Bool
isCharList (VStr _) = pure True
isCharList (VCon "[]" _) = pure True
isCharList (VCon ":"  [h, _]) = do
    hv <- force h
    case hv of
        VChar _ -> pure True
        _       -> pure False
isCharList _ = pure False

-- | Render any supported WHNF value as the Haskell @show@ of it.
showVal :: Val -> IO String
showVal (VInt n)    = pure (show n)
showVal (VChar c)   = pure (show c)
showVal VUnit       = pure "()"
showVal v@(VCon "[]" _) = pure "[]"
showVal v@(VCon ":" _) = do
    cl <- isCharList v
    if cl
        then do s <- valToString v; pure (show s)
        else do
            xs <- forceList v
            parts <- mapM showVal xs
            pure ("[" <> intercalate "," parts <> "]")
showVal (VStr s)    = pure (show (BC.unpack s))
showVal (VCon name thunks) = do
    parts <- mapM (\t -> do v <- force t; showVal v) thunks
    case parts of
        [] -> pure (BC.unpack name)
        _  -> pure (BC.unpack name <> " " <> unwords parts)
showVal (VFun _)    = pure "<function>"

-- | @xs ++ ys@ as a list concat. For VStr+VStr the fast path uses
-- ByteString concat. For cons-lists we walk the spine of @xs@,
-- forcing each cons (but NOT the head elements), and reuse the
-- original @ys@ thunk as the final tail — so elements stay lazy.
listConcat :: IO Val
listConcat = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    case av of
        VStr x -> do
            bv <- force b
            case bv of
                VStr y -> pure (VStr (x <> y))
                _ -> do
                    -- VStr ++ [Char]: promote and chain.
                    listV <- stringToListValIO (BC.unpack x)
                    appendVal listV b
        _ -> appendVal av b
  where
    appendVal :: Val -> Thunk -> IO Val
    appendVal (VCon "[]" _)     bT = force bT
    appendVal (VCon ":" [h, t]) bT = do
        -- Force the tail's spine lazily on demand: we create a WHNF
        -- thunk whose value is the recursive append on the next cons.
        tv    <- force t
        rv    <- appendVal tv bT
        restT <- newWHNFThunk rv
        pure (VCon ":" [h, restT])
    appendVal other _ =
        error ("(++): not a list: " <> showValForDebug other)

-- | Polymorphic-ish @show@: Int, Char, [Char], or generic list / constructor.
showB :: IO Val
showB = pure $ VFun $ \a -> do
    av <- force a
    s  <- showVal av
    stringToListValIO s

-- | Build a cons-chain of VChar from a host 'String' (in IO — needs
-- to allocate thunks).
stringToListValIO :: String -> IO Val
stringToListValIO []     = pure (VCon "[]" [])
stringToListValIO (c:cs) = do
    hT   <- newWHNFThunk (VChar c)
    restV <- stringToListValIO cs
    tT   <- newWHNFThunk restV
    pure (VCon ":" [hT, tT])

-- | Generic @length@ — walks the spine of a list, forcing each cons
-- but not the elements.
lengthB :: IO Val
lengthB = pure $ VFun $ \a -> do
    av <- force a
    n  <- go av 0
    pure (VInt n)
  where
    go (VStr s) !acc = pure (acc + fromIntegral (BC.length s))
    go (VCon "[]" _) !acc = pure acc
    go (VCon ":" [_, t]) !acc = do
        tv <- force t
        go tv (acc + 1)
    go other _ = error ("length: not a list: " <> showValForDebug other)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- | Write a @[Char]@ plus newline. Accepts either a cons-chain of
-- VChar or a transitional VStr.
putStrLnB :: IO Val
putStrLnB = pure $ VFun $ \a -> do
    av <- force a
    s  <- valToString av
    putStrLn s
    hFlush stdout
    pure VUnit

putStrB :: IO Val
putStrB = pure $ VFun $ \a -> do
    av <- force a
    s  <- valToString av
    putStr s
    hFlush stdout
    pure VUnit

printB :: IO Val
printB = pure $ VFun $ \a -> do
    av <- force a
    s  <- showVal av
    putStrLn s
    hFlush stdout
    pure VUnit

putCharB :: IO Val
putCharB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VChar c -> do { putChar c; hFlush stdout; pure VUnit }
        VInt c  -> do { putChar (toEnum (fromIntegral c)); hFlush stdout; pure VUnit }
        _ -> error ("putChar: not a Char: " <> showValForDebug av)

getLineB :: IO Val
getLineB = pure $ VFun $ \_ -> do
    -- Note: takes a dummy arg in Phase-1's convention. We'll fix when
    -- proper IO actions arrive in Phase 2.4.
    s <- getLine
    stringToListValIO s

errorB :: IO Val
errorB = pure $ VFun $ \a -> do
    av <- force a
    s  <- valToString av
    error ("ihc: " <> s)

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
