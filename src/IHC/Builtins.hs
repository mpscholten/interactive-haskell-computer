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

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode(..), exitWith)
import System.IO
    ( BufferMode(..)
    , Handle
    , IOMode(..)
    , hClose
    , hFlush
    , hGetLine
    , hPutStr
    , hPutStrLn
    , hSetBuffering
    , openFile
    , stderr
    , stdin
    , stdout
    )

import IHC.AST  (Name)
import IHC.Eval (apply, force)
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
    -- IOMode/BufferMode ctors: arity-0 data constructors surfaced so
    -- that primops like `openFile path ReadMode` can pattern match.
    ioModes <- mapM mkCon0
        [ "ReadMode", "WriteMode", "AppendMode", "ReadWriteMode"
        , "NoBuffering", "LineBuffering", "BlockBuffering"
        ]
    -- Standard handles.
    stdinT  <- newWHNFThunk (VPrimObj (PrimHandle stdin))
    stdoutT <- newWHNFThunk (VPrimObj (PrimHandle stdout))
    stderrT <- newWHNFThunk (VPrimObj (PrimHandle stderr))
    let handles = [("stdin", stdinT), ("stdout", stdoutT), ("stderr", stderrT)]
    pure (extendEnvMany (pairs ++ listCtors ++ boolish ++ ioModes ++ handles)
                        emptyEnv)
  where
    consV = VFun $ \h -> pure $ VFun $ \t -> pure (VCon ":" [h, t])
    mkCon0 name = do
        t <- newWHNFThunk (VCon name [])
        pure (name, t)

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
    -- Monad core (plain names so Phase 2.3 class dispatch can overlay)
    , (">>=",      bindB)
    , (">>",       seqIOB)
    , ("return",   returnB)
    , ("pure",     returnB)
    , ("fmap",     fmapB)
    , ("<*>",      apB)
    , ("join",     joinB)
    -- IORef
    , ("newIORef",    newIORefB)
    , ("readIORef",   readIORefB)
    , ("writeIORef",  writeIORefB)
    , ("modifyIORef", modifyIORefB)
    , ("modifyIORef'",modifyIORefB)             -- same, no laziness diff here
    -- File IO
    , ("openFile",    openFileB)
    , ("hClose",      hCloseB)
    , ("hPutStr",     hPutStrB)
    , ("hPutStrLn",   hPutStrLnB)
    , ("hGetLine",    hGetLineB)
    , ("hFlush",      hFlushB)
    , ("hSetBuffering", hSetBufferingB)
    -- Control flow
    , ("seq",         seqB)
    , ("$!",          dollarBangB)
    , ("error",       errorB)
    , ("undefined",   undefinedB)
    , ("exitWith",    exitWithB)
    , ("exitSuccess", exitSuccessB)
    -- Char / numeric conversions
    , ("ord",         ordB)
    , ("chr",         chrB)
    , ("fromIntegral", fromIntegralB)
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
showVal (VIO _)     = pure "<IO>"
showVal (VPrimObj (PrimIORef  _)) = pure "<IORef>"
showVal (VPrimObj (PrimHandle _)) = pure "<Handle>"

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
--
-- Returns @VIO action@ (Phase 2.4): the host IO is delayed until the
-- driver (or a do-block binding) actually runs the action.
putStrLnB :: IO Val
putStrLnB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    s  <- valToString av
    putStrLn s
    hFlush stdout
    pure VUnit

putStrB :: IO Val
putStrB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    s  <- valToString av
    putStr s
    hFlush stdout
    pure VUnit

printB :: IO Val
printB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    s  <- showVal av
    putStrLn s
    hFlush stdout
    pure VUnit

putCharB :: IO Val
putCharB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    case av of
        VChar c -> do { putChar c; hFlush stdout; pure VUnit }
        VInt c  -> do { putChar (toEnum (fromIntegral c)); hFlush stdout; pure VUnit }
        _ -> error ("putChar: not a Char: " <> showValForDebug av)

-- | 'getLine' — zero-arity IO action. We register the VIO directly
-- (no dummy-thunk wrapper like Phase 2.2/3). Reading from the env
-- thus immediately yields the action; binding it in a do-block runs it.
getLineB :: IO Val
getLineB = pure $ VIO $ do
    s <- getLine
    stringToListValIO s

errorB :: IO Val
errorB = pure $ VFun $ \a -> do
    av <- force a
    s  <- valToString av
    error ("ihc: " <> s)

undefinedB :: IO Val
undefinedB = pure (VIO (error "Prelude.undefined"))

--------------------------------------------------------------------------------
-- Monad core. Every builtin here is a plain global binding so Phase
-- 2.3 class-dispatch can later overlay it with dictionary-threaded
-- versions. The only monad we actually handle here is IO — 'VIO'.
--------------------------------------------------------------------------------

-- | @return x = VIO (pure x)@. The @x@ thunk is not forced until the
-- receiver runs the action (preserving Haskell laziness).
returnB :: IO Val
returnB = pure $ VFun $ \a -> pure (VIO (force a))

-- | @m >>= k@. Left side must evaluate to 'VIO'; run it, force the
-- right side to 'VFun', apply to the result (via a WHNF thunk), then
-- force the outer 'VIO' and run it.
bindB :: IO Val
bindB = pure $ VFun $ \ma -> pure $ VFun $ \kt -> pure $ VIO $ do
    mv <- force ma
    v  <- runIOVal mv
    kv <- force kt
    vT <- newWHNFThunk v
    r  <- apply kv vT
    runIOVal r

-- | @m >> n@ = run m (discarding result), then run n.
seqIOB :: IO Val
seqIOB = pure $ VFun $ \ma -> pure $ VFun $ \mb -> pure $ VIO $ do
    mv <- force ma
    _  <- runIOVal mv
    nv <- force mb
    runIOVal nv

-- | @fmap f m = do { v <- m; return (f v) }@. Only works for IO in 2.4.
fmapB :: IO Val
fmapB = pure $ VFun $ \ft -> pure $ VFun $ \mt -> pure $ VIO $ do
    mv <- force mt
    v  <- runIOVal mv
    fv <- force ft
    vT <- newWHNFThunk v
    apply fv vT

-- | @f <*> m = do { fun <- f; v <- m; return (fun v) }@.
apB :: IO Val
apB = pure $ VFun $ \ft -> pure $ VFun $ \mt -> pure $ VIO $ do
    fv <- force ft
    f1 <- runIOVal fv
    mv <- force mt
    v  <- runIOVal mv
    vT <- newWHNFThunk v
    apply f1 vT

-- | @join mm = do { m <- mm; m }@.
joinB :: IO Val
joinB = pure $ VFun $ \mmt -> pure $ VIO $ do
    mv <- force mmt
    inner <- runIOVal mv
    runIOVal inner

-- | Run a VIO (or re-run nested VIOs) until a non-VIO value is
-- reached. Mirrors the helper in 'IHC.Eval.evalDo'.
runIOVal :: Val -> IO Val
runIOVal (VIO io) = io >>= runIOVal
runIOVal v        = pure v

--------------------------------------------------------------------------------
-- IORef primops. Each returns 'VIO' — construction, read, and write
-- are all IO actions.
--------------------------------------------------------------------------------

newIORefB :: IO Val
newIORefB = pure $ VFun $ \a -> pure $ VIO $ do
    v  <- force a
    rf <- newIORef v
    pure (VPrimObj (PrimIORef rf))

readIORefB :: IO Val
readIORefB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    case av of
        VPrimObj (PrimIORef rf) -> readIORef rf
        _ -> error ("readIORef: not an IORef: " <> showValForDebug av)

writeIORefB :: IO Val
writeIORefB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force a
    case av of
        VPrimObj (PrimIORef rf) -> do
            bv <- force b
            writeIORef rf bv
            pure VUnit
        _ -> error ("writeIORef: not an IORef: " <> showValForDebug av)

-- | @modifyIORef ref f@. We force f then apply it to a thunk holding
-- the current ref contents. Works for both the lazy and strict forms
-- (Phase 2.4 does not differentiate beyond that).
modifyIORefB :: IO Val
modifyIORefB = pure $ VFun $ \a -> pure $ VFun $ \f -> pure $ VIO $ do
    av <- force a
    case av of
        VPrimObj (PrimIORef rf) -> do
            fv <- force f
            cur <- readIORef rf
            curT <- newWHNFThunk cur
            new <- apply fv curT
            writeIORef rf new
            pure VUnit
        _ -> error ("modifyIORef: not an IORef: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- File IO primops.
--------------------------------------------------------------------------------

requireHandle :: String -> Val -> IO Handle
requireHandle fnName v = case v of
    VPrimObj (PrimHandle h) -> pure h
    _ -> error (fnName <> ": not a Handle: " <> showValForDebug v)

ioModeFromVal :: Val -> IOMode
ioModeFromVal (VCon "ReadMode"      _) = ReadMode
ioModeFromVal (VCon "WriteMode"     _) = WriteMode
ioModeFromVal (VCon "AppendMode"    _) = AppendMode
ioModeFromVal (VCon "ReadWriteMode" _) = ReadWriteMode
ioModeFromVal v = error ("openFile: not an IOMode: " <> showValForDebug v)

bufferModeFromVal :: Val -> BufferMode
bufferModeFromVal (VCon "NoBuffering"   _) = NoBuffering
bufferModeFromVal (VCon "LineBuffering" _) = LineBuffering
bufferModeFromVal (VCon "BlockBuffering" _) = BlockBuffering Nothing
bufferModeFromVal v = error ("hSetBuffering: not a BufferMode: "
                             <> showValForDebug v)

openFileB :: IO Val
openFileB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    pv  <- force a
    path <- valToString pv
    mv  <- force b
    let mode = ioModeFromVal mv
    h <- openFile path mode
    pure (VPrimObj (PrimHandle h))

hCloseB :: IO Val
hCloseB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force a >>= requireHandle "hClose"
    hClose h
    pure VUnit

hPutStrB :: IO Val
hPutStrB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force a >>= requireHandle "hPutStr"
    sv <- force b
    s  <- valToString sv
    hPutStr h s
    pure VUnit

hPutStrLnB :: IO Val
hPutStrLnB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force a >>= requireHandle "hPutStrLn"
    sv <- force b
    s  <- valToString sv
    hPutStrLn h s
    pure VUnit

hGetLineB :: IO Val
hGetLineB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force a >>= requireHandle "hGetLine"
    s <- hGetLine h
    stringToListValIO s

hFlushB :: IO Val
hFlushB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force a >>= requireHandle "hFlush"
    hFlush h
    pure VUnit

hSetBufferingB :: IO Val
hSetBufferingB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force a >>= requireHandle "hSetBuffering"
    mv <- force b
    hSetBuffering h (bufferModeFromVal mv)
    pure VUnit

--------------------------------------------------------------------------------
-- Control flow.
--------------------------------------------------------------------------------

-- | @seq a b@: force @a@ to WHNF, then return @b@.
seqB :: IO Val
seqB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    _ <- force a
    force b

-- | @f $! x@: force @x@, then apply @f@ to the (now-evaluated) thunk.
-- Returns a 1-arg remainder (curried).
dollarBangB :: IO Val
dollarBangB = pure $ VFun $ \f -> pure $ VFun $ \x -> do
    xv <- force x
    xT <- newWHNFThunk xv
    fv <- force f
    apply fv xT

-- | @exitWith code@: throws 'ExitCode'. Wrapped in VIO so it's delayed.
exitWithB :: IO Val
exitWithB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    case av of
        VCon "ExitSuccess" _ -> throwIO ExitSuccess
        VCon "ExitFailure" [nT] -> do
            nv <- force nT
            case nv of
                VInt n -> throwIO (ExitFailure (fromIntegral n))
                _ -> error ("exitWith ExitFailure: not an Int: "
                            <> showValForDebug nv)
        VInt n -> throwIO (if n == 0 then ExitSuccess
                                     else ExitFailure (fromIntegral n))
        _ -> error ("exitWith: not an ExitCode: " <> showValForDebug av)

exitSuccessB :: IO Val
exitSuccessB = pure $ VIO (throwIO ExitSuccess)

--------------------------------------------------------------------------------
-- Char / numeric conversions.
--------------------------------------------------------------------------------

ordB :: IO Val
ordB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VChar c -> pure (VInt (fromIntegral (ord c)))
        _ -> error ("ord: not a Char: " <> showValForDebug av)

chrB :: IO Val
chrB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VChar (chr (fromIntegral n)))
        _ -> error ("chr: not an Int: " <> showValForDebug av)

-- | 'fromIntegral' as an Int identity in Phase 2.4 — we only have one
-- Int-like numeric type, so the coercion collapses.
fromIntegralB :: IO Val
fromIntegralB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt n)
        _ -> error ("fromIntegral: not an Int: " <> showValForDebug av)

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
