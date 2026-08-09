-- | The standard environment that every program starts in.
--
-- Each builtin is a Haskell function returning @IO Val@, taking its
-- arguments as 'Thunk's so it can be lazy if it wants. Most are
-- strict in their numeric arguments (force first), since the
-- arithmetic operators need actual numbers.
--
-- The evaluator and the builtins are both Haskell code in the same
-- process, so calls are direct (no @foreign export@ / FFI bridge).
module IHC.Builtins
    ( builtinEnv
    , buildConEnv
    , buildFieldEnv
    , showValWith
    , stringToListValIO
    , clearCtorIndex
    , clearForeignPtrWord8Ranges
    , foreignPtrValToForeignPtr
    , flushHostHandleBuffer
    , isHostWord8PtrVal
    , ordCmp
    , eqByteStringHost
    , peekHostWord8ByteOff
    , pokeHostWord8ByteOff
    , reapSpawnedThreads
    ) where

import Control.Concurrent
    ( ThreadId, forkIO, killThread, myThreadId, threadDelay
    , threadWaitRead, threadWaitWrite
    )
import Control.Concurrent.MVar
    ( MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar
    , tryTakeMVar, tryPutMVar, isEmptyMVar
    )
import Control.Concurrent.STM
    ( TVar, atomically
    , newTVarIO, readTVar, writeTVar, readTVarIO
    )
import qualified Control.Exception as CE
import Control.Exception
    ( throwIO, try
    , throwTo
    , SomeException
    )
import Data.Bits
    ( (.&.), (.|.), xor, complement, shiftL, shiftR
    , popCount, countLeadingZeros, countTrailingZeros, finiteBitSize
    , bit, testBit, clearBit, setBit, complementBit
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import Data.Int (Int8, Int64)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32)
import Numeric.Natural (Natural)
import Foreign.C.String (peekCAString)
import Foreign.ForeignPtr
    ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr, newForeignPtr_
    , plusForeignPtr )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes, fillBytes, moveBytes)
import Foreign.Ptr (Ptr, IntPtr, castPtr, plusPtr, nullPtr, minusPtr, intPtrToPtr, ptrToIntPtr)
import qualified Foreign.Ptr as FP
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff, peekElemOff, pokeElemOff)
import qualified System.Info
import System.IO.Unsafe (unsafePerformIO)
import System.IO
    ( BufferMode(..)
    , Handle
    , IOMode(..)
    , hClose
    , hFlush
    , hGetContents
    , hGetLine
    , hPutBuf
    , hPutChar
    , hPutStr
    , hPutStrLn
    , hSetBuffering
    , openFile
    , stderr
    , stdin
    , stdout
    )
import Control.Monad (when)
import IHC.AST  (Name, Expr(..))
import IHC.Classes
    ( ClassRegistry, lookupInstanceMethod, registerInstance, typeTagOf, normalizeTyTag
    , mkTypeRep, typeRepEq
    , drainCataloguedInstancesForClass
    , legacyHooks
    )
import qualified IHC.Classes
import IHC.Eval (apply, force, forceMethodVal, runIOVal)
import IHC.Scan (DataRegistry, FieldRegistry, lookupCtorStrictness)
import IHC.TH (thBuiltinPairs)
import IHC.Val

mkForeignPtrVal :: ForeignPtr Word8 -> IO Val
mkForeignPtrVal fp = do
    markForeignPtrWord8 fp
    addrT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr (unsafeForeignPtrToPtr fp))))
    gutsT <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    pure (VCon "ForeignPtr" [addrT, gutsT])

-- word8PtrRangesRef / markWord8PtrRange / isMarkedWord8Ptr live in
-- IHC.Val so Eval.byteStringConFromBS can mark OverloadedStrings
-- buffers the same way mallocForeignPtrBytesB does.

-- | Every interpreter-spawned thread (source-loaded 'forkIO' via 'fork#').  An
-- interpreted program's @main@ can fork background threads (warp's
-- accept loop, System.TimeManager, async workers, bare @forkIO@) that
-- are still alive — running or blocked on an MVar\/STM\/threadDelay —
-- when 'IHC.Driver.runFile' has already forced @main@'s result and
-- returned.  Nothing reaps them, so under the ~600-example in-process
-- hspec suite their TSO stacks accumulate without bound: a heap
-- profile of the full run is ~4 GB of @STACK@ (everything else ≤56 MB),
-- which is the master-CI OOM.  'reapSpawnedThreads' (wired into
-- 'IHC.Scheduler.resetPerRunGlobals') kills the prior run's threads at
-- the next run boundary so their stacks become collectable.
{-# NOINLINE spawnedThreadsRef #-}
spawnedThreadsRef :: IORef [ThreadId]
spawnedThreadsRef = unsafePerformIO (newIORef [])

-- | Record an interpreter-spawned thread for end-of-run reaping.
registerSpawnedThread :: ThreadId -> IO ()
registerSpawnedThread tid = modifyIORef' spawnedThreadsRef (tid :)

markWord8Ptr :: Ptr Word8 -> IO ()
markWord8Ptr p = markWord8PtrRange p 1

markForeignPtrWord8 :: ForeignPtr Word8 -> IO ()
markForeignPtrWord8 fp =
    markWord8Ptr (castPtr (unsafeForeignPtrToPtr fp))

isHostWord8PtrVal :: Val -> IO Bool
isHostWord8PtrVal v = do
    p <- ptrValToPtr v
    marked <- isMarkedWord8Ptr p
    if marked
        then pure True
        else do
            mTyped <- lookupTypedHostPtr p
            pure (mTyped `elem` map Just [BC.pack "Word8", BC.pack "CChar", BC.pack "CSChar", BC.pack "CUChar"])

pokeHostWord8ByteOff :: Val -> Val -> Val -> IO Val
pokeHostWord8ByteOff ptrV offV valV = do
    p <- ptrValToPtr ptrV
    off <- byteOffsetFromVal "pokeHostWord8ByteOff" offV
    byte <- word8FromVal valV
    pokeByteOff (p :: Ptr Word8) off byte
    pure VUnit
  where
    word8FromVal (VInt n) =
        pure (fromIntegral n :: Word8)
    word8FromVal (VInteger n) =
        pure (fromInteger n :: Word8)
    word8FromVal (VCon "W8#" [t]) = do
        inner <- force legacyHooks t
        case inner of
            VInt n -> pure (fromIntegral n :: Word8)
            _ -> error ("pokeHostWord8ByteOff: W8# inner not Int: "
                        <> showValForDebug inner)
    word8FromVal (VChar c) =
        pure (fromIntegral (ord c) :: Word8)
    word8FromVal other =
        error ("pokeHostWord8ByteOff: bad byte: " <> showValForDebug other)

peekHostWord8ByteOff :: Val -> Val -> IO Val
peekHostWord8ByteOff ptrV offV = do
    p <- ptrValToPtr ptrV
    off <- byteOffsetFromVal "peekHostWord8ByteOff" offV
    mTyped <- lookupTypedHostPtr p
    case mTyped of
        Just ty | ty `elem` map BC.pack ["CChar", "CSChar"] -> do
            byte <- peekByteOff (castPtr p :: Ptr Int8) off :: IO Int8
            pure (VInt (fromIntegral byte))
        _ -> do
            byte <- peekByteOff (p :: Ptr Word8) off :: IO Word8
            pure (VInt (fromIntegral byte))

byteOffsetFromVal :: String -> Val -> IO Int
byteOffsetFromVal who (VInt off) =
    pure (fromIntegral off)
byteOffsetFromVal who (VInteger off) =
    pure (fromInteger off)
byteOffsetFromVal who other =
    error (who <> ": bad offset: " <> showValForDebug other)

ptrValToPtr :: Val -> IO (Ptr Word8)
ptrValToPtr (VPrimObj (PrimPtr p)) = pure p
ptrValToPtr (VCon "Ptr" [pT]) = force legacyHooks pT >>= ptrValToPtr
ptrValToPtr other = error ("expected Ptr: " <> showValForDebug other)

foreignPtrValToForeignPtr :: Val -> IO (ForeignPtr Word8)
foreignPtrValToForeignPtr (VPrimObj (PrimForeignPtr fp)) = pure fp
foreignPtrValToForeignPtr (VCon "ForeignPtr" [addrT, gutsT]) = do
    gv <- force legacyHooks gutsT
    case gv of
        VPrimObj (PrimForeignPtr fp) -> do
            -- Source-loaded @plusForeignPtr (ForeignPtr addr c) (I# d)
            -- = ForeignPtr (plusAddr# addr d) c@ stores the NEW
            -- address in 'addrT' but keeps the ORIGINAL
            -- 'PrimForeignPtr' as the finalizer-carrying 'gutsT'
            -- stand-in.  If we returned the original fp directly,
            -- the offset would be lost.  Apply the address
            -- difference via the host's 'plusForeignPtr' so the
            -- returned 'ForeignPtr' points at the right byte AND
            -- shares the finalizer of the underlying allocation.
            addrV <- force legacyHooks addrT
            newP <- ptrValToPtr addrV
            let origPtr = unsafeForeignPtrToPtr fp
                offset  = newP `minusPtr` origPtr
            pure (plusForeignPtr fp offset)
        -- Source-loaded code can construct ForeignPtr values whose guts are
        -- constructors like FinalPtr rather than our host PrimForeignPtr.
        -- Rebuild an equivalent host ForeignPtr from the raw address so the
        -- RTS-backed pointer builtins can still operate on it.
        _ -> do
            addrV <- force legacyHooks addrT
            p <- ptrValToPtr addrV
            newForeignPtr_ (castPtr p)
foreignPtrValToForeignPtr other = error ("expected ForeignPtr: " <> showValForDebug other)

-- | Build the initial environment containing every well-known name.
--
-- This also registers the built-in list constructors @[]@ and @(:)@
-- — lists are Phase 2.2's first taste of a built-in ADT. We treat
-- them exactly like user-declared constructors from 'buildConEnv':
-- arity-0 nil is a bare @VCon "[]" []@; arity-2 cons is a curried
-- function that accumulates two thunks and returns @VCon ":" [h, t]@.
--
-- The 'ClassRegistry' is threaded in so dispatch operations like @==@
-- and @show@ can look up user-defined instances at runtime.
builtinEnv :: ClassRegistry -> IO Env
builtinEnv reg = do
    -- Lazy-init the bulk of the builtin table: a hello-world program only
    -- touches a handful of these (putStrLn, show, …), yet eager allocation
    -- used to spend ~60-80ms on IORef + VFun allocation per startup. We now
    -- store each entry as a 'LazyBuiltin' thunk that runs the host 'IO Val'
    -- action on first force (see 'IHC.Val.ThunkState' and 'IHC.Eval.force').
    pairs <- mapM (\(n, mkV) -> do { t <- newLazyBuiltinThunk mkV; pure (n, t) })
                  (builtins reg)
    -- Arity-0 ctor thunks (VCon name []) are already tiny constant values,
    -- so we keep them eager — deferring wouldn't save anything meaningful
    -- and many of these (True/False/[]/LT/…) are on virtually every hot
    -- path anyway.
    nilT  <- newWHNFThunk (VCon "[]" [])
    consT <- newWHNFThunk consV
    let listCtors = [("[]", nilT), (":", consT)]
    -- Phase 2.3: True/False are now proper VCon constructors.
    -- The EIf evaluator already handles both VInt and VCon "True"/"False".
    -- `otherwise` remains VInt 1 for back-compat with guard patterns.
    otherT <- newWHNFThunk (VInt 1)
    trueT  <- newWHNFThunk (VCon "True"  [])
    falseT <- newWHNFThunk (VCon "False" [])
    let boolish = [("otherwise", otherT), ("True", trueT), ("False", falseT)]
    -- IOMode/BufferMode ctors: arity-0 data constructors surfaced so
    -- that primops like `openFile path ReadMode` can pattern match.
    ioModes <- mapM mkCon0
        [ "ReadMode", "WriteMode", "AppendMode", "ReadWriteMode"
        , "NoBuffering", "LineBuffering", "BlockBuffering"
        ]
    -- Standard handles — source-loaded FileHandle constructors.
    stdinT  <- newWHNFThunk =<< mkFileHandleVal "<stdin>"  stdin  ReadMode
    stdoutT <- newWHNFThunk =<< mkFileHandleVal "<stdout>" stdout WriteMode
    stderrT <- newWHNFThunk =<< mkFileHandleVal "<stderr>" stderr WriteMode
    let handles = [("stdin", stdinT), ("stdout", stdoutT), ("stderr", stderrT)]
    -- Ordering constructors.
    ltT <- newWHNFThunk (VCon "LT" [])
    eqT <- newWHNFThunk (VCon "EQ" [])
    gtT <- newWHNFThunk (VCon "GT" [])
    let orderingCtors = [("LT", ltT), ("EQ", eqT), ("GT", gtT)]
    -- Unit constructor: () → VUnit.
    -- The parser emits EVar "()" for the () expression; VUnit is the
    -- canonical runtime representation so we register the name here
    -- exactly as we do for True/False/[]/Nothing.
    unitT <- newWHNFThunk VUnit
    let unitCtor = [("()", unitT)]
    -- Phase 2.8: unboxed tuple constructors (# , #), (# ,, #) etc.
    -- Lazy-init — most programs never construct unboxed tuples.
    unbox2T <- newLazyBuiltinThunk (pure (VFun $ \a -> pure $ VFun $ \b -> pure (VCon "(#,#)" [a, b])))
    unbox3T <- newLazyBuiltinThunk (pure (VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure (VCon "(#,,#)" [a, b, c])))
    -- Unboxed-sum constructor: @(# a | b | ... #)@.  IHC encodes
    -- every unboxed sum, regardless of alternative count, as a
    -- 2-field @VCon "(#|#)" [tagThunk, payloadThunk]@ where
    -- @tagThunk@ evaluates to @VInt n@ (1-based alternative index)
    -- and @payloadThunk@ holds that alternative's value.  The parser
    -- desugars @(# x | #)@ → @(#|#) 1 x@ and @(# | x #)@ →
    -- @(#|#) 2 x@; matchPat's generic @PCon/VCon@ zip handles the
    -- tag check (a 'PLit (LInt n)' sub-pattern) + payload bind.
    unboxSumT <- newLazyBuiltinThunk (pure (VFun $ \tag -> pure $ VFun $ \v -> pure (VCon "(#|#)" [tag, v])))
    let unboxCtors = [("(#,#)", unbox2T), ("(#,,#)", unbox3T), ("(#|#)", unboxSumT)]
    -- IO constructor: IO wraps a (State# RealWorld -> (# State# RealWorld, a #))
    -- function into a VIO action.  GHC.Types defines `newtype IO a = IO (State# ...)`
    -- but since GHC.Types is builtin-backed (no .hs source), we register it here.
    -- Deferred: the VFun isn't needed until the source actually constructs an IO
    -- value from a state-passing function (most programs go through primops).
    ioCtorT <- newLazyBuiltinThunk (pure (VFun $ \fThunk -> do
        f <- force legacyHooks fThunk
        pure (VIO (do
            let stok = VPrimObj PrimRealWorld
            stokT <- newWHNFThunk stok
            r <- apply legacyHooks f stokT
            case r of
                VCon "(#,#)" [_stT, aT] -> do
                    v <- force legacyHooks aT
                    pure v
                other                    -> pure other))))
    -- Ptr smart constructor: `Ptr addr#` wraps an Addr# (PrimPtr) back into PrimPtr.
    ptrCtorT <- newLazyBuiltinThunk (pure ptrCtorV)
    -- Phase 2.9.5: Proxy and Dynamic constructors.
    -- Phase 3.5 note: when a VLabel is used where a Proxy is expected,
    -- fromLabel produces VCon "Proxy" [VLabel name].
    proxyT    <- newWHNFThunk (VCon "Proxy" [])
    let phase295Ctors = [("Proxy", proxyT)]
    -- Phase 2.9.5: Built-in Typeable dictionaries for primitive types.
    -- Lazy-init: each dict costs two IORef allocs + a VCon, and most
    -- programs only touch typeableDict_Int / _Char / _Bool.
    typeableInsts <- buildBuiltinTypeableInsts reg
    -- Phase 3.5: Default IsLabel dispatch.
    -- Register the IHP-style default instance `(s ~ s') => IsLabel s (Proxy s')`
    -- under the synthetic type tag "Proxy": when `fromLabel` is applied to a
    -- VLabel and no user-defined IsLabel instance wins, dispatch falls through
    -- to this one which produces `VCon "Proxy" []`.
    -- The method slot order mirrors the class declaration: [fromLabel].
    -- fromLabelB just builds a small VFun — leave eager to avoid special-casing
    -- the ClassRegistry (which is a separate store from the Env).
    defaultFromLabel <- fromLabelB reg
    registerInstance reg (BC.pack "IsLabel") (BC.pack "Proxy")
        (HashMap.singleton (BC.pack "fromLabel") defaultFromLabel)
    pure (extendEnvMany (pairs ++ listCtors ++ boolish ++ ioModes ++ handles
                         ++ orderingCtors ++ unboxCtors
                         ++ unitCtor ++ [("IO", ioCtorT)]
                         ++ [("Ptr", ptrCtorT)]
                         ++ phase295Ctors ++ typeableInsts)
                        emptyEnv)
  where
    consV = VFun $ \h -> pure $ VFun $ \t -> pure (VCon ":" [h, t])
    mkCon0 name = do
        t <- newWHNFThunk (VCon name [])
        pure (name, t)
    -- Ptr constructor: `Ptr addr#` => VPrimObj (PrimPtr p).
    -- The Addr# is already a VPrimObj PrimPtr internally.
    ptrCtorV = VFun $ \addrT -> do
        v <- force legacyHooks addrT
        case v of
            VPrimObj (PrimPtr p) -> pure (VPrimObj (PrimPtr p))
            _                   -> pure (VCon "Ptr" [addrT])  -- fallback

builtins :: ClassRegistry -> [(Name, IO Val)]
builtins reg =
    -- Arithmetic: +, -, *, /, mod, div graduated to source-loaded.
    --
    -- Num/Integral/Fractional instance bodies in GHC.Internal.Num,
    -- GHC.Internal.Real, GHC.Internal.Float bottom out on the +#,
    -- -#, *#, modInt#, divInt# (Int) / plusFloat#, minusFloat#,
    -- timesFloat#, divideFloat# (Float) / +##, -##, *##, /## (Double)
    -- primops (all registered below) via the I#, F#, D# pattern
    -- unwraps in Eval.hs:matchPat.
    --
    -- abs / signum dropped earlier (same mechanism): instance bodies
    -- in GHC.Internal.Num resolve via the env-fallback class-method
    -- dispatcher.
    -- @min@ and @max@ deliberately omitted: source bodies live in
    -- @GHC.Classes@'s @class Ord@ defaults
    --   max x y = if x <= y then y else x
    --   min x y = if x <= y then x else y
    -- and resolve through the source-loaded @<=@ class method via the
    -- env-fallback's 'tryClassMethodFromRegistry'.
    --
    -- 'gcd' graduated to source-loaded.  Body lives at
    --   ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Real.hs:928-930
    --     gcd x y = gcd' (abs x) (abs y)
    --       where gcd' a 0 = a
    --             gcd' a b = gcd' b (a `rem` b)
    -- abs already graduated in Phase E; rem source-loads from
    -- 'Integral Int.rem' (Real.hs:452-455 → remInt → remInt#).
    -- The Phase F buildOwnerLocalEnv guard handles class-method
    -- resolution inside the source-loaded body.
    --
    -- 'sqrt' graduated to source-loaded (Builtins-removal batch).
    -- The 'Floating Double' instance method body lives at
    --   ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Float.hs:746
    --     sqrt x = sqrtDouble x
    -- and 'sqrtDouble' (Float.hs:1578) bottoms on the 'sqrtDouble#'
    -- GHC.Prim primop: @sqrtDouble (D# x) = D# (sqrtDouble# x)@.
    -- The ("sqrt","Floating") class-method seed is registered in
    -- 'IHC.TypeGlobals.seedBuiltinClassMethodSigs', and the carved-out
    -- 'sqrtDouble#' GHC.Prim builtin (no .hs source — PrimopWrappers.hs
    -- just re-exports GHC.Prim.sqrtDouble#) is registered below next to
    -- the other Double# unary primops.
    --
    -- Phase 5: graduated 'floor' / 'ceiling' / 'round' / 'truncate'
    -- to source-loaded.  The chain is:
    --
    --   floor :: RealFrac Double => Double -> Int
    --   = RealFrac Double.floor  (source-loaded GHC.Internal.Float)
    --   = floorDouble x
    --   = case properFractionDouble x of (n,r) -> if r < 0 then n-1 else n
    --   = ... uses 'decodeFloat' / 'encodeFloat' / Integer arithmetic
    --
    -- Needed primops added in this PR:
    --   int2Double#, intEncodeDouble#, negateDouble#, double2Int#,
    --   double2Float#, float2Double#  — Int#/Double# bridges
    --   ==##, /=##, <##, <=##, >##, >=##  — Double# comparisons
    --   fabsDouble#, negateFloat#  — Double# unary
    --
    -- 'encodeFloat' / 'decodeFloat' source-load via RealFloat now.
    -- 'fromIntegral' also source-loads from GHC.Internal.Real:
    --   fromIntegral = fromInteger . toInteger
    -- The source path relies on class dispatch for Integral.toInteger
    -- and Num.fromInteger; ghc-bignum's IS/IP/IN constructors normalize
    -- to the Integer type tag in 'IHC.Classes.typeTagOf'.
    --
    -- 'maxBound' / 'minBound' source-load via Bounded in
    -- GHC.Internal.Enum.  Nullary dispatch is type-directed through
    -- ETyApp / wrapNullaryResultSig instead of a host Int default.
    -- Comparisons: @==@, @/=@, @<@, @<=@, @>@, and @>=@ are
    -- deliberately omitted.  They have real source in @GHC.Classes@'s
    -- @class Eq@ / @class Ord@ methods and route through the
    -- source-loaded class-method dispatcher seeded in
    -- 'IHC.TypeGlobals.seedBuiltinClassMethodSigs'.  The discovery
    -- cascade that previously blocked this removal is covered by
    -- @test/Fixtures/Coverage/discovery_compare_eq_ordering.hs@.
    --
    -- @compare@ is deliberately omitted: source body lives in
    -- @GHC.Classes@'s @class Ord@ default
    --   compare x y = if x == y then EQ else if x <= y then LT else GT
    -- and per-instance overrides (e.g. @compare = compareInt@ for Int).
    -- The source-loaded class-method dispatcher binds @compare@ on
    -- demand via the env-fallback's @tryClassMethodFromRegistry@.
    --
    -- (&&), (||), and `not` are deliberately omitted — their
    -- source bodies live at
    --   ~/.cache/ihc/sources/ghc-prim-0.12.0/GHC/Classes.hs:597-609
    --     (&&) :: Bool -> Bool -> Bool
    --     True  && x  =  x
    --     False && _  =  False
    --     (||) :: Bool -> Bool -> Bool
    --     True  || _  =  True
    --     False || x  =  x
    --     not  :: Bool -> Bool
    --     not True  = False
    --     not False = True
    -- which the source-loaded GHC.Classes path interprets.  Per
    -- CLAUDE.md "Builtin modules: minimum surface only", any
    -- symbol with .hs source must be interpreted, not shimmed.
    --
    -- 'show', 'concatMap', 'length', and the entire @Data.ByteString.*@
    -- shim block (pack, empty, null, length, head, take, drop, replicate,
    -- unpack, append, index, concat, singleton, Char8.putStrLn, create,
    -- createFp, createAndTrim, createFpAndTrim, PS, minusForeignPtr) all
    -- graduated to pure source — see CLAUDE.md rule 4.  Resolution flows
    -- through the demand-driven 'classMethodDispatcher' for class methods
    -- (show, length via Foldable), through source-loaded module bodies
    -- for plain Haskell functions (concatMap from GHC.Internal.List,
    -- everything in Data.ByteString.Internal.Type), and through the
    -- RTS-level primitives still host-backed below (mallocPlainForeignPtrBytes,
    -- minusAddr#, etc.) for actual allocation/pointer boundaries.
    [
    -- Data.ByteString.Char8: graduated to pure source (empty/null/
    -- length/pack/unpack/head/index/singleton/replicate/concat/
    -- append/putStrLn).  'Char8.putStrLn' was the last hold-out —
    -- its source path uses bare 'length ps' on a 'BS' value, which
    -- used to route to the builtin list-walking 'lengthB'.  Dropping
    -- 'lengthB' from the builtin registry above lets
    -- 'buildImportRewrites' honour Char8's explicit
    -- @import Data.ByteString (length, …)@ and route 'length' to
    -- 'Data.ByteString.length' (the BS-specific function).
    -- IO
    --
    -- 'putStrLn' deliberately omitted: it has source at
    -- ~/.cache/ihc/sources/base-4.19.0.0/System/IO.hs:282-283
    --     putStrLn s = hPutStrLn stdout s
    -- Per CLAUDE.md "Builtin modules: minimum surface only", any symbol
    -- with .hs source must be interpreted, not shimmed. Keeping the shim
    -- short-circuits demand discovery on 'putStrLn' (the most-typed name
    -- in the suite), which causes the FV walk to record phantom misses
    -- on whatever the source body would have referenced — the workaround
    -- for those misses is the manifest-driven core load and the
    -- eager-load-every-import phase in 'loadProgramFromSource'. The
    -- source path bottoms out one level down on 'hPutStrLn' (still
    -- shimmed), so demand discovery walks two AST nodes and reaches an
    -- existing primop boundary. See plan
    --     ~/.claude/plans/why-is-ihc-test-taking-silly-mango.md
    -- for the multi-slice arc.
    --
    -- Slice 2: 'putStr' and 'putChar' graduate to source-loaded under
    -- the same rule. Source bodies are
    --     putStr s   =  hPutStr stdout s        -- System/IO.hs:278
    --     putChar c  =  hPutChar stdout c       -- System/IO.hs:272
    -- Both bottom out on host shims already pinned via 'ffiBuiltinNames'
    -- in slice 1 (hPutStr, hPutChar, stdout).
    --
    -- Slice 4: 'print' graduates. Source body is
    --     print x  =  putStrLn (show x)         -- System/IO.hs:296-297
    -- 'putStrLn' is source-loaded (slice 1).  'show' is now source-
    -- loaded too (see the @show@ omission note above) — both calls
    -- inside @print@ resolve through the demand-driven Prelude /
    -- class-method dispatcher.
    --
    -- Slice 3 (this commit): 'getLine' graduates. Source body is
    --     getLine  =  hGetLine stdin            -- System/IO.hs:308-309
    -- Bottoms out on 'hGetLine' (still a host shim — see
    -- 'ffiBuiltinNames' in Scheduler.hs for why the shim must take
    -- precedence over the source-level GHC.IO.Handle.Text definition
    -- until the source-level Handle ADT layer is implemented).
    --
    -- 'getContents' is similarly source-loaded from
    --     getContents  =  hGetContents stdin    -- System/IO.hs:316-317
    -- It was never shimmed in the first place; bottoms out on host
    -- 'hGetContents' (pinned below) once that name is demanded.
    --
    -- Slice: 'readFile' / 'writeFile' / 'appendFile' graduate to source.
    -- Source bodies (GHC.Internal.System.IO / System.IO) are ordinary
    -- wrappers:
    --   readFile name = openFile name ReadMode >>= hGetContents
    --   writeFile f txt = withFile f WriteMode (\hdl -> hPutStr hdl txt)
    --   appendFile f txt = withFile f AppendMode (\hdl -> hPutStr hdl txt)
    -- They bottom out on host 'openFile' / 'withFile' / 'hGetContents' /
    -- 'hClose' / 'hPutStr' — temporary Handle-device carve-outs (same
    -- family as hPutStrLn / hGetLine): source Handle ADT + locale
    -- encoding is not modelled yet, so these names are pinned via
    -- 'ffiBuiltinNames' and must resolve to PrimHandle host shims.
    -- Monad bind and sequence are deliberately omitted: they are class
    -- methods with real source in GHC.Internal.Base and resolve through
    -- the seeded class-method dispatcher.
    -- 'pure' / 'return' deliberately omitted: both are class methods
    -- with real source in @GHC.Internal.Base@.  Bare references route
    -- through the seeded class-method dispatcher and its
    -- result-polymorphic fallback.
    -- fmap / <*>: source-loaded from the corresponding Functor /
    -- Applicative instances in GHC.Internal.Base.  No host-backed
    -- dispatchers -- per CLAUDE.md "No host-backed class method
    -- dispatchers".  Resolved via classMethodDispatcher.
    -- 'Semigroup.(<>)' deliberately omitted: it is a class method with
    -- real source in @GHC.Internal.Base@, so bare references resolve
    -- through the seeded class-method dispatcher.
    --
    -- 'join' deliberately omitted: it has source at
    -- ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Base.hs:1292-1293
    --     join :: Monad m => m (m a) -> m a
    --     join x = x >>= id
    -- The source dispatches via the Monad class-method dispatcher.
    --
    -- 'void', 'first', 'second', 'runIdentity' all graduated to
    -- pure source.  Their definitions are:
    --   void x = () <$ x          -- Functor class method via <$
    --   first  f (a, b) = (f a, b) -- Arrow (->) instance method
    --   second g (a, b) = (a, g b) -- ditto
    --   runIdentity (Identity x) = x  -- newtype field accessor
    -- IORef wrappers are deliberately omitted.  Data.IORef,
    -- GHC.IORef, GHC.Internal.IORef, and GHC.Internal.Data.IORef all
    -- have real source; they lower to the MutVar# and Weak# primops below.
    -- File IO (Handle-device boundary; high-level readFile/writeFile/
    -- appendFile are source-loaded — see slice comment above).
      ("openFile",    openFileB)
    , ("hClose",      hCloseB)
    , ("withFile",    withFileB)
    , ("hGetContents", hGetContentsB)
    , ("hPutStr",     hPutStrB)
    , ("hPutChar",    hPutCharB)
    , ("hPutStrLn",   hPutStrLnB)
    , ("hGetLine",    hGetLineB)
    , ("hFlush",      hFlushB)
    , ("hSetBuffering", hSetBufferingB)
    -- Control flow
    , ("seq",         seqB)
    -- error / undefined source-load from GHC.Internal.Err and bottom out
    -- in errorCall* helpers plus the raise# primop below.
    -- B.1: debug-only superclass-relation probe.  Source-loaded code
    -- can call @__ihc_class_supers \"MyOrd\"@ to inspect the global
    -- superclass map; useful for testing that the scanner captured
    -- the @class C a => D a@ relation. Single argument is a [Char]
    -- list (a String); result is a [[Char]] list (a [String]).
    , ("__ihc_class_supers", classSupersProbeB)
    -- Char / numeric conversions.
    -- NOTE: only the GHC.Prim primops @ord#@ / @chr#@ are host-backed.
    -- @Data.Char.ord@ / @Data.Char.chr@ have real source — @ord@ in
    -- @GHC/Internal/Base.hs@ (@ord (C# c#) = I# (ord# c#)@) and @chr@ in
    -- @GHC/Internal/Char.hs@ (@chr i\@(I# i#) | isTrue# (...) = C# (chr# i#)
    -- | otherwise = ...@) — so they are interpreted from source via the
    -- env-fallback path, bottoming out on these primops. Do NOT re-add
    -- the bare ("ord"/"chr") shims.
    , ("ord#",        ordB)
    , ("chr#",        chrB)
    -- isTrue# :: Int# -> Bool.  Projects the 1#/0# unboxed-Int encoding
    -- of a comparison primop result (@==#@, @<#@, etc.) into a regular
    -- Bool.  Our Int# is VInt, so it's a plain @/= 0@.
    , ("isTrue#",     isTrueHashB)
    -- Phase 1: BigNat# runtime representation (first slice of the
    -- full source-loaded Integer roadmap, see
    -- @plans/full-ghc-bignum-source-load.md@).  Source-loading the
    -- ghc-bignum primop suite from 'GHC.Num.BigNat' would bottom out
    -- in WordArray# / ByteArray# limb manipulation that doesn't
    -- match the 'PrimBigNat !Natural' runtime chosen in Phase 1.  Per
    -- the roadmap, the entire @bigNat*#@ family is intentionally
    -- host-shimmed as thin wrappers over host 'Natural' arithmetic.
    -- This is a tracked carve-out (NOT a "minimum surface" violation
    -- per CLAUDE.md): the representation mismatch makes source-load
    -- semantically wrong, not just slow.
    --
    -- Phase 1 landed: @bigNatFromWord#@.  Phase 2.A adds the 10
    -- comparison primops (this block, below); Phase 2.B–F add
    -- arithmetic / bit-ops / remaining conversions / show / long-tail.
    , ("bigNatFromWord#",          bigNatFromWordB)
    -- bigNatCompare and the bigNat*# comparison predicates source-load
    -- from ghc-bignum.  Their backend compare loop is pure Haskell over
    -- the retained wordArraySize#/indexWordArray# leaves.
    -- bigNatSize#, bigNatIsZero#, and bigNatIsOne# source-load from
    -- ghc-bignum; their bodies bottom on wordArraySize# and
    -- indexWordArray#, which are retained lower leaves.
    -- Phase 2.B: arithmetic primops.  Thin wrappers over host
    -- 'Numeric.Natural' arithmetic.  Signatures from
    -- @~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs@.
    --
    -- 'bigNatSub' (returning @(# (# #) | BigNat# #)@) is DEFERRED —
    -- IHC has no unboxed-sum runtime yet; ghc-bignum's source-level
    -- @case bigNatSub a b of (# (# #) | #) -> ... ; (# | bn #) -> ...@
    -- can't be matched against any VCon we currently produce.  Tracked
    -- as a follow-up.  Callers that don't need the underflow check
    -- can use 'bigNatSubUnsafe' instead; both ghc-bignum 'integerSub'
    -- callsites guard with 'bigNatCompare' first.
    --
    -- 'bigNatPow#' is listed in the Phase 2 plan but doesn't exist in
    -- ghc-bignum source.  The closest primop is 'bigNatPowModWord#'
    -- (modular exponentiation), which is niche and deferred.
    --
    -- For symmetry with 'bigNatSub', 'bigNatIsPowerOf2#' (also
    -- @(# (# #) | Word# #)@-returning) is deferred to the same
    -- unboxed-sum follow-up.
    , ("bigNatAdd",                makeBigNatBinOp "bigNatAdd" (+))
    , ("bigNatMul",                makeBigNatBinOp "bigNatMul" (*))
    , ("bigNatSubUnsafe",          makeBigNatBinOp "bigNatSubUnsafe" (-))
    -- Unboxed-sum-returning primops (now that the runtime exists):
    --   bigNatSub        :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)
    --   bigNatIsPowerOf2# :: BigNat# -> (# (# #) | Word# #)
    , ("bigNatSub",                bigNatSubB)
    , ("bigNatIsPowerOf2#",        bigNatIsPowerOf2HashB)
    , ("bigNatQuot",               makeBigNatBinOp "bigNatQuot" quot)
    , ("bigNatRem",                makeBigNatBinOp "bigNatRem" rem)
    , ("bigNatGcd",                makeBigNatBinOp "bigNatGcd" gcd)
    , ("bigNatLcm",                makeBigNatBinOp "bigNatLcm" lcm)
    -- bigNatSqr source-loads from ghc-bignum:
    -- bigNatSqr a = bigNatMul a a.
    , ("bigNatQuotRem#",           bigNatQuotRemHashB)
    , ("bigNatAddWord#",           makeBigNatWordOp "bigNatAddWord#" (+))
    , ("bigNatMulWord#",           makeBigNatWordOp "bigNatMulWord#" (*))
    , ("bigNatQuotWord#",          makeBigNatWordOp "bigNatQuotWord#" quot)
    , ("bigNatSubWordUnsafe#",     makeBigNatWordOp "bigNatSubWordUnsafe#" (-))
    , ("bigNatRemWord#",           bigNatRemWordHashB)
    , ("bigNatQuotRemWord#",       bigNatQuotRemWordHashB)
    -- Phase 2.C: bit-op primops.  Thin wrappers over host
    -- 'Numeric.Natural''s 'Data.Bits' instance.  Signatures from
    -- @~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs@.
    , ("bigNatAnd",                makeBigNatBinOp "bigNatAnd" (.&.))
    , ("bigNatOr",                 makeBigNatBinOp "bigNatOr"  (.|.))
    , ("bigNatXor",                makeBigNatBinOp "bigNatXor" xor)
    , ("bigNatAndNot",             makeBigNatBinOp "bigNatAndNot" andNotNat)
    , ("bigNatAndWord#",           makeBigNatWordOp "bigNatAndWord#" (.&.))
    , ("bigNatOrWord#",            makeBigNatWordOp "bigNatOrWord#"  (.|.))
    , ("bigNatXorWord#",           makeBigNatWordOp "bigNatXorWord#" xor)
    , ("bigNatAndNotWord#",        makeBigNatWordOp "bigNatAndNotWord#" andNotNat)
    , ("bigNatAndInt#",            bigNatAndIntHashB)
    , ("bigNatShiftL#",            makeBigNatShiftOp "bigNatShiftL#" shiftL)
    , ("bigNatShiftR#",            makeBigNatShiftOp "bigNatShiftR#" shiftR)
    , ("bigNatShiftRNeg#",         bigNatShiftRNegHashB)
    , ("bigNatPopCount#",          bigNatPopCountHashB)
    , ("bigNatTestBit#",           bigNatTestBitHashB)
    , ("bigNatBit#",               bigNatBitHashB)
    -- Phase 2.D: conversion primops.  'bigNatFromWord#' landed in
    -- Phase 1; this tranche covers the remaining basic conversions
    -- plus the Integer<->BigNat# bridge primops from
    -- GHC.Num.Integer (which the source-loaded ghc-bignum 'Integer'
    -- arithmetic uses pervasively).
    --
    -- DEFERRED to a later tranche (state-token / Addr# threading,
    -- unboxed sums, list-based):
    --   bigNatFromWordList / bigNatToWordList         (list-based)
    --   bigNatToWordMaybe# :: BigNat# -> (# (# #) | Word# #)
    --   bigNatToAddr* / bigNatFromAddr* / bigNatTo|FromByteArray* /
    --     bigNatFromWordArray*                         (IO + Addr#)
    , ("bigNatFromWord64#",        bigNatFromWordB)           -- alias on 64-bit
    , ("bigNatEncodeDouble#",      bigNatEncodeDoubleHashB)   -- m * 2^e
    -- Integer <-> BigNat# bridges (from GHC.Num.Integer)
    , ("integerFromBigNat#",       integerFromBigNatHashB)
    , ("integerFromBigNatNeg#",    integerFromBigNatNegHashB)
    , ("integerFromBigNatSign#",   integerFromBigNatSignHashB)
    , ("integerToBigNatClamp#",    integerToBigNatClampHashB)
    -- Logarithms
    -- bigNatLogBaseWord# source-loads from ghc-bignum:
    -- base <= 1 -> unexpectedValue_Word#; base == 2 -> bigNatLog2#;
    -- otherwise bigNatLogBase# (bigNatFromWord# base) a.
    -- Phase 2.E: completion tranche.  The Phase 2 plan listed
    -- 'bigNatShow' / 'bigNatRead' / 'bigNatToHexString' as the show/read
    -- tranche, but none of those primops exist in ghc-bignum source —
    -- show/read for 'Integer' uses character-by-character building
    -- through quotRem, which is composed at the source level over the
    -- arithmetic primops already shipped in Phase 2.B.  Instead this
    -- tranche fills out the remaining basic BigNat# primops: bit-modify,
    -- BigNat#-vs-Word# comparison, count-trailing-zeros, indexing into
    -- limbs, zero/one constants, and digit-count-in-base.
    , ("bigNatClearBit#",          makeBigNatShiftOp "bigNatClearBit#" (\n i -> clearBit n i))
    , ("bigNatSetBit#",            makeBigNatShiftOp "bigNatSetBit#"   (\n i -> setBit n i))
    , ("bigNatComplementBit#",     makeBigNatShiftOp "bigNatComplementBit#" (\n i -> complementBit n i))
    -- bigNatGtWord#, bigNatLeWord#, and bigNatEqWord# source-load from
    -- ghc-bignum; their bodies compose source-loaded BigNat readers over
    -- the retained wordArraySize#/indexWordArray# leaves.
    -- bigNatCompareWord#, bigNatIsTwo#, and bigNatCheck# source-load
    -- from ghc-bignum over retained word-array reader leaves
    -- (wordArraySize#, indexWordArray#, sizeofByteArray#).
    -- bigNatIndex# source-loads directly from ghc-bignum:
    -- bigNatIndex# x i = indexWordArray# x i.
    , ("bigNatZero#",              bigNatZeroHashB)           -- (# #) -> BigNat#
    , ("bigNatOne#",               bigNatOneHashB)            -- (# #) -> BigNat#
    -- bigNatCtz# and bigNatCtzWord# source-load from ghc-bignum;
    -- their loops use indexWordArray# and the GHC.Prim ctz# leaf.
    -- bigNatSizeInBase# source-loads from ghc-bignum:
    -- base <= 1 -> unexpectedValue_Word#; zero -> 0##;
    -- otherwise bigNatLogBaseWord# base a + 1##.
    -- Phase 4: ghc-bignum 'integerMul (IS x) (IS y)' overflow path
    -- constructs BigNats from a high/low Word# pair.  Without this
    -- shim, in-Int64 × in-Int64 → out-of-Int64 multiplications
    -- (e.g. @2 ^ 100 :: Integer@ which repeatedly squares through
    -- the overflow boundary) silently truncated to 0.
    , ("bigNatFromWord2#",         bigNatFromWord2HashB)      -- Word# -> Word# -> BigNat#
    -- Phase 2.8: RealWorld / State primops
    , ("realWorld#",               realWorldB)
    -- seq# :: a -> State# s -> (# State# s, a #) -- GHC.Prim primop.
    -- No .hs source (GHC/PrimopWrappers.hs only re-exports GHC.Prim.seq#);
    -- compiler-intrinsic, qualifies under the carve-out rule. Backs
    -- source-loaded `evaluate a = IO $ \s -> seq# a s` in GHC.Internal.IO.
    , ("seq#",                     seqHashB)
    , ("noDuplicate#",            noDuplicateB)  -- GHC primop: no-op in interpreter
    , ("touch#",                  touchHashB)     -- GHC primop: keep-alive touch, no-op at Val level
    , ("runRW#",                   runRWB)
    -- 'lazy' / 'GHC.Magic.lazy' / 'GHC.Exts.lazy' resolve from source
    -- (ghc-prim GHC.Magic: @lazy x = x@; re-exported via Base / Exts).
    -- Demand-driven re-export discovery materialises the identity body.
    -- 'unsafePerformIO' / 'unsafeDupablePerformIO' graduated to
    -- source-loaded from GHC.Internal.IO.Unsafe:
    --   unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)
    --   unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> lazy a
    -- Bottoms on runRW# / noDuplicate# / source lazy.
    -- matchPat on VIO already reconstructs the State# function so the
    -- @(IO m)@ pattern match works (Eval.hs).
    -- 'accursedUnutterablePerformIO' graduated to source-loaded from
    -- Data.ByteString.Internal.Type:
    --   accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
    -- Phase 2.8: boxing/unboxing constructors
    , ("I#",  iHashB)
    , ("W#",  wHashB)
    , ("W8#", w8HashB)
    , ("C#",  cHashB)
    -- Float / Double boxing constructors.  Source-loaded Num Float /
    -- Num Double instance bodies wrap unboxed primop results with
    -- F# / D# (see plusFloatHashB / plusDoubleHashB).  The runtime
    -- represents both Float and Double as VFloat (Double internally),
    -- so the boxing is a no-op force.
    , ("F#",  fHashB)
    , ("D#",  dHashB)
    -- Phase 2.8: Addr# primitives
    , ("nullAddr#",   nullAddrB)
    , ("plusAddr#",   plusAddrB)
    , ("minusAddr#",  minusAddrB)
    , ("addr2Int#",   addr2IntB)
    , ("indexCharOffAddr#", indexCharOffAddrHashB)
    -- GHC.Prim.indexWord8OffAddr# is a compiler primop with no .hs body.
    -- Source-loaded byte-oriented libraries use it for pure raw-address
    -- reads, so it must bottom out in the interpreter's RTS memory access.
    , ("indexWord8OffAddr#", indexWord8OffAddrHashB)
    -- Addr# comparison primops — RTS-exclusive (Addr# is unboxed; no
    -- source 'Eq Addr#' instance).  Used by the derived
    -- @instance Eq (Ptr a)@ from @data Ptr a = Ptr Addr#@'s synthesis
    -- of @Ptr a == Ptr b = isTrue# (eqAddr# a b)@ once the @==@
    -- builtin shim is dropped.
    , ("eqAddr#",     addrCmpHashB (==))
    , ("neAddr#",     addrCmpHashB (/=))
    , ("ltAddr#",     addrCmpHashB (<))
    , ("leAddr#",     addrCmpHashB (<=))
    , ("gtAddr#",     addrCmpHashB (>))
    , ("geAddr#",     addrCmpHashB (>=))
    -- GHC.Prim-only raw-address access.  Source-loaded
    -- GHC.Internal.Storable defines write*OffPtr/read*OffPtr in terms of
    -- these primops; there is no .hs implementation to interpret below them.
    , ("readIntOffAddr#",  readIntOffAddrHashB)
    , ("writeIntOffAddr#", writeIntOffAddrHashB)
    , ("readWord8OffAddr#",  readWord8OffAddrHashB)
    , ("writeWord8OffAddr#", writeWord8OffAddrHashB)
    , ("readWideCharOffAddr#",  readWideCharOffAddrHashB)
    , ("writeWideCharOffAddr#", writeWideCharOffAddrHashB)
    -- Ptr arithmetic (plusPtr/minusPtr/nullPtr/castPtr) is now
    -- source-loaded from GHC.Internal.Ptr — its bodies bottom on
    -- plusAddr#/minusAddr#/nullAddr#/coerce, all already available.
    -- Phase 2.8: ForeignPtr
    , ("mallocPlainForeignPtrBytes", mallocForeignPtrBytesB)
    , ("mallocForeignPtrBytes",      mallocForeignPtrBytesB)
    -- withForeignPtr / unsafeWithForeignPtr / touchForeignPtr source-load
    -- from GHC.Internal.ForeignPtr. They bottom out in keepAlive# and touch#.
    -- 'plusForeignPtr' / 'minusForeignPtr' source-loaded; the
    -- reconstruction round-trips through 'foreignPtrValToForeignPtr'.
    -- 'newForeignPtr_' graduated to source-loaded from
    -- GHC.Internal.ForeignPtr:
    --   newForeignPtr_ (Ptr obj) = do
    --     r <- newIORef NoFinalizers
    --     return (ForeignPtr obj (PlainForeignPtr r))
    -- Bottoms on newIORef + ForeignPtr/PlainForeignPtr constructors;
    -- foreignPtrValToForeignPtr / withForeignPtr already accept the
    -- VCon "ForeignPtr" shape.
    -- newForeignPtr source-loads from ForeignPtr.Imp:
    -- newForeignPtr finalizer p = newForeignPtr_ p >>= \fp ->
    -- addForeignPtrFinalizer finalizer fp >> pure fp.
    , ("addForeignPtrFinalizer",     addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.Imp.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.Safe.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.Internal.Foreign.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.Internal.Foreign.ForeignPtr.Imp.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    -- Phase 2.8: Storable ops on Ptr.  Qualified module entries source-load
    -- through the Storable class; keep only bare host fallbacks for raw
    -- pointer operations where optimistic execution lacks enough type evidence.
    , ("peek",         peekB)
    , ("poke",         pokeB)
    , ("peekByteOff",  peekByteOffB)
    , ("pokeByteOff",  pokeByteOffB)
    -- Phase 2.8: MutableByteArray# family
    , ("newByteArray#",             newByteArrayB)
    , ("newPinnedByteArray#",       newPinnedByteArrayB)
    , ("newAlignedPinnedByteArray#", newAlignedPinnedByteArrayB)
    , ("writeWord8Array#",          writeWord8ArrayB)
    , ("readWord8Array#",           readWord8ArrayB)
    , ("indexWord8Array#",          indexWord8ArrayB)
    -- GHC.Prim primop, no .hs source.  ghc-bignum's WordArray#
    -- source uses it to read 64-bit limbs from the BigNat# ByteArray#
    -- representation; IHC also supports PrimBigNat here while that
    -- representation is Natural-backed.
    , ("indexWordArray#",           indexWordArrayHashB)
    -- GHC.Prim primop, no .hs source.  ghc-bignum's BigNat# -> Int#
    -- path indexes the low limb through this leaf; the host runtime
    -- uses the same Natural-backed limb access here.
    , ("indexIntArray#",            indexIntArrayHashB)
    , ("unsafeFreezeByteArray#",    unsafeFreezeByteArrayB)
    , ("byteArrayContents#",        byteArrayContentsB)
    , ("mutableByteArrayContents#", mutableByteArrayContentsB)
    , ("getSizeofMutableByteArray#", getSizeofMutableByteArrayB)
    , ("sizeofByteArray#",          sizeofByteArrayB)
    , ("resizeMutableByteArray#",   resizeMutableByteArrayB)
    , ("shrinkMutableByteArray#",   shrinkMutableByteArrayB)
    , ("setByteArray#",             setByteArrayB)
    , ("copyMutableByteArray#",     copyMutableByteArrayB)
    , ("copyByteArray#",            copyByteArrayB)
    , ("copyAddrToByteArray#",      copyAddrToByteArrayB)
    , ("copyByteArrayToAddr#",      copyByteArrayToAddrB)
    , ("compareByteArrays#",        compareByteArraysB)
    -- Boxed Array#/MutableArray# primops used by source-loaded GHC.Arr.
    , ("newArray#",                 newArrayHashB)
    , ("writeArray#",               writeArrayHashB)
    , ("readArray#",                readArrayHashB)
    , ("indexArray#",               indexArrayHashB)
    , ("unsafeFreezeArray#",        unsafeFreezeArrayHashB)
    , ("unsafeThawArray#",          unsafeThawArrayHashB)
    , ("sizeofArray#",              sizeofArrayHashB)
    , ("sizeofMutableArray#",       sizeofMutableArrayHashB)
    -- Phase 2.8: C memory ops
    -- 'memcpy', 'memcpyFp' graduated to source — both are one-liners
    -- in 'Data.ByteString.Internal.Type' that delegate to 'copyBytes'
    -- (now source-loaded itself, see below).
    --
    -- 'copyBytes' / 'moveBytes' / 'fillBytes' graduated to source.
    -- Their source bodies are @coerce $ \... s -> (# primOp ... s, () #)@;
    -- the underlying primops (@copyAddrToAddrNonOverlapping#@,
    -- @copyAddrToAddr#@, @setAddrRange#@) are registered above.
    -- memset / memchr / memcmp / c_strlen retired: bytestring and the
    -- rest of Hackage declare these as
    --   foreign import ccall unsafe "string.h memset" c_memset
    -- (and similarly for memchr/memcmp/strlen).  The generic libffi
    -- dispatcher (src/IHC/FFI.hs) picks them up at scan time; hardcoded
    -- shims are no longer needed to unblock source-loaded bytestring.
    -- Phase 2.8: buffered I/O
    , ("hPutBuf",    hPutBufB)
    -- Phase 2.8: Int/Word coercions + bit ops
    , ("int2Word#",         int2WordB)
    , ("word2Int#",         word2IntB)
    , ("word8ToWord#",      word8ToWordB)
    , ("wordToWord8#",      wordToWord8B)
    , ("word32ToWord#",     word32ToWordB)
    , ("GHC.Prim.word32ToWord#", word32ToWordB)
    , ("wordToWord32#",     wordToWord32B)
    , ("GHC.Prim.wordToWord32#", wordToWord32B)
    , ("setAddrRange#",     setAddrRangeB)
    , ("copyAddrToAddrNonOverlapping#", copyAddrToAddrNonOverlappingB)
    , ("copyAddrToAddr#",   copyAddrToAddrB)
    , ("or#",               orHashB)
    , ("and#",              andHashB)
    , ("xor#",              xorHashB)
    , ("not#",              notHashB)
    , ("+#",                plusIntHashB)
    , ("-#",                minusIntHashB)
    , ("*#",                timesIntHashB)
    , ("<#",                ltIntHashB)
    , ("<=#",               leIntHashB)
    , ("==#",               eqIntHashB)
    , (">#",                gtIntHashB)
    , (">=#",               geIntHashB)
    , ("/=#",               neIntHashB)
    , ("uncheckedShiftL#",  uncheckedShiftLB)
    , ("uncheckedIShiftL#", uncheckedShiftLB)
    , ("uncheckedShiftRL#", uncheckedShiftRLB)
    -- @uncheckedIShiftRL# :: Int# -> Int# -> Int#@ — logical
    -- (zero-fill) right shift of an Int#.  Same bit operation as
    -- 'uncheckedShiftRL#' (reinterpret the Int64 bits as unsigned,
    -- shift right with zero fill); ghc-bignum's @bigNat*@ /
    -- @integer*@ paths use it for limb extraction.
    , ("uncheckedIShiftRL#", uncheckedShiftRLB)
    , ("uncheckedIShiftRA#", uncheckedIShiftRAB)
    -- Bitwise Int# primops: aliased to the boxed bit ops since
    -- IHC represents Int# as VInt (Int64-backed).  Required by
    -- the Int64-encoding helpers in 'GHC.Num.Integer' that
    -- 'integerDecodeDouble#' rides through.
    , ("andI#",             bitAndB)
    , ("orI#",              bitOrB)
    , ("xorI#",             bitXorB)
    -- notI# :: Int# -> Int# — a genuine GHC.Prim primop with NO
    -- .hs source (ghc-prim's GHC/PrimopWrappers.hs only re-exports
    -- @GHC.Prim.notI#@), so it qualifies as a compiler-intrinsic
    -- carve-out under the builtins minimum-surface rule.  Two
    -- source-loaded leaves bottom on it: @divModInt#@
    -- (GHC/Classes.hs:846, ridden by the graduated @divMod@) and
    -- @complement (I# x#) = I# (notI# x#)@ in @instance Bits Int@
    -- (GHC.Internal.Bits, Bits.hs:461, ridden by the graduated
    -- @complement@).  Aliased to the boxed complement since IHC
    -- represents Int# as 'VInt' (Int64-backed).
    , ("notI#",             notIB)
    -- Int64# primops: IHC stores both Int# and Int64# as
    -- 'VInt' (Int64-backed Haskell), so conversions are
    -- identity functions and arithmetic dispatches to the
    -- regular Int# implementations.  Required by
    -- 'GHC.Num.Integer.integerFromInt64#' and friends, which
    -- the source-loaded 'integerDecodeDouble#' uses to lift
    -- the @Int64#@ mantissa returned by 'decodeDouble_Int64#'
    -- back into 'Integer'.
    , ("leInt64#",          leIntHashB)
    , ("ltInt64#",          ltIntHashB)
    , ("eqInt64#",          eqIntHashB)
    , ("geInt64#",          geIntHashB)
    , ("gtInt64#",          gtIntHashB)
    , ("neInt64#",          neIntHashB)
    , ("plusInt64#",        plusIntHashB)
    , ("minusInt64#",       minusIntHashB)
    , ("timesInt64#",       timesIntHashB)
    , ("negateInt64#",      negateIntB)
    , ("intToInt64#",       identityIntPrimop)
    , ("int64ToInt#",       identityIntPrimop)
    , ("int64ToWord64#",    identityIntPrimop)
    , ("word64ToInt64#",    identityIntPrimop)
    -- 64-bit word-size compatibility aliases from source-loaded
    -- Data.Memory.Internal.CompatPrim64 / ghc-bignum. These are
    -- representation no-ops on our target, so a builtin leaf keeps
    -- the source-loaded path honest without reintroducing a BigNat shim.
    , ("wordToWord64#",     identityIntPrimop)
    , ("word64ToWord#",     identityIntPrimop)
    , ("timesInt2#",        timesInt2B)
    , ("timesWord2#",       timesWord2B)
    , ("ltChar#",           ltCharHashB)
    , ("leChar#",           leCharHashB)
    , ("eqChar#",           eqCharHashB)
    , ("gtChar#",           gtCharHashB)
    , ("geChar#",           geCharHashB)
    , ("neChar#",           neCharHashB)
    -- Phase 2.8: GHC.Exts Word# comparison primops (for containers)
    , ("ltWord#",   ltWordB)
    , ("leWord#",   leWordB)
    , ("eqWord#",   eqWordB)
    , ("gtWord#",   gtWordB)
    , ("geWord#",   geWordB)
    -- Word8# comparison primops are compiler intrinsics used by
    -- source-loaded GHC.Internal.Word and text's UTF-8 paths.
    , ("ltWord8#",  ltWord8B)
    , ("leWord8#",  leWord8B)
    , ("eqWord8#",  eqWord8B)
    , ("gtWord8#",  gtWord8B)
    , ("geWord8#",  geWord8B)
    , ("neWord8#",  neWord8B)
    -- Word8# arithmetic (Num Word8 bottoms). Results stay boxed as
    -- VCon "W8#" so typeTagOf → Word8.
    , ("plusWord8#",  plusWord8B)
    , ("subWord8#",   subWord8B)
    , ("timesWord8#", timesWord8B)
    , ("minusWord#", minusWordB)
    , ("plusWord#",  plusWordB)
    , ("timesWord#", timesWordB)
    , ("quotWord#",  quotWordB)
    , ("remWord#",   remWordB)
    , ("popCnt#",    popCntB)
    -- GHC.Prim primop, no .hs source.  Required by source-loaded
    -- ghc-bignum word/BigNat trailing-zero helpers.
    , ("ctz#",       ctzHashB)
    , ("clz#",       clzHashB)
    -- Sized WordN# bit-count primops (GHC.Prim, no .hs source).
    -- Word32 FiniteBits / Bits bottom out on these via
    -- @countLeadingZeros (W32# x) = clz32# (word32ToWord# x)@ etc.
    -- Without them warp's chunked encoder (F.countLeadingZeros /
    -- F.unsafeShiftR on Word32 chunk lengths) dies mid-response.
    , ("popCnt8#",   popCntWidthB 8)
    , ("popCnt16#",  popCntWidthB 16)
    , ("popCnt32#",  popCntWidthB 32)
    , ("popCnt64#",  popCntWidthB 64)
    , ("clz8#",      clzWidthB 8)
    , ("clz16#",     clzWidthB 16)
    , ("clz32#",     clzWidthB 32)
    , ("clz64#",     clzWidthB 64)
    , ("ctz8#",      ctzWidthB 8)
    , ("ctz16#",     ctzWidthB 16)
    , ("ctz32#",     ctzWidthB 32)
    , ("ctz64#",     ctzWidthB 64)
    , ("GHC.Prim.popCnt8#",  popCntWidthB 8)
    , ("GHC.Prim.popCnt16#", popCntWidthB 16)
    , ("GHC.Prim.popCnt32#", popCntWidthB 32)
    , ("GHC.Prim.popCnt64#", popCntWidthB 64)
    , ("GHC.Prim.clz8#",     clzWidthB 8)
    , ("GHC.Prim.clz16#",    clzWidthB 16)
    , ("GHC.Prim.clz32#",    clzWidthB 32)
    , ("GHC.Prim.clz64#",    clzWidthB 64)
    , ("GHC.Prim.ctz8#",     ctzWidthB 8)
    , ("GHC.Prim.ctz16#",    ctzWidthB 16)
    , ("GHC.Prim.ctz32#",    ctzWidthB 32)
    , ("GHC.Prim.ctz64#",    ctzWidthB 64)
    , ("indexOfTheOnlyBit#", indexOfTheOnlyBitB)
    -- Phase 2.8: Int# arithmetic primops
    , ("negateInt#",   negateIntB)
    , ("quotInt#",     quotIntB)
    , ("remInt#",      remIntB)
    , ("quotRemInt#",  quotRemIntB)
    , ("addIntC#",     addIntCB)
    , ("subIntC#",     subIntCB)
    , ("mulIntMayOflo#", mulIntMayOfloB)
    -- Int# division primops (GHC.Prim, no .hs source).
    -- Backing source-loaded Integral Int.{div,mod} which route
    -- through divInt/modInt (in GHC.Internal.Base) and bottom on
    -- these.  Carried in the same family as quotInt#/remInt# above.
    , ("divInt#",      divIntHashB)
    , ("modInt#",      modIntHashB)
    -- Float# arithmetic primops (GHC.Prim, no .hs source).
    -- Backing source-loaded Num Float / Fractional Float instances:
    --   instance Num Float where (+) x y = plusFloat x y
    --   plusFloat (F# x) (F# y) = F# (plusFloat# x y)
    -- (Eval.hs:matchPat wires F# to expose VFloat through the unwrap.)
    , ("plusFloat#",   plusFloatHashB)
    , ("minusFloat#",  minusFloatHashB)
    , ("timesFloat#",  timesFloatHashB)
    , ("divideFloat#", divideFloatHashB)
    -- Double# arithmetic primops (GHC.Prim, no .hs source).
    -- Backing source-loaded Num Double / Fractional Double instances:
    --   instance Num Double where (+) x y = plusDouble x y
    --   plusDouble (D# x) (D# y) = D# (x +## y)
    , ("+##",          plusDoubleHashB)
    , ("-##",          minusDoubleHashB)
    , ("*##",          timesDoubleHashB)
    , ("/##",          divideDoubleHashB)
    -- Power primops (GHC.Prim, no .hs source — carve-out, same class
    -- as the +##/-##/*##//## intrinsics above).  Backing the
    -- source-loaded Floating instances graduated when the (^)/(^^)/(**)
    -- builtins were removed:
    --   instance Floating Double where (**) x y = powerDouble x y
    --   powerDouble (D# x) (D# y) = D# (x **## y)        (Float.hs:1594)
    --   instance Floating Float  where (**) x y = powerFloat x y
    --   powerFloat (F# x) (F# y) = F# (powerFloat# x y)  (Float.hs:1542)
    -- @powerFloat#@'s only "source" is the GHC.PrimopWrappers.hs
    -- pass-through @powerFloat# a1 a2 = GHC.Prim.powerFloat# a1 a2@;
    -- @**##@ has no wrapper at all.  Both are compiler intrinsics.
    , ("**##",         powerDoubleHashB)
    , ("powerFloat#",  powerFloatHashB)
    -- Phase 5: Double# comparison primops (==##, <##, etc.).
    -- These are needed by source-loaded Eq Double / Ord Double bodies
    -- and by 'floorDouble' / 'ceilingDouble' (which compare r < 0.0).
    , ("==##",         makeDoubleCmpOp "==##" (==))
    , ("/=##",         makeDoubleCmpOp "/=##" (/=))
    , ("<##",          makeDoubleCmpOp "<##"  (<))
    , ("<=##",         makeDoubleCmpOp "<=##" (<=))
    , (">##",          makeDoubleCmpOp ">##"  (>))
    , (">=##",         makeDoubleCmpOp ">=##" (>=))
    -- Phase 5: Double# unary primops (needed by 'roundDouble' /
    -- 'truncateDouble' which use 'abs' and 'compare' on Doubles).
    , ("fabsDouble#",  unaryOpFloat abs)
    , ("negateFloat#", unaryOpFloat negate)
    -- Builtins-removal carve-out: @sqrtDouble# :: Double# -> Double#@
    -- is a genuine GHC.Prim primop with NO .hs source (ghc-prim
    -- PrimopWrappers.hs:841 just re-exports @GHC.Prim.sqrtDouble#@), so
    -- it qualifies as a compiler-intrinsic builtin under the
    -- "Builtin modules: minimum surface only" carve-out rule. It is the
    -- bottom of the source-loaded @Floating Double.sqrt@ chain
    -- (Float.hs:746 @sqrt x = sqrtDouble x@; Float.hs:1578
    -- @sqrtDouble (D# x) = D# (sqrtDouble# x)@) now that the bare-name
    -- @sqrt@ shim is gone.  Reuses the shared 'unaryOpFloat' helper.
    , ("sqrtDouble#",  unaryOpFloat sqrt)
    -- Floating Double bottoms (Float.hs:1574–1591) — all GHC.Prim
    -- intrinsics with no .hs source.  packIntegral / logBase need
    -- logDouble#; register the full Floating Double unary suite so
    -- the next missing primop does not re-block warp response packing.
    , ("expDouble#",   unaryOpFloat exp)
    , ("expm1Double#", unaryOpFloat (\x -> exp x - 1))
    , ("logDouble#",   unaryOpFloat log)
    , ("log1pDouble#", unaryOpFloat (\x -> log (1 + x)))
    , ("sinDouble#",   unaryOpFloat sin)
    , ("cosDouble#",   unaryOpFloat cos)
    , ("tanDouble#",   unaryOpFloat tan)
    , ("asinDouble#",  unaryOpFloat asin)
    , ("acosDouble#",  unaryOpFloat acos)
    , ("atanDouble#",  unaryOpFloat atan)
    , ("sinhDouble#",  unaryOpFloat sinh)
    , ("coshDouble#",  unaryOpFloat cosh)
    , ("tanhDouble#",  unaryOpFloat tanh)
    , ("asinhDouble#", unaryOpFloat asinh)
    , ("acoshDouble#", unaryOpFloat acosh)
    , ("atanhDouble#", unaryOpFloat atanh)
    -- @decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)@.
    -- Bottom-of-stack primop for 'decodeFloat' on Double:
    -- 'GHC.Num.Integer.integerDecodeDouble#' wraps it
    -- (ghc-bignum-1.3/src/GHC/Num/Integer.hs:1046).  Required by
    -- the source-loaded RealFrac Double / properFractionFloat
    -- chain, which 'floor'\/'ceiling'\/'round'\/'truncate' ride.
    , ("decodeDouble_Int64#", decodeDoubleInt64HashB)
    -- Phase 5: Int#/Double# conversion primops needed by source-loaded
    -- @RealFloat Double.encodeFloat@ (which calls
    -- @integerEncodeDouble#@ → @intEncodeDouble#@ / @int2Double#@).
    , ("int2Double#",         int2DoubleHashB)
    , ("intEncodeDouble#",    intEncodeDoubleHashB)
    , ("negateDouble#",       negateDoubleHashB)
    , ("double2Int#",         double2IntHashB)
    , ("double2Float#",       double2FloatHashB)
    , ("float2Double#",       float2DoubleHashB)
    -- GHC.CString now source-loads from ghc-prim. Its Haskell bodies
    -- bottom out on the Addr#/Char# primops above plus the scanned
    -- foreign import ccall "strlen" leaf, so the high-level
    -- cstringLength#/unpackCString# shims do not belong here.
    -- Phase 2.8: misc
    -- Foreign.C.String shortcuts — the locale-aware source bodies reach
    -- for RTS locale state via getForeignEncoding; keep only those host
    -- shims.  The CAString variants are byte-wise Haskell and source-load.
    -- withCStringLen0 is NOT registered: real source is
    --   withCStringLen0 :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
    -- (Encoding.hs) — a 3-arg encoding-first API.  A 2-arg host alias
    -- to withCStringLen would shadow the correct source arity.
    , ("withCString",     withCStringB)
    , ("withCStringLen",  withCStringLenB)
    -- Foreign.Marshal.Utils.with source-loads from
    -- GHC.Internal.Foreign.Marshal.Utils:
    -- with val f = alloca $ \ptr -> poke ptr val >> f ptr.
    , ("peekCString",     peekCStringB)
    , ("newCString",      newCStringB)
    -- Foreign.Storable.sizeOf / alignment source-load for qualified module
    -- entries.  Keep only the bare optimistic fallback for polymorphic
    -- library code like alloca's sizeOf (undefined :: a), where IHC has no
    -- typechecker dictionary to recover a concrete Storable instance.
    , ("sizeOf",       sizeOfB)
    , ("alignment",    alignmentB)
    -- Network.Socket.Syscall.socket source-loads; its socket(2) foreign
    -- import dispatches through the generic FFI path.
    -- Network.Socket.Options.setSocketOption source-loads; it bottoms out on
    -- lower setSockOpt foreign imports.
    -- Network.Socket.Syscall.listen source-loads; its listen(2) foreign
    -- import dispatches through the generic FFI path.
    -- Network.Socket.Syscall.accept source-loads; its accept(2) foreign
    -- import dispatches through the generic FFI path.
    -- Network.Socket.Syscall.connect source-loads; its connect(2) foreign
    -- import dispatches through the generic FFI path.
    -- Network.Socket.Name.getSocketName source-loads; its getsockname(2)
    -- foreign import dispatches through the generic FFI path.
    -- Network.Socket.Syscall.bind source-loads; its bind(2) foreign import
    -- dispatches through the generic FFI path.
    -- Network.Socket.Types.close / close' source-load; the actual close(2)
    -- happens through lower foreign imports / closeFdWith.
    -- Network.Socket.Types.withFdSocket source-loads over the Socket IORef.
    -- Network.Socket.Types.fdSocket / unsafeFdSocket source-load over the
    -- same Socket IORef representation.
    -- Network.Socket.Buffer.sendBuf / recvBuf source-load; their foreign
    -- imports dispatch through the generic FFI path.
    -- Phase C.3 (builtins-removal): the @Settings@ field accessors
    -- (settingsPort/Host/Timeout/FdCacheDuration/FileInfoCacheDuration)
    -- used to live here as positional shims that indexed into a host-
    -- constructed VCon.  They were removed once defaultSettings became
    -- source-loaded via Scheduler.preludeDirectOwner: the loaded module
    -- registers all Settings fields in lmFieldReg, and tryFieldSlot
    -- synthesises the accessors automatically.  Helpers warpSettings*B
    -- and settingsFieldB went with them.
    -- Network.Socket.Info.getAddrInfo source-loads; its getaddrinfo(3)
    -- foreign imports dispatch through the generic FFI path.
    -- Phase 2.8: additional numeric ops needed by containers (graduated)
    --   * 'fromInteger' (Num class) → 'Num Int.fromInteger'
    --     at GHC/Internal/Num.hs:115 — body @fromInteger i = I# (integerToInt# i)@.
    --     The IS/IP/IN matchPat bridge in Eval.hs (PR #136) lets the
    --     source-loaded @integerToInt# (IS i) = i@ unwrap a 'VInt'.
    --   * 'toInteger' (Integral class) → 'Integral Int.toInteger'
    --     at GHC/Internal/Real.hs:442 — body @toInteger (I# i) = IS i@.
    --     Returns 'VCon "IS" [VInt n]'; downstream consumers either
    --     pattern-match through the IS bridge or dispatch on the
    --     normalized Integer tag in 'IHC.Classes.typeTagOf'.
    --   * 'quot' / 'rem' (Integral class) → 'Integral Int.{quot,rem}'
    --     at Real.hs:445-455, routed through quotInt / remInt
    --     (Base.hs:2376-2390) bottoming on quotInt# / remInt# primops.
    -- 'div' graduated with the rest of the TODO 2.6 block: it now
    -- routes through the Integral Int instance (a `divInt` b) and
    -- bottoms on divInt# registered below.
    --   * 'divMod' / 'quotRem' (Integral class) graduated (builtins
    --     minimum-surface): the @divModB@ / @quotRemB@ shims were
    --     dropped.  They now source-load through @Integral Int@ in
    --     GHC/Internal/Real.hs:471-482, which routes
    --     @a \`quotRem\` b@ → 'quotRemInt' / @a \`divMod\` b@ →
    --     'divModInt' (Base.hs:2428,2444).  'quotRemInt' bottoms on
    --     the 'quotRemInt#' primop (registered below); 'divModInt'
    --     bottoms on 'divModInt#', which is *itself* source-loaded
    --     from ghc-prim's GHC/Classes.hs:840 (it has real .hs source,
    --     so per the doctrine it is interpreted, not shimmed) and in
    --     turn rides 'quotRemInt#' plus the 'notI#' GHC.Prim primop
    --     carve-out registered below.  The class-method dispatch is
    --     seeded via @("divMod","Integral")@ / @("quotRem","Integral")@
    --     in 'IHC.TypeGlobals.seedBuiltinClassMethodSigs'.
    -- NOTE (Bits bitwise core): the @class Bits@ methods
    --   shiftL / shiftR / .&. / .|. / xor / complement
    -- are no longer shimmed.  They source-load from the
    -- @instance Bits Int@ in @GHC.Internal.Bits@ (Bits.hs:444),
    -- which bottoms on the @andI# / orI# / xorI# / notI# /
    -- iShiftL# / iShiftRA#@ primops (the first three registered
    -- below; @iShiftL#@/@iShiftRA#@ source-load from Base.hs and
    -- ride @uncheckedIShiftL#@/@uncheckedIShiftRA#@; @notI#@ is
    -- the GHC.Prim carve-out registered below).  Method→class
    -- seeds live in 'IHC.TypeGlobals.seedBuiltinClassMethodSigs'.
    -- popCount / bit / testBit / clearBit / setBit removed per CLAUDE.md
    -- "Builtin modules: minimum surface only".  These are @class Bits@
    -- methods (GHC.Internal.Bits); the @Bits Int@ instance + class
    -- defaults express them via shifts, @.&.@, @.|.@, @complement@, and
    -- the @popCnt#@ primop (registered below).  Resolution now flows
    -- through the source-loaded @class Bits@ via the env-fallback's
    -- 'tryClassMethodFromRegistry' → 'classMethodDispatcher', seeded by
    -- @("popCount"/"bit"/"testBit"/"clearBit"/"setBit","Bits")@ in
    -- 'IHC.TypeGlobals.seedBuiltinClassMethodSigs'.
    -- Power operators graduated to source-loaded:
    --   (^)  / (^^) are top-level functions in GHC.Internal.Real
    --     (^)  :: (Num a, Integral b)        => a -> b -> a   (Real.hs:744)
    --     (^^) :: (Fractional a, Integral b) => a -> b -> a   (Real.hs:772)
    --   They recurse via (*), `quot`, recip, negate — all already
    --   graduated — so they resolve through the env-fallback EVar path
    --   with no class-method seed.
    --   (**) is a Floating class method, seeded @("**","Floating")@ in
    --   TypeGlobals.seedBuiltinClassMethodSigs and routed via
    --   tryClassMethodFromRegistry → classMethodDispatcher.  Its
    --   Double#/Float# power primops (@**##@ / @powerFloat#@) are
    --   GHC.Prim intrinsics — registered as carve-outs below near the
    --   Double# arithmetic primops (full chain documented there).
    -- Phase 2.10a: concurrency primitives.  forkIO / forkIOWithUnmask
    -- are deliberately omitted: GHC.Internal.Conc.Sync has real source and
    -- lowers them to unIO plus the fork# primop below.
    -- Compiler-intrinsic 'fork#' primop. ghc-prim has no .hs source;
    -- forkIO and warp's defaultFork bottom out into this.
    , ("fork#",           forkHashB)
    , ("GHC.Prim.fork#",  forkHashB)
    -- throwTo / killThread / myThreadId are deliberately omitted:
    -- GHC.Internal.Conc.Sync has real source and lowers them to
    -- killThread# / myThreadId# plus the source ThreadId constructor.
    , ("killThread#",     killThreadHashB)
    , ("GHC.Prim.killThread#", killThreadHashB)
    , ("myThreadId#",     myThreadIdHashB)
    -- labelThread / labelThreadByteArray# source-load from
    -- GHC.Internal.Conc.Sync. They bottom out in the raw labelThread#
    -- primop, which is eventlog/debug metadata only in IHC.
    , ("labelThread#", labelThreadHashB)
    , ("GHC.Prim.labelThread#", labelThreadHashB)
    -- threadDelay source-loads from GHC.Internal.Conc.IO/POSIX and bottoms
    -- out in the raw delay# primop when the source-side RTS event-manager path
    -- is not selected.
    , ("delay#", delayHashB)
    , ("GHC.Prim.delay#", delayHashB)
    -- closeFdWith coordinates with GHC's RTS event manager. IHC does not
    -- run that manager, so the compatible behavior is to run the supplied
    -- low-level close action directly.
    , ("closeFdWith", closeFdWithB)
    , ("GHC.Conc.closeFdWith", closeFdWithB)
    , ("GHC.Internal.Conc.IO.closeFdWith", closeFdWithB)
    , ("GHC.Internal.Event.Thread.closeFdWith", closeFdWithB)
    -- threadWaitRead / threadWaitWrite: delegate to host RTS.  Needed by
    -- Network.Socket's async I/O path and warp's connection handling.
    , ("threadWaitRead",  threadWaitReadB)
    , ("GHC.Conc.threadWaitRead", threadWaitReadB)
    , ("GHC.Conc.IO.threadWaitRead", threadWaitReadB)
    , ("GHC.Internal.Conc.IO.threadWaitRead", threadWaitReadB)
    , ("threadWaitWrite", threadWaitWriteB)
    , ("GHC.Conc.threadWaitWrite", threadWaitWriteB)
    , ("GHC.Conc.IO.threadWaitWrite", threadWaitWriteB)
    , ("GHC.Internal.Conc.IO.threadWaitWrite", threadWaitWriteB)
    -- threadWaitReadSTM / threadWaitWriteSTM: warp makeGracefulRecv waits
    -- via @atomically (checkShutdown <|> sockWait)@.  Source path bottoms
    -- out in Event.registerFd with a nested Fd/CInt that IHC often leaves
    -- as a VFun shell ("Fd payload not VInt: <function>").  Host-back the
    -- STM wait pair directly so cancellable recv works.
    , ("threadWaitReadSTM", threadWaitReadSTMB)
    , ("GHC.Conc.threadWaitReadSTM", threadWaitReadSTMB)
    , ("GHC.Conc.IO.threadWaitReadSTM", threadWaitReadSTMB)
    , ("GHC.Internal.Conc.IO.threadWaitReadSTM", threadWaitReadSTMB)
    , ("GHC.Internal.Event.Thread.threadWaitReadSTM", threadWaitReadSTMB)
    , ("threadWaitWriteSTM", threadWaitWriteSTMB)
    , ("GHC.Conc.threadWaitWriteSTM", threadWaitWriteSTMB)
    , ("GHC.Conc.IO.threadWaitWriteSTM", threadWaitWriteSTMB)
    , ("GHC.Internal.Conc.IO.threadWaitWriteSTM", threadWaitWriteSTMB)
    , ("GHC.Internal.Event.Thread.threadWaitWriteSTM", threadWaitWriteSTMB)
    -- Network.Socket.STM.waitReadSocketSTM / waitWriteSocketSTM graduated
    -- to pure source (network package).  Bodies are ordinary wrappers:
    --   waitReadSocketSTM s = fst <$> waitAndCancelReadSocketSTM s
    --   waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd
    -- (and the Write twin).  Per CLAUDE.md rule 4, no Hackage shims.
    -- Bottoms on host-backed threadWaitReadSTM / threadWaitWriteSTM above
    -- once withFdSocket peels the Socket IORef to a CInt/Fd.
    -- GHC.Internal.Event.Thread.threadWait evt fd — the single choke point the
    -- threaded socket-wait path (threadWaitRead/Write -> Event.threadWaitRead
    -- -> threadWait) reduces to.  The source body registers on the RTS event
    -- manager IHC never runs; host-back it to the host RTS IO manager directly.
    , ("threadWait", threadWaitB)
    , ("GHC.Internal.Event.Thread.threadWait", threadWaitB)
    -- GHC.Internal.Event.Manager.registerFd / unregisterFd_ — the event-manager
    -- registration both threadWait (IO) and threadWaitSTM (the cancellable STM
    -- wait network/warp use for recv) funnel through after getSystemEventManager_.
    -- IHC does not run the RTS event manager; emulate a OneShot registration on
    -- the host RTS IO manager (see 'registerFdB').
    , ("registerFd", registerFdB)
    , ("GHC.Internal.Event.Manager.registerFd", registerFdB)
    , ("unregisterFd_", unregisterFd_B)
    , ("GHC.Internal.Event.Manager.unregisterFd_", unregisterFd_B)
    -- IHC does not run GHC's RTS event manager.  We return a stub 'Just mgr'
    -- (see 'getSystemEventManagerB') so the threaded socket-wait path
    -- (threadWait / threadWaitSTM) proceeds via our host-backed registerFd.
    -- Code that branches on this probe (e.g. auto-update's mkAutoUpdate) then
    -- takes the event-manager backend, which routes through the equally
    -- host-backed timer manager (getSystemTimerManager / registerTimeout).
    , ("GHC.Internal.Event.getSystemEventManager", getSystemEventManagerB)
    , ("GHC.Internal.Event.Thread.getSystemEventManager", getSystemEventManagerB)
    -- IHC also does not run GHC's RTS timer manager.  Warp/time-manager use
    -- these operations only to register idle timeout callbacks; expose a
    -- no-op timer manager so request handling can proceed without evaluating
    -- the RTS-only TimerManager implementation.
    , ("GHC.Internal.Event.getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Internal.Event.Thread.getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Internal.Event.registerTimeout", registerTimeoutB)
    , ("GHC.Internal.Event.TimerManager.registerTimeout", registerTimeoutB)
    , ("GHC.Internal.Event.unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Internal.Event.TimerManager.unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Internal.Event.updateTimeout", updateTimeoutB)
    , ("GHC.Internal.Event.TimerManager.updateTimeout", updateTimeoutB)
    -- System.TimeManager.withHandle / withHandleKillThread source-load:
    -- the package definitions are ordinary bracket/register wrappers. The
    -- RTS-only boundary remains the timer manager operations above.
    -- System.TimeManager.initialize / stopManager source-load: upstream
    -- definitions are ordinary Haskell wrappers around the Manager newtype.
    -- System.Posix.IO.setFdOption source-loads from unix and bottoms out in
    -- System.Posix.Internals.c_fcntl_read/write foreign imports.
    -- MVar wrappers are deliberately omitted. Control.Concurrent.MVar,
    -- GHC.MVar, GHC.Internal.MVar, and GHC.Internal.Control.Concurrent.MVar
    -- have real source; they lower to the MVar# primops below.
    -- STM/TVar wrappers are deliberately omitted. GHC.Internal.Conc.Sync
    -- and the public Control.Concurrent.STM re-exports have real source;
    -- they lower to the STM/TVar# primops below.
    -- Phase 2.10a: exceptions
    -- throwIO / throw are deliberately omitted. GHC.Internal.IO.throwIO
    -- and GHC.Internal.Exception.throw have real source; they lower to
    -- toExceptionWithBacktrace plus the raiseIO# / raise# primops below.
    -- GHC primops from GHC.Prim: compiler-intrinsic, no Haskell source.
    -- Source-loaded `error`, `throw`, `undefined`, `head []`, numeric
    -- overflow paths etc. all bottom out into these. See commit message
    -- / CLAUDE.md builtin rule: compiler-built + RTS-exclusive.
    , ("raise#",          raiseHashB)
    , ("raiseIO#",        raiseIOHashB)
    , ("raiseDivZero#",   raiseDivZeroB)
    , ("raiseOverflow#",  raiseOverflowB)
    , ("raiseUnderflow#", raiseUnderflowB)
    -- catch#: GHC.Prim primop with no Haskell source. Backbone of
    -- Control.Exception.catch source:
    --   catch (IO io) h = IO $ catch# io handler'
    -- Takes the IO action (State# -> (# State#, a #)), the handler (exc ->
    -- State# -> (# State#, a #)), and the state token; runs the action and,
    -- on IhcException, invokes the handler with the exception value.
    -- Compiler-intrinsic / RTS-exclusive per CLAUDE.md.
    , ("catch#",           catchHashB)
    -- newMVar# / takeMVar# / putMVar# / readMVar#: GHC.Prim primops with no
    -- Haskell source. Source-loaded GHC.MVar operations bottom out into
    -- these. The RTS provides the underlying synchronisation machinery; we
    -- thread it through the host's Control.Concurrent.MVar.
    , ("newMVar#",         newMVarHashB)
    , ("takeMVar#",        takeMVarHashB)
    , ("putMVar#",         putMVarHashB)
    , ("readMVar#",        readMVarHashB)
    , ("tryTakeMVar#",     tryTakeMVarHashB)
    , ("tryPutMVar#",      tryPutMVarHashB)
    , ("tryReadMVar#",     tryReadMVarHashB)
    , ("isEmptyMVar#",     isEmptyMVarHashB)
    -- STM primops: GHC.Prim, compiler-intrinsic, no Haskell source.
    -- Source-loaded GHC.Conc.Sync wrappers (atomically/newTVar/readTVar/
    -- writeTVar/retry/catchSTM/orElse) bottom out into these. The RTS owns
    -- the transactional scheduler; our interpreter is single-threaded at
    -- the eval level so STM collapses cleanly onto IO (mirroring the
    -- ST s a ≈ IO a bridge, commit 1ed2881). Compiler-intrinsic +
    -- RTS-exclusive per CLAUDE.md.
    , ("atomically#",      atomicallyHashB)
    , ("retry#",           retryHashB)
    , ("catchRetry#",      catchRetryHashB)
    , ("catchSTM#",        catchSTMHashB)
    , ("newTVar#",         newTVarHashB)
    , ("readTVar#",        readTVarHashB)
    , ("readTVarIO#",      readTVarIOHashB)
    , ("writeTVar#",       writeTVarHashB)
    -- keepAlive#: GHC.Prim primop with no Haskell source. Used by
    -- Foreign.ForeignPtr.withForeignPtr to keep a ForeignPtr live across the
    -- body. Signature is `a -> State# s -> (State# s -> b) -> b`; we cannot
    -- express the GC-reachability guarantee in the interpreter, but we can
    -- faithfully apply the continuation to the state — which is all the
    -- source-loaded ForeignPtr machinery needs at the Val level (the actual
    -- lifetime is managed by host GC via PrimForeignPtr).
    , ("keepAlive#",       keepAliveHashB)
    -- Async-exception masking primops. GHC.Prim, no Haskell source. In the
    -- interpreter we don't deliver async exceptions through the mask
    -- machinery, so the three state-token wrappers are identity on the IO
    -- action and getMaskingState# always returns 0 (Unmasked).
    , ("getMaskingState#",      getMaskingStateHashB)
    , ("maskAsyncExceptions#",  maskAsyncExceptionsHashB)
    , ("maskUninterruptible#",  maskAsyncExceptionsHashB)
    , ("unmaskAsyncExceptions#", maskAsyncExceptionsHashB)
    -- unsafeCoerce / unsafeCoerce#: compiler-intrinsic. The Unsafe.Coerce
    -- source defines these in terms of `unsafeEqualityProof`, whose
    -- recursive body is rewritten by GHC's CoreToStg.Prep pass to
    -- `UnsafeRefl` — without that rewrite the source loops. See Note
    -- [Implementing unsafeCoerce] (U5) in base's Unsafe/Coerce.hs.
    -- At the Val level there is no static type to violate, so the primop
    -- is the identity on Val (same pattern as lazy, I#, W#, C#).
    , ("unsafeCoerce",    unsafeCoerceB)
    , ("unsafeCoerce#",   unsafeCoerceB)
    , ("unsafeCoerceUnlifted", unsafeCoerceB)
    -- Compiler magic consumed by the source implementation of eqTypeRep and
    -- unsafeCoerce. GHC replaces its deliberately recursive source binding
    -- with UnsafeRefl during CoreToStg.Prep; the interpreter performs that
    -- compiler rewrite at the builtin boundary.
    , ("unsafeEqualityProof", pure (VCon "UnsafeRefl" []))
    , ("GHC.Internal.Unsafe.Coerce.unsafeEqualityProof"
      , pure (VCon "UnsafeRefl" []))
    , ("unsafeCoerceAddr", unsafeCoerceB)
    -- coerce: GHC.Prim primop, re-exported by Data.Coerce.  No Haskell
    -- source: GHC resolves the type-safe @Coercible@ constraint at
    -- compile time and erases the call.  At the Val level there are no
    -- runtime types to coerce between (same-representation is vacuously
    -- true), so the primop is the identity on Val — same shape as
    -- unsafeCoerce.  Used by Data.Functor.Utils's @(#.)@ composition
    -- operator, transitively reached through Data.Foldable's default
    -- 'foldr' body, which every source-loaded Foldable instance we
    -- rely on.
    , ("coerce",          unsafeCoerceB)
    -- toExceptionWithBacktrace: lives in GHC.Internal.Exception. Source
    -- exists there but wiring the ghc-internal package through import
    -- resolution is a separate project; source-loaded throwIO/throw still
    -- calls it by name. We shim it as an IO action that wraps the value in
    -- SomeException (dropping the actual CallStack-capture — the backtrace
    -- is cosmetic at the Val level, and our `extractExceptionMessage`
    -- already understands the SomeException wrapper).
    , ("toExceptionWithBacktrace", toExceptionWithBacktraceB)
    -- toException: class method of Exception. Source-loaded throwIO
    -- chain also reaches this via `throwIO e = IO (raiseIO# (toException e))`.
    -- In the Val world we have no type-driven dispatch, so identity-with-
    -- SomeException-wrap is fine (same contract as toExceptionWithBacktrace).
    , ("toException",     toExceptionB)
    , ("Control.Exception.toException", toExceptionB)
    , ("GHC.Internal.Control.Exception.toException", toExceptionB)
    , ("GHC.Internal.Exception.toException", toExceptionB)
    -- fromException: pair of toException. Used by source-loaded catch:
    --   handler' e = case fromException e of Just e' -> h e'; Nothing -> raiseIO# e
    -- With Val-level dynamic typing we return the wrapper in `Just`;
    -- matchPat handles concrete constructor demands through the
    -- SomeException wrapper and lets failed downcast guards fall through
    -- to `Nothing`.
    , ("fromException",   fromExceptionB)
    , ("Control.Exception.fromException", fromExceptionB)
    , ("GHC.Internal.Control.Exception.fromException", fromExceptionB)
    , ("GHC.Internal.Exception.fromException", fromExceptionB)
    -- =================================================================
    -- VIO <-> State# bridge -- RTS-exclusive
    --
    -- IHC's runtime represents IO as VIO (a host IO action that reduces
    -- to Val).  Source-level GHC defines `newtype IO a = IO (State#
    -- RealWorld -> (# State# RealWorld, a #))` -- a state-transformer
    -- over an unboxed-tuple result.  The two shapes are not
    -- interconvertible in Haskell source: there is no userland term
    -- that can coerce between a host IO action and a function consuming
    -- a State# token (the unboxed-tuple constructor `(#,#)` is a
    -- wired-in primitive; State# is uninhabited at the source level).
    -- These bridges sit at the boundary and are compiler-intrinsic in
    -- the same way that `unsafeCoerce` is -- see the justification at
    -- `isBuiltinBackedModule`'s `Unsafe.Coerce` clause
    -- (Scheduler.hs:5493-5500).  Removing them would require giving Val
    -- a real State#-token shape; that is out of scope.
    -- =================================================================

    -- unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
    -- Source defines `unIO (IO a) = a`; we reconstruct a fresh state
    -- transformer from a VIO action.  RTS-exclusive: VIO's inner host
    -- IO action cannot be expressed as a source-level State# function.
    , ("unIO",            unIOB)
    , ("GHC.IO.unIO",     unIOB)
    , ("GHC.Internal.IO.unIO", unIOB)
    -- ioToST / unsafeIOToST :: IO a -> ST s a
    -- Source body re-wraps a State# function in the ST newtype.  IHC's
    -- ST is also a state-transformer at the source level, but the VIO
    -- carrier needs unwrapping into the host IO before re-wrapping as
    -- an ST runner -- this transformation crosses the VIO/State#
    -- boundary and is not source-expressible.  unsafeIOToST is the
    -- unchecked variant (source uses `unsafeCoerce`, itself compiler-
    -- intrinsic; see Unsafe.Coerce clause).
    , ("ioToST",          ioToSTB)
    , ("GHC.IO.ioToST",   ioToSTB)
    , ("GHC.Internal.IO.ioToST", ioToSTB)
    , ("unsafeIOToST",    ioToSTB)
    , ("GHC.IO.unsafeIOToST", ioToSTB)
    , ("GHC.Internal.IO.unsafeIOToST", ioToSTB)
    , ("Control.Monad.ST.Unsafe.unsafeIOToST", ioToSTB)
    -- stToIO / unsafeSTToIO :: ST RealWorld a -> IO a
    -- Inverse direction: takes an ST's State# function, runs it via
    -- the host runStateTransformer, packages the result as VIO.  Same
    -- RTS boundary as ioToST -- runs a source-level State# token
    -- producer inside the host IO interpreter.
    , ("stToIO",          stToIOB)
    , ("GHC.IO.stToIO",   stToIOB)
    , ("GHC.Internal.IO.stToIO", stToIOB)
    , ("unsafeSTToIO",    stToIOB)
    , ("GHC.IO.unsafeSTToIO", stToIOB)
    , ("GHC.Internal.IO.unsafeSTToIO", stToIOB)
    , ("Control.Monad.ST.Unsafe.unsafeSTToIO", stToIOB)
    -- =================================================================
    -- end VIO <-> State# bridge
    -- =================================================================
    -- catch: source-loaded from GHC.Internal.IO:
    --   catch (IO io) handler = IO $ catch# io handler'
    -- The source bottoms out on the catch# primop plus the VIO/State#
    -- bridge (`unIO`) and the Val-level `fromException` helper above.
    -- evaluate: removed shim. Source-loaded from GHC.Internal.IO:
    --   `evaluate a = IO $ \s -> seq# a s`
    -- bottoms out into the `seq#` GHC.Prim primop (registered below),
    -- which forces @a@ to WHNF and returns the IO unboxed tuple.
    -- mask / mask_ / uninterruptibleMask{,_} / block / unblock /
    -- unsafeUnmask / allowInterrupt / interruptible are source-loaded from
    -- GHC.Internal.IO: the chain bottoms out on getMaskingState# /
    -- maskAsyncExceptions# / maskUninterruptible# / unmaskAsyncExceptions#
    -- (GHC.Prim primops, registered above) which the interpreter treats as
    -- Unmasked / identity — the no-op masking semantics this code needs.
    -- try / handle: removed shim (PR #171). Source-loaded from
    -- GHC.Internal.Control.Exception.Base: `try a = catch (Right <$> a)
    -- (pure . Left)` / `handle = flip catch`; bottoms out on the
    -- source-loaded `catch` chain (catch# primop).
    -- bracket / bracket_ / bracketOnError / finally / onException:
    -- removed shim (PR #166). Source-loaded from
    -- GHC.Internal.Control.Exception.Base — each bottoms out on the
    -- source-loaded `catch` / `mask` chain.
    -- throwTo source-loads from GHC.Internal.Conc.Sync and bottoms out
    -- on the killThread# primop above.
    -- displayException is a source Exception class method.
    -- Phase 2.9.5: Typeable / TypeRep / cast / Dynamic
    -- typeRep# is the sole method of compiler-generated Typeable
    -- dictionaries. GHC.Internal.Data.Typeable.Internal defines the ordinary
    -- `typeRep = typeRep#` wrapper in source.
    , ("typeRep#",       pure (typeRepHashDispatcher reg))
    , ("typeOf",         typeOfB)
    , ("cast",           castB)
    , ("eqT",            eqTB)
    , ("mkTyCon3",       mkTyCon3B)
    , ("mkTyConApp",     mkTyConAppB)
    -- Phase 3.5: OverloadedLabels
    , ("fromLabel",    fromLabelB reg)
    -- DataKinds Tier 1: GHC.TypeLits runtime dispatch.
    --
    -- 'symbolVal', 'natVal', 'charVal' (and their @'@ Proxy# variants)
    -- recover a DataKinds literal from a @Proxy@ at runtime. GHC
    -- normally generates a 'KnownSymbol' / 'KnownNat' / 'KnownChar'
    -- dictionary during type-checking; we don't type-check, so we
    -- inspect the @VCon "Proxy" [payload]@ that the evaluator's 'ETyApp'
    -- special case produced. Also copes with the "label" escape hatch
    -- where a bare @VLabel name@ flows in (e.g. @symbolVal #email@).
    -- Backstops @symbolVal \@"T" undefined@ / @natVal \@42 undefined@
    -- style calls via the 'ETyApp' short-circuit in IHC.Eval.
    , ("symbolVal",    symbolValB)
    , ("symbolVal'",   symbolValB)
    , ("natVal",       natValB)
    , ("natVal'",      natValB)
    , ("charVal",      charValB)
    , ("charVal'",     charValB)
    , ("someSymbolVal", someSymbolValB)
    , ("someNatVal",    someNatValB)
    , ("someCharVal",   someCharValB)
    -- Phase 3.6: MutVar# primops (backing ST monad source).
    -- GHC.Prim has no .hs source; these are wired-in by the GHC build system.
    , ("newMutVar#",            newMutVarB)
    , ("readMutVar#",           readMutVarB)
    , ("writeMutVar#",          writeMutVarB)
    , ("atomicModifyMutVar#",   atomicModifyMutVarB)
    , ("atomicModifyMutVar2#",  atomicModifyMutVar2B)
    , ("atomicModifyMutVar_#",  atomicModifyMutVarUB)
    , ("atomicSwapMutVar#",     atomicSwapMutVarB)
    , ("casMutVar#",            casMutVarB)
    -- GHC.Prim.reallyUnsafePtrEquality#: raw pointer-equality primop.
    -- Source GHC.Prim.PtrEq defines sameMutVar# and friends in terms of it.
    , ("reallyUnsafePtrEquality#", reallyUnsafePtrEqualityHashB)
    -- Weak# primops.  Weak pointers are RTS objects; source modules only
    -- wrap these operations in ordinary newtypes.
    , ("mkWeak#",               mkWeakHashB)
    , ("mkWeakNoFinalizer#",    mkWeakNoFinalizerHashB)
    , ("deRefWeak#",            deRefWeakHashB)
    , ("finalizeWeak#",         finalizeWeakHashB)
    -- Phase 2.11: TH Lift builtins.
    ] ++ thBuiltinPairs

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

-- | Float-only unary op.
unaryOpFloat :: (Double -> Double) -> IO Val
unaryOpFloat op = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VFloat (op d))
        VInt n   -> pure (VFloat (op (fromIntegral n)))
        _ -> error ("unaryOpFloat: non-numeric arg: " <> showValForDebug av)

-- cmpInt removed in Phase 2.3 — replaced by source-loaded Eq/Ord
-- class dispatch.  The primitive comparison helpers below remain only
-- for GHC.Prim-level operations with no .hs source.

-- | Boolean-returning version of a comparison: returns VCon "True" or "False".
boolVal :: Bool -> Val
boolVal True  = VCon "True"  []
boolVal False = VCon "False" []

primBoolVal :: Bool -> Val
primBoolVal True  = VInt 1
primBoolVal False = VInt 0

--------------------------------------------------------------------------------
-- Primop builder helpers
--
-- Many Int#/Char#/Word# primops follow the same shape: force two
-- arguments, pattern-match on (VInt, VInt) (or extract via
-- 'charPrimOrd'), apply an operator, wrap the result. The helpers
-- below collapse those families to one-liners — each primop becomes
-- a name + operator pair instead of an eight-line copy.
--------------------------------------------------------------------------------

-- | Binary Int# comparison primop (e.g. <#, ==#, >=#).
makeIntCmpOp :: String -> (Int64 -> Int64 -> Bool) -> IO Val
makeIntCmpOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (primBoolVal (op x y))
        _ -> error (name <> ": bad args: " <> showValForDebug av)

-- | Binary Char# comparison primop. Args are unwrapped through
-- 'charPrimOrd' (which itself errors on non-Char/Int args).
makeCharCmpOp :: (Int -> Int -> Bool) -> IO Val
makeCharCmpOp op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    pure (primBoolVal (op (charPrimOrd av) (charPrimOrd bv)))

-- | Binary Word# comparison primop. The args are reinterpreted as
-- 'Word64' before applying the operator.
makeWordCmpOp :: String -> (Word64 -> Word64 -> Bool) -> IO Val
makeWordCmpOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (primBoolVal (op (fromIntegral x) (fromIntegral y)))
        _ -> error (name <> ": bad args")

-- | Binary Word# arithmetic primop. The op runs in 'Word64'; the
-- result is cast back to 'Int64' (storage type for 'VInt').
makeWordArithOp :: String -> (Word64 -> Word64 -> Word64) -> IO Val
makeWordArithOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral (op (fromIntegral x) (fromIntegral y))))
        _ -> error (name <> ": bad args")

-- | Unwrap Word8# payload: bare VInt or boxed VCon "W8#".
asWord8Val :: Val -> Maybe Word8
asWord8Val (VInt n) = Just (fromIntegral n)
asWord8Val (VInteger n)
    | n >= 0, n <= 255 = Just (fromInteger n)
asWord8Val _ = Nothing

forceWord8Payload :: Thunk -> IO (Maybe Word8)
forceWord8Payload t = do
    v <- force legacyHooks t
    case v of
        VCon "W8#" [inner] -> force legacyHooks inner >>= pure . asWord8Val
        other -> pure (asWord8Val other)

-- | Binary Word8# comparison primop. Accepts bare VInt or boxed W8#.
makeWord8CmpOp :: String -> (Word8 -> Word8 -> Bool) -> IO Val
makeWord8CmpOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    ma <- forceWord8Payload a
    mb <- forceWord8Payload b
    case (ma, mb) of
        (Just x, Just y) -> pure (primBoolVal (op x y))
        _ -> do
            av <- force legacyHooks a; bv <- force legacyHooks b
            error (name <> ": bad args: " <> showValForDebug av
                   <> " " <> showValForDebug bv)

-- | Binary Word8# arithmetic; result boxed as VCon "W8#" for Num Word8.
makeWord8ArithOp :: String -> (Word8 -> Word8 -> Word8) -> IO Val
makeWord8ArithOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    ma <- forceWord8Payload a
    mb <- forceWord8Payload b
    case (ma, mb) of
        (Just x, Just y) -> do
            t <- newWHNFThunk (VInt (fromIntegral (op x y)))
            pure (VCon "W8#" [t])
        _ -> do
            av <- force legacyHooks a; bv <- force legacyHooks b
            error (name <> ": bad args: " <> showValForDebug av
                   <> " " <> showValForDebug bv)

plusWord8B, subWord8B, timesWord8B :: IO Val
plusWord8B  = makeWord8ArithOp "plusWord8#"  (+)
subWord8B   = makeWord8ArithOp "subWord8#"   (-)
timesWord8B = makeWord8ArithOp "timesWord8#" (*)

-- | Test for truthy value: VCon "True"/VInt non-zero is True.
isTruthy :: Val -> Bool
isTruthy (VCon "True" _)  = True
isTruthy (VCon "False" _) = False
isTruthy (VInt 0)         = False
isTruthy (VInt _)         = True
isTruthy other = error ("isTruthy: not a Bool: " <> showValForDebug other)

--------------------------------------------------------------------------------
-- Phase 2.3: type-class dispatch helpers for Eq, Ord, Show
--
-- Eq/Ord class methods source-load through the scheduler's class-method
-- dispatcher.  'eqVals' remains as an internal helper for derived
-- instances and structural Ord fallback.
--------------------------------------------------------------------------------

-- | Force a 'VLazyMethod' result from 'lookupInstanceMethod', tolerating
-- parse/eval failures by treating them as "no instance".  Instance method
-- bodies are registered lazily in the class registry (see
-- 'IHC.Scheduler.evalMethodWithLazy'); without this force, the builtin
-- dispatch paths would try to @apply@ the opaque 'VLazyMethod' wrapper.
forceInstanceMethod :: Maybe Val -> IO (Maybe Val)
forceInstanceMethod Nothing  = pure Nothing
forceInstanceMethod (Just v) = do
    r <- try (forceMethodVal legacyHooks v) :: IO (Either SomeException Val)
    case r of
        Right v'
          | isPlaceholder v' -> pure Nothing   -- treat placeholder as miss
          | otherwise        -> pure (Just v')
        Left  _              -> pure Nothing
  where
    isPlaceholder (VCon n []) = BC.pack "<ihc-method-placeholder>" `BS.isPrefixOf` n
    isPlaceholder _           = False

-- | Core equality test on WHNF values.
eqVals :: ClassRegistry -> Val -> Val -> IO Val
eqVals reg av bv = case (av, bv) of
    (VInt x, VInt y)     -> pure (boolVal (x == y))
    (VFloat x, VFloat y) -> pure (boolVal (x == y))
    (VInt x, VFloat y)   -> pure (boolVal (fromIntegral x == y))
    (VFloat x, VInt y)   -> pure (boolVal (x == fromIntegral y))
    (VChar x, VChar y)   -> pure (boolVal (x == y))
    (VInt x, VChar y)    -> pure (boolVal (toEnum (fromIntegral x) == y))
    (VChar x, VInt y)    -> pure (boolVal (x == toEnum (fromIntegral y)))
    (VStr x, VStr y)     -> pure (boolVal (x == y))
    (VStr x, _) -> do
        m <- charListBytes bv
        case m of
            Just y  -> pure (boolVal (x == y))
            Nothing -> fallbackNoBuiltinEq
    (_, VStr y) -> do
        m <- charListBytes av
        case m of
            Just x  -> pure (boolVal (x == y))
            Nothing -> fallbackNoBuiltinEq
    (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2)) ->
        pure (boolVal (p1 == p2))
    (VUnit, VPrimObj (PrimPtr p)) ->
        pure (boolVal (p == nullPtr))
    (VPrimObj (PrimPtr p), VUnit) ->
        pure (boolVal (p == nullPtr))
    (VInt n, VPrimObj (PrimPtr p)) ->
        pure (boolVal ((n == 0 && p == nullPtr)))
    (VPrimObj (PrimPtr p), VInt n) ->
        pure (boolVal ((n == 0 && p == nullPtr)))
    -- Cross-representation Ptr equality: one side is a host-primitive
    -- pointer (VPrimObj PrimPtr from a libffi/primop call like memchr),
    -- the other is a source-loaded @Ptr addr#@ (@VCon "Ptr" [_]@).
    -- Without these cases the comparison falls through to the class-
    -- method dispatcher, which has no @Eq Ptr@ instance registered for
    -- the @<Ptr>@ tag and bombs.  Surfaced by warp's parseRequestLine
    -- doing @pathptr0 == nullPtr@ where one operand is libffi-backed
    -- (memchr) and the other is the source-loaded
    -- @Foreign.Ptr.nullPtr@ which scope-resolves to @VCon "Ptr" [VInt 0]@
    -- inside the warp module's import scope.
    (VPrimObj (PrimPtr p), VCon "Ptr" [t]) -> do
        v <- force legacyHooks t
        case v of
            VPrimObj (PrimPtr p2) -> pure (boolVal (p == p2))
            VInt n                -> pure (boolVal (p == nullPtr && n == 0))
            VInteger n            -> pure (boolVal (p == nullPtr && n == 0))
            VUnit                 -> pure (boolVal (p == nullPtr))
            _                     -> pure (boolVal False)
    (VCon "Ptr" [t], VPrimObj (PrimPtr p)) -> do
        v <- force legacyHooks t
        case v of
            VPrimObj (PrimPtr p2) -> pure (boolVal (p2 == p))
            VInt n                -> pure (boolVal (p == nullPtr && n == 0))
            VInteger n            -> pure (boolVal (p == nullPtr && n == 0))
            VUnit                 -> pure (boolVal (p == nullPtr))
            _                     -> pure (boolVal False)
    (VPrimObj (PrimForeignPtr fp1), VPrimObj (PrimForeignPtr fp2)) ->
        pure (boolVal (fp1 == fp2))
    (VUnit, VUnit)      -> pure (boolVal True)
    (VCon "True" _, VCon "True" _)   -> pure (boolVal True)
    (VCon "False" _, VCon "False" _) -> pure (boolVal True)
    (VCon "True" _, VCon "False" _)  -> pure (boolVal False)
    (VCon "False" _, VCon "True" _)  -> pure (boolVal False)
    -- ByteString's source-loaded Eq instance eventually needs byte-content
    -- equality.  Keep that representation bridge here rather than reviving
    -- any Data.ByteString API shim: default VCon field-by-field equality
    -- would compare ForeignPtr identity and fail for equal fresh buffers.
    --
    -- Also coerce [Char]/VStr on either side: OverloadedStrings leaves
    -- @"server"@ as a char list until a BS consumer packs it.  Without
    -- this, @bs == "server"@ in warp's responseKeyIndex (first arg real
    -- BS, second still [Char]) falls through to structural VCon compare
    -- and always yields False → idx = -1 → empty IndexedHeader.
    (VCon "BS" _, VCon "BS" _) -> do
        ba <- bsValToBS av
        bb <- bsValToBS bv
        pure (boolVal (ba == bb))
    (VCon "BS" _, _) -> do
        mBb <- charListBytes bv
        case mBb of
            Just bb -> do
                ba <- bsValToBS av
                pure (boolVal (ba == bb))
            Nothing -> fallbackNoBuiltinEq
    (_, VCon "BS" _) -> do
        mBa <- charListBytes av
        case mBa of
            Just ba -> do
                bb <- bsValToBS bv
                pure (boolVal (ba == bb))
            Nothing -> fallbackNoBuiltinEq
    (VCon n1 ts1, VCon n2 ts2) -> do
        -- A user-defined @instance Eq T@ may override the default
        -- structural equality (e.g. comparing only one field of a
        -- record, or normalising before compare). Look up the
        -- user instance first; structural compare is the fallback.
        --
        -- 'lookupInstanceMethod' drains the lazy-instance catalogue
        -- on miss, so this is also the trigger that materialises
        -- the user's instance the first time '==' fires on @T@.
        let tag = typeTagOf av
        mUser <- lookupInstanceMethod reg "Eq" tag "==" >>= forceInstanceMethod
        case mUser of
            Just eqMethod -> do
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply legacyHooks eqMethod aT
                apply legacyHooks r1 bT
            Nothing
                | n1 /= n2  -> pure (boolVal False)
                | otherwise -> do
                    -- Default: structural field-by-field equality.
                    results <- mapM (\(t1, t2) -> do
                        v1 <- force legacyHooks t1
                        v2 <- force legacyHooks t2
                        eqVals reg v1 v2)
                        (zip ts1 ts2)
                    pure (boolVal (all isTruthy results))
    _ -> fallbackNoBuiltinEq
  where
    charListBytes v = do
        isChars <- isCharList v
        if isChars
            then Just . BC.pack <$> valToString v
            else pure Nothing

    fallbackNoBuiltinEq = do
        -- Try user-defined instance.
        let tag = typeTagOf av
        mEqMethod <- lookupInstanceMethod reg "Eq" tag "==" >>= forceInstanceMethod
        case mEqMethod of
            Just eqMethod -> do
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply legacyHooks eqMethod aT
                apply legacyHooks r1 bT
            _ -> error ("(==): no Eq instance for type tag `"
                        <> BC.unpack tag <> "`: "
                        <> showValForDebug av
                        <> " vs " <> showValForDebug bv)

-- | Internal Ord relation helper. Slot in the method list:
--   0 = (<), 1 = (<=), 2 = (>), 3 = (>=), 4 = compare
-- Top-level relation operators are not registered as builtins; they
-- source-load through @GHC.Classes@.  This helper is kept for derived /
-- structural comparisons and representation-level recursion.
ordCmp :: ClassRegistry -> Int -> Val -> Val -> IO Val
ordCmp _reg slot av bv = case (av, bv) of
    (VInt x, VInt y)     -> pure (boolVal (intOrdSlot slot x y))
    (VFloat x, VFloat y) -> pure (boolVal (dblOrdSlot slot x y))
    (VInt x, VFloat y)   -> pure (boolVal (dblOrdSlot slot (fromIntegral x) y))
    (VFloat x, VInt y)   -> pure (boolVal (dblOrdSlot slot x (fromIntegral y)))
    (VChar x, VChar y)   -> let xi = fromIntegral (fromEnum x) :: Int64
                                yi = fromIntegral (fromEnum y) :: Int64
                            in pure (boolVal (intOrdSlot slot xi yi))
    (VStr x, VStr y)     -> pure (boolVal (strOrdSlot slot x y))
    (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2)) ->
        pure (boolVal (ptrOrdSlot slot p1 p2))
    (VPrimObj (PrimForeignPtr fp1), VPrimObj (PrimForeignPtr fp2)) ->
        pure (boolVal (foreignPtrOrdSlot slot fp1 fp2))
    -- ByteString's source-loaded Ord instance eventually needs byte-content
    -- ordering.  This is a representation bridge, not a Data.ByteString API
    -- shim; structural VCon comparison would order by ForeignPtr address.
    (VCon "BS" _, VCon "BS" _) -> do
        ba <- bsValToBS av
        bb <- bsValToBS bv
        let o = compare ba bb
        pure (boolVal (ordSlot slot o))
    _ -> do
        let tag = typeTagOf av
        let ordMethodName = case slot of
                0 -> Just (BC.pack "<")
                1 -> Just (BC.pack "<=")
                2 -> Just (BC.pack ">")
                3 -> Just (BC.pack ">=")
                _ -> Nothing
        mMethod <- maybe (pure Nothing)
                         (\mn -> lookupInstanceMethod _reg "Ord" tag mn >>= forceInstanceMethod)
                         ordMethodName
        case mMethod of
            Just method -> do
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply legacyHooks method aT
                apply legacyHooks r1 bT
            _ -> do
                -- No user Ord instance registered. Try the structural
                -- fallback: if both values are VCon, derive an Ord by
                -- comparing constructor identity first, then fields
                -- left-to-right. This mirrors 'eqVals' for Eq.
                mOrd <- structuralOrdering _reg av bv
                case mOrd of
                    Just o  -> pure (boolVal (ordSlot slot o))
                    Nothing ->
                        -- Fall back to Eq for <= and >=
                        case slot of
                            1 -> do r <- eqVals _reg av bv
                                    if isTruthy r then pure (boolVal True)
                                    else ordCmp _reg 0 av bv
                            3 -> do r <- eqVals _reg av bv
                                    if isTruthy r then pure (boolVal True)
                                    else ordCmp _reg 2 av bv
                            _ -> error ("Ord: no instance for type tag `"
                                        <> BC.unpack (typeTagOf av) <> "` while comparing "
                                        <> showValForDebug av <> " and "
                                        <> showValForDebug bv)
  where
    intOrdSlot 0 x y = x < y
    intOrdSlot 1 x y = x <= y
    intOrdSlot 2 x y = x > y
    intOrdSlot 3 x y = x >= y
    intOrdSlot _ _ _ = False

    dblOrdSlot 0 x y = x < y
    dblOrdSlot 1 x y = x <= y
    dblOrdSlot 2 x y = x > y
    dblOrdSlot 3 x y = x >= y
    dblOrdSlot _ _ _ = False

    strOrdSlot 0 x y = x < y
    strOrdSlot 1 x y = x <= y
    strOrdSlot 2 x y = x > y
    strOrdSlot 3 x y = x >= y
    strOrdSlot _ _ _ = False

    ptrOrdSlot 0 x y = x < y
    ptrOrdSlot 1 x y = x <= y
    ptrOrdSlot 2 x y = x > y
    ptrOrdSlot 3 x y = x >= y
    ptrOrdSlot _ _ _ = False

    foreignPtrOrdSlot 0 x y = x < y
    foreignPtrOrdSlot 1 x y = x <= y
    foreignPtrOrdSlot 2 x y = x > y
    foreignPtrOrdSlot 3 x y = x >= y
    foreignPtrOrdSlot _ _ _ = False

    -- Map a host 'Ordering' into the comparison result the caller's
    -- dispatch slot expects. Slot 0 = (<), 1 = (<=), 2 = (>), 3 = (>=).
    -- Slot 4 is unused at this level (the source-loaded @compare@
    -- default in @GHC.Classes@ triangulates via slot 1 + Eq), but we
    -- handle it defensively.
    ordSlot 0 o = o == LT
    ordSlot 1 o = o == LT || o == EQ
    ordSlot 2 o = o == GT
    ordSlot 3 o = o == GT || o == EQ
    ordSlot _ _ = False

-- | Global map from constructor name to @(typeName, declIndex)@, kept
-- alive for the lifetime of the interpreter. Populated as a side effect
-- of 'buildConEnv' (every call merges the DataRegistry entries in) and
-- consulted by 'structuralOrdering' when two different constructors of
-- the same type need to be compared.
--
-- We use a module-level 'IORef' (via 'unsafePerformIO') because
-- 'structuralOrdering' is invoked through the Ord-dispatch chain
-- (ordCmp → structuralOrdering → compareFields → valOrdering → …)
-- and threading an extra registry through every one of them for a
-- purely-derived fallback would touch far too many call sites. The
-- ref is written once per module load and read many times per
-- comparison; races aren't a concern because the scheduler only
-- rebuilds the env single-threaded.
{-# NOINLINE ctorIndexRegistry #-}
ctorIndexRegistry :: IORef (Map.Map ByteString (ByteString, Int))
ctorIndexRegistry = unsafePerformIO (newIORef Map.empty)

-- | Merge the @(typeName, declIndex)@ entries from a 'DataRegistry'
-- into the global 'ctorIndexRegistry'. Arity is intentionally dropped
-- here — the index registry only cares about ordering.
populateCtorIndex :: DataRegistry -> IO ()
populateCtorIndex reg =
    modifyIORef' ctorIndexRegistry $ \m ->
        Map.union m (Map.map (\(tyName, _arity, idx) -> (tyName, idx)) reg)

-- | Look up a constructor's @(typeName, declIndex)@ in the global
-- registry. Built-in constructors (list, Bool, tuples, Unit) aren't
-- recorded there — structural ordering handles them explicitly.
lookupCtorIndex :: ByteString -> IO (Maybe (ByteString, Int))
lookupCtorIndex name = Map.lookup name <$> readIORef ctorIndexRegistry

-- | Reset the global ctor-index registry.  Called by the scheduler so a
-- second 'loadProgramFromSource' call doesn't see stale entries from a
-- prior run that no longer correspond to any loaded module's data
-- decls.
clearCtorIndex :: IO ()
clearCtorIndex = writeIORef ctorIndexRegistry Map.empty

-- | Reset the foreign-ptr-Word8 address-range list.  Without this, a
-- second 'loadProgramFromSource' run accumulates ranges from the first
-- run's already-collected 'ForeignPtr' allocations — addresses the GC
-- may have reused for other purposes by the time of the next
-- 'isMarkedWord8Ptr' check.
clearForeignPtrWord8Ranges :: IO ()
clearForeignPtrWord8Ranges = clearWord8PtrRanges

-- | Kill every interpreter-spawned thread from the prior
-- 'loadProgramFromSource' run and clear the registry.  Called by
-- 'IHC.Scheduler.resetPerRunGlobals' at the next run boundary — by
-- which point the prior program's @main@ result has already been
-- forced and returned, so any thread it forked (warp accept loop,
-- TimeManager, async workers, bare @forkIO@) is leaked background
-- work whose TSO stack is pure garbage.  Without this the ~600-example
-- in-process hspec suite accumulates ~4 GB of @STACK@ and the
-- master-CI run heap-exhausts (the GC death-spiral repeatedly walking
-- thousands of retained thread stacks).  @killThread@ exceptions are
-- swallowed: an already-finished thread, or one wedged in an
-- uninterruptible FFI call, must not abort the reset.
reapSpawnedThreads :: IO ()
reapSpawnedThreads = do
    tids <- readIORef spawnedThreadsRef
    writeIORef spawnedThreadsRef []
    mapM_ (\t -> do
              _ <- CE.try (killThread t) :: IO (Either SomeException ())
              pure ())
          tids

-- | Structural Ord fallback for VCon values.
--
-- Returns 'Just ord' when @av@ and @bv@ can be compared structurally,
-- 'Nothing' when the shapes don't line up and the caller should keep
-- trying (e.g. fall back to Eq-based slot dispatch for slot 1 / 3).
--
-- Semantics for user-derived Ord on sums-of-products:
--
--   1. Same constructor → compare fields lexicographically
--      (left-to-right, short-circuit on first non-EQ).
--   2. Different constructors of the *same* type → compare by
--      declaration index, so @data Color = Red | Green | Blue@ gives
--      @Red < Green < Blue@ (matching GHC's derived-Ord semantics).
--   3. Different constructors with no recorded type (built-ins not in
--      the registry, or types from different declarations) → fall back
--      to lexicographic comparison of the constructor name. This is a
--      best-effort last resort; correct programs shouldn't compare
--      values of unrelated types.
structuralOrdering :: ClassRegistry -> Val -> Val -> IO (Maybe Ordering)
structuralOrdering reg av bv = case (av, bv) of
    (VUnit, VUnit) -> pure (Just EQ)
    -- Bool is declared `data Bool = False | True` so False < True.
    (VCon "False" _, VCon "False" _) -> pure (Just EQ)
    (VCon "True"  _, VCon "True"  _) -> pure (Just EQ)
    (VCon "False" _, VCon "True"  _) -> pure (Just LT)
    (VCon "True"  _, VCon "False" _) -> pure (Just GT)
    -- Lists: @[] < (_ : _)@; then compare heads, then tails.
    (VCon "[]" _, VCon "[]" _) -> pure (Just EQ)
    (VCon "[]" _, VCon ":"  _) -> pure (Just LT)
    (VCon ":"  _, VCon "[]" _) -> pure (Just GT)
    -- Generic VCon path: covers @:@, tuples, and user-defined ADTs.
    (VCon n1 ts1, VCon n2 ts2)
        | n1 == n2 && length ts1 == length ts2 -> do
            o <- compareFields ts1 ts2
            pure (Just o)
        | otherwise -> do
            -- Different constructors: prefer the declaration-index
            -- registry so user-derived Ord matches GHC semantics.
            mIdx1 <- lookupCtorIndex n1
            mIdx2 <- lookupCtorIndex n2
            case (mIdx1, mIdx2) of
                (Just (ty1, i1), Just (ty2, i2)) | ty1 == ty2 ->
                    pure (Just (compare i1 i2))
                _ ->
                    -- Last-resort fallback: neither constructor (or
                    -- both from different types) carries index data.
                    -- Fall back to lex-comparing the name.
                    pure (Just (compare n1 n2))
    _ -> pure Nothing
  where
    compareFields []     []     = pure EQ
    compareFields (t1:r1) (t2:r2) = do
        v1 <- force legacyHooks t1
        v2 <- force legacyHooks t2
        -- Recurse through the full Ord dispatch (user instances +
        -- structural fallback) rather than structuralOrdering directly,
        -- so primitive fields (Int, Char, etc.) hit their fast paths.
        mo <- valOrdering reg v1 v2
        case mo of
            EQ -> compareFields r1 r2
            o  -> pure o
    compareFields _ _ = pure EQ   -- unreachable (arity equal check above)

-- | Run the full Ord dispatch and distil the result into a host
-- 'Ordering'. Used by 'compareFields' inside 'structuralOrdering' so
-- the recursive field walk hits the primitive Ord fast paths
-- (Int/Float/Char/String/Ptr) before bottoming out on 'ordCmp'.
valOrdering :: ClassRegistry -> Val -> Val -> IO Ordering
valOrdering reg av bv = case (av, bv) of
    (VInt x,   VInt y)   -> pure (compare x y)
    (VFloat x, VFloat y) -> pure (compare x y)
    (VInt x,   VFloat y) -> pure (compare (fromIntegral x :: Double) y)
    (VFloat x, VInt y)   -> pure (compare x (fromIntegral y :: Double))
    (VChar x,  VChar y)  -> pure (compare x y)
    (VStr x,   VStr y)   -> pure (compare x y)
    (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2)) ->
        pure (compare p1 p2)
    (VPrimObj (PrimForeignPtr fp1), VPrimObj (PrimForeignPtr fp2)) ->
        pure (compare fp1 fp2)
    _ -> do
        -- Try user Ord instance first (compare at slot 4 if present).
        let tag = typeTagOf av
        mCmpMethod <- lookupInstanceMethod reg "Ord" tag "compare" >>= forceInstanceMethod
        case mCmpMethod of
            Just cmpMethod -> do
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply legacyHooks cmpMethod aT
                cv <- apply legacyHooks r1 bT
                pure (orderingFromVCon cv)
            _ -> do
                mo <- structuralOrdering reg av bv
                case mo of
                    Just o  -> pure o
                    Nothing -> do
                        -- Last resort: use slot-0 (<) and Eq to triangulate.
                        lt <- ordCmp reg 0 av bv
                        if isTruthy lt then pure LT
                        else do
                            eq <- eqVals reg av bv
                            if isTruthy eq then pure EQ else pure GT
  where
    orderingFromVCon (VCon "LT" _) = LT
    orderingFromVCon (VCon "EQ" _) = EQ
    orderingFromVCon (VCon "GT" _) = GT
    orderingFromVCon other = error ("valOrdering: user Ord `compare` "
                                    <> "returned non-Ordering: "
                                    <> showValForDebug other)

-- | Show a value, consulting the ClassRegistry for user-defined Show.
--
-- This is the workhorse formatter that backs the source-loaded
-- @class Show a@ machinery via 'IHC.Scheduler.hostShowFallback' (called
-- by 'classMethodDispatcher' on a registry miss / placeholder).  The
-- bare-name @show@ shim was removed per CLAUDE.md "Builtin modules:
-- minimum surface only"; resolution now flows through
-- 'tryClassMethodFromRegistry' to the demand-driven dispatcher, which
-- delegates here whenever the source-loaded @Show.show@ instance isn't
-- a working method (e.g. parser gaps on primop unboxing patterns).
showValWith :: ClassRegistry -> Val -> IO String
showValWith reg av = case av of
    VLabel name -> pure ("#" <> BC.unpack name)   -- Phase 3.5: #name
    VInt _    -> showVal av
    VFloat _  -> showVal av
    VChar _   -> showVal av
    VStr _    -> showVal av
    VUnit     -> showVal av
    VCon "[]" _ -> showVal av
    VCon ":" _  -> do
        cl <- isCharList av
        if cl then showVal av
        else do
            xs <- forceList av
            parts <- mapM (showValWith reg) xs
            pure ("[" <> intercalate "," parts <> "]")
    VCon "True" _  -> pure "True"
    VCon "False" _ -> pure "False"
    VCon "Proxy" _ -> pure "Proxy"   -- DataKinds payload is invisible in show
    VCon "BS" _ -> do
        -- Render a ByteString using Data.ByteString.Char8's show-style
        -- output: `"..."` with printable ASCII passed through and
        -- non-printables escaped. Matches the GHC stock instance.
        bs <- bsValToBS av
        pure (show bs)
    -- Prim boxes first — stock Show Word8/Int8 print the numeric
    -- payload only.  Do this before instance lookup so a missing or
    -- placeholder Show Word8 cannot fall through to "W8# 255".
    VCon n [t]
        | n `elem` wordSizedPrimShowCons || n `elem` intSizedPrimShowCons -> do
            v <- force legacyHooks t
            showValWith reg v
    VCon n _ | isTupleConName n -> showVal av
    VCon n _ -> do
        -- Prefer type-name tag (Word8 for W8#, Maybe for Just, …) so
        -- Show instances registered under the type — not the prim box
        -- ctor — are found.  Fall back to the raw ctor name for ADTs
        -- that register per-constructor (registerOne).
        let tags = nubTags [typeTagOf av, n]
        mShowMethod <- findShowMethod tags
        case mShowMethod of
            Just showMethod -> do
                shown <- CE.try @SomeException $ do
                    aT <- newWHNFThunk av
                    rv <- apply legacyHooks showMethod aT
                    valToString rv
                case shown of
                    Right s -> pure s
                    Left _  -> showVal av
            _ -> showVal av
    _ -> showVal av
  where
    nubTags = go []
      where
        go acc [] = reverse acc
        go acc (t:ts)
            | t `elem` acc = go acc ts
            | otherwise    = go (t:acc) ts
    findShowMethod [] = pure Nothing
    findShowMethod (tag:tags) = do
        m <- lookupInstanceMethod reg "Show" tag "show" >>= forceInstanceMethod
        case m of
            Just _  -> pure m
            Nothing -> findShowMethod tags

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
    hv <- force legacyHooks h
    tv <- force legacyHooks t
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
    hv <- force legacyHooks h
    case hv of
        VChar _ -> pure True
        _       -> pure False
isCharList _ = pure False

-- | Render any supported WHNF value as the Haskell @show@ of it.
-- | Show a Double in Haskell-compatible format. Whole numbers are shown
-- with a trailing ".0", e.g. @3.0@ not @3@.
showDouble :: Double -> String
showDouble d
    | isNaN d      = "NaN"
    | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
    | otherwise    =
        let s = show d
        in if '.' `elem` s || 'e' `elem` s then s else s <> ".0"

-- | Prim box constructors for fixed-width Word/Int.  Shown as bare
-- numeric payloads (stock Show Word8 / Int8), not "W8# n".
wordSizedPrimShowCons :: [ByteString]
wordSizedPrimShowCons = ["W8#", "W16#", "W32#", "W64#", "W#"]

intSizedPrimShowCons :: [ByteString]
intSizedPrimShowCons = ["I8#", "I16#", "I32#", "I64#", "I#"]

showVal :: Val -> IO String
showVal (VLabel name) = pure ("#" <> BC.unpack name)   -- Phase 3.5
showVal (VInt n)    = pure (show n)
showVal (VInteger n) = pure (show n)
showVal (VFloat d)  = pure (showDouble d)
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
showVal (VCon name thunks)
    -- Fixed-width Int/Word boxes: GHC Show prints the numeric payload
    -- only (Show Word8 uses fromIntegral to Int).  When the source
    -- Show instance isn't available yet (hostShowFallback path), still
    -- match stock output instead of "W8# 255".
    | name `elem` wordSizedPrimShowCons || name `elem` intSizedPrimShowCons
    , [t] <- thunks = do
        v <- force legacyHooks t
        showVal v
    | isUnboxedTupleConName name = do
        parts <- mapM (\t -> do v <- force legacyHooks t; showVal v) thunks
        pure ("(#" <> intercalate "," parts <> "#)")
    | isTupleConName name = do
        parts <- mapM (\t -> do v <- force legacyHooks t; showVal v) thunks
        pure ("(" <> intercalate "," parts <> ")")
    -- @Proxy@ is rendered as just "Proxy" regardless of any DataKinds
    -- payload the evaluator attached from an 'ETyApp' annotation
    -- (GHC's @show (Proxy :: Proxy "foo") = "Proxy"@).
    | name == "Proxy" = pure "Proxy"
    | otherwise = do
        parts <- mapM (\t -> do v <- force legacyHooks t; showVal v) thunks
        case parts of
            [] -> pure (BC.unpack name)
            _  -> pure (BC.unpack name <> " " <> unwords parts)
showVal (VFun _)    = pure "<function>"
showVal (VFunIP _ _) = pure "<function>"
showVal (VClassMethod _ _ _ _ _) = pure "<function>"
showVal (VLazyMethod _) = pure "<function>"
showVal (VIO _)     = pure "<IO>"
showVal (VPrimObj (PrimIORef  _))      = pure "<IORef>"
showVal (VPrimObj (PrimHandle _))      = pure "<Handle>"
showVal (VPrimObj (PrimForeignPtr _))  = pure "<ForeignPtr>"
showVal (VPrimObj (PrimPtr _))         = pure "<Ptr>"
showVal (VPrimObj (PrimByteArray _))   = pure "<MutableByteArray>"
showVal (VPrimObj (PrimArray _))       = pure "<MutableArray#>"
showVal (VPrimObj (PrimBoxedArray _ _)) = pure "<BoxedArray#>"
showVal (VPrimObj PrimRealWorld)       = pure "<RealWorld#>"
showVal (VPrimObj (PrimMVar _))        = pure "<MVar>"
showVal (VPrimObj (PrimTVar _))        = pure "<TVar>"
showVal (VPrimObj (PrimThreadId tid))  = pure ("ThreadId " <> show tid)
showVal (VPrimObj (PrimBigNat n))      = pure ("<BigNat# " <> show n <> ">")

-- | Tuple constructors are named @(,)@, @(,,)@, @(,,,)@, etc. — any
-- @(@ followed by @n@ commas and @)@.
isTupleConName :: ByteString -> Bool
isTupleConName bs = case BC.unpack bs of
    '(':rest | not (null rest), last rest == ')' ->
        let middle = init rest
        in not (null middle) && all (== ',') middle
    _ -> False

-- | Unboxed tuple constructors: @(#,#)@, @(#,,#)@, etc.
isUnboxedTupleConName :: ByteString -> Bool
isUnboxedTupleConName bs = case BC.unpack bs of
    '(':'#':rest
        | not (null rest)
        , last rest == ')'
        -> let inner = init rest   -- e.g. ",#" or ",,#"
           in not (null inner) && last inner == '#'
              && all (\c -> c == ',' || c == '#') inner
    _ -> False

-- Phase 3.5: OverloadedLabels ------------------------------------------------

-- | @fromLabel :: VLabel name -> Val@
--
-- In GHC, @fromLabel \@"name"@ selects an @IsLabel@ instance via type
-- inference. We have no type inference, so we dispatch at runtime.
--
-- Dispatch strategy:
--
-- * We first walk the class registry looking for an @IsLabel@ instance
--   whose type tag is something other than the synthetic @Proxy@ default.
--   The first such user-defined instance is picked (its @fromLabel@
--   method receives the VLabel and produces the instance's target value).
-- * Otherwise we fall through to the registered default instance for
--   @(IsLabel s (Proxy s'))@ (IHP's instance) which produces
--   @VCon "Proxy" []@.
-- * Pattern-matching in the evaluator treats @Proxy@ as transparently
--   matching a @VLabel@, so downstream code that pattern-matches on
--   @Proxy@ still works whether or not @fromLabel@ was called first.
--
-- Note on user overrides: with no type information we cannot tell which
-- user instance to pick when multiple are visible. The first non-default
-- instance wins — userland code that relies on specific dispatch should
-- call @fromLabel@ explicitly in a monomorphic context where exactly one
-- instance is in scope.
fromLabelB :: ClassRegistry -> IO Val
fromLabelB reg = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VLabel lbl -> do
            mMethod <- lookupUserIsLabel reg lbl
            case mMethod of
                Just fromLabelM ->
                    -- User-defined @fromLabel@ typically has type
                    -- @forall s. IsLabel s a => a@ and is 0-arity
                    -- (e.g. @fromLabel = Wrap \"x\"@), but some instances
                    -- may define it as a function taking the label. If the
                    -- method is a function, apply it to the label; otherwise
                    -- return the stored value directly.
                    case fromLabelM of
                        VFun _               -> do { aT <- newWHNFThunk av; apply legacyHooks fromLabelM aT }
                        VFunIP _ _           -> do { aT <- newWHNFThunk av; apply legacyHooks fromLabelM aT }
                        VClassMethod _ _ _ _ _ -> do { aT <- newWHNFThunk av; apply legacyHooks fromLabelM aT }
                        v                    -> pure v
                _ -> pure (VCon "Proxy" [])   -- default: Proxy IsLabel instance
        other -> error ("fromLabel: expected a Label value, got: "
                        <> showValForDebug other)

-- | Find a user-defined IsLabel instance keyed by the label's Symbol.
--
-- The class registry stores @IsLabel@ instances under composite keys from
-- 'registerInstanceMulti'. For an IHP-shaped
-- @instance IsLabel \"email\" Wrap where ...@ the key is
-- @(\"IsLabel\", [\"email\", \"Wrap\"])@, and for a polymorphic
-- @instance IsLabel s Wrap where ...@ the key is
-- @(\"IsLabel\", [\"s\", \"Wrap\"])@.
--
-- Dispatch strategy given a runtime @VLabel \"email\"@:
--
-- 1. Look for an exact Symbol-keyed match — first tag equals the label
--    name.  This is what makes two @IsLabel \"email\" Wrap@ and
--    @IsLabel \"name\" Wrap@ instances route to distinct method bodies.
-- 2. Fall back to a polymorphic user instance (first tag is a lower-case
--    type variable, i.e. not a Symbol or upper-case type name).  Those
--    are written @instance IsLabel s Wrap where ...@ and should fire
--    for any label.
-- 3. Skip the built-in default @(IsLabel s (Proxy s'))@ registered
--    under @[\"Proxy\"]@ — that's the fallthrough handled by the caller.
lookupUserIsLabel :: ClassRegistry -> ByteString -> IO (Maybe Val)
lookupUserIsLabel reg lbl = do
    -- Stage 2: user-defined @IsLabel s T@ instances now sit in
    -- 'instanceCatalogueRef' under @"IsLabel"@. Drain them into the
    -- registry before we scan @reg@ — this is the IsLabel-dispatch
    -- counterpart of 'lazyInstanceRetry' inside the class-method
    -- dispatcher.  The drain is cheap when empty and runs at most
    -- once per (cls, tag) combination per run.
    _ <- drainCataloguedInstancesForClass (BC.pack "IsLabel")
    m <- readIORef reg
    let entries = [ (tags, methods)
                  | ((cls, tags), methods) <- HashMap.toList m
                  , cls == BC.pack "IsLabel"
                  ]
        -- First pass: instance whose leading tag matches the label literal.
        symbolMatches =
            [ fromLabelMethod
            | (tag : _, methods) <- entries
            , tag == lbl
            , Just fromLabelMethod <- [HashMap.lookup (BC.pack "fromLabel") methods]
            ]
        -- Second pass: polymorphic user instance — first tag is a lower-case
        -- type variable (e.g. 's'), not the @Proxy@ default and not a
        -- concrete Symbol/type.  'normalizeTyTag' leaves these lower-case.
        polymorphicMatches =
            [ fromLabelMethod
            | (tag : _, methods) <- entries
            , tag /= BC.pack "Proxy"
            , tag /= lbl
            , not (BS.null tag)
            , let c = BC.head tag
            , c >= 'a' && c <= 'z'
            , Just fromLabelMethod <- [HashMap.lookup (BC.pack "fromLabel") methods]
            ]
    -- Instance method bodies are registered lazily as 'VLazyMethod'
    -- (see 'IHC.Scheduler.evalMethodWithLazy').  Force the wrapper now
    -- so the caller ('fromLabelB') can pattern-match against the
    -- concrete Val (VCon, VFun, etc.) without having to unwrap.
    let forceFirst [] = pure Nothing
        forceFirst (ms : _) = fmap Just (forceMethodVal legacyHooks ms)
    case symbolMatches of
        hit@(_ : _) -> forceFirst hit
        []          -> forceFirst polymorphicMatches

-- DataKinds Tier 1 -------------------------------------------------------------

-- | @symbolVal :: Proxy (n :: Symbol) -> String@ — recover the symbol
-- literal attached to a 'Proxy' by the evaluator's @ETyApp@ special case.
--
-- Accepts:
--
--   * @VCon \"Proxy\" [VLabel name]@ — the normal post-@ETyApp@ shape.
--   * @VCon \"Proxy\" [VInt n]@      — tolerates accidental @natVal@
--     shapes (e.g. @symbolVal (Proxy @42)@ in debug code).
--   * @VLabel name@                   — a bare @#name@ flowing straight
--     in, since 'IsLabel' dispatch may short-circuit the @Proxy@
--     wrapper altogether.
--   * @VCon \"Proxy\" []@             — no payload (e.g. forgot @\@T@);
--     returns the empty string rather than crashing.
symbolValB :: IO Val
symbolValB = pure $ VFun $ \p -> do
    pv <- force legacyHooks p
    bs <- proxySymbolPayload pv
    stringToListValIO (BC.unpack bs)

-- | @natVal :: Proxy (n :: Nat) -> Integer@ — dual of 'symbolValB'.
-- Expects a @VInt@ payload; tolerates @VLabel@ by reading it as digits
-- (raw DataKinds nat literal parses to @VInt@ via @parseTyArgLit@, but
-- user-provided proxies may carry labels).
natValB :: IO Val
natValB = pure $ VFun $ \p -> do
    pv <- force legacyHooks p
    case pv of
        VCon "Proxy" (t : _) -> do
            x <- force legacyHooks t
            case x of
                VInt n   -> pure (VInt n)
                VLabel s -> case BC.readInteger s of
                    Just (n, rest) | BS.null rest -> pure (VInt (fromInteger n))
                    _ -> error ("natVal: non-numeric Proxy payload: "
                                 <> BC.unpack s)
                other -> error ("natVal: unexpected Proxy payload: "
                                 <> showValForDebug other)
        VInt n -> pure (VInt n)
        other -> error ("natVal: expected a Proxy, got: "
                         <> showValForDebug other)

-- | @charVal :: Proxy (n :: Char) -> Char@.
charValB :: IO Val
charValB = pure $ VFun $ \p -> do
    pv <- force legacyHooks p
    case pv of
        VCon "Proxy" (t : _) -> do
            x <- force legacyHooks t
            case x of
                VChar c  -> pure (VChar c)
                VLabel s | BC.length s == 1 -> pure (VChar (BC.head s))
                other -> error ("charVal: unexpected Proxy payload: "
                                 <> showValForDebug other)
        VChar c -> pure (VChar c)
        other -> error ("charVal: expected a Proxy, got: "
                         <> showValForDebug other)

-- | @someSymbolVal :: String -> SomeSymbol@ — wrap a runtime string into
-- a @SomeSymbol@ whose contained @Proxy@ carries the string as a
-- @VLabel@ so 'symbolVal' round-trips.
someSymbolValB :: IO Val
someSymbolValB = pure $ VFun $ \s -> do
    sv <- force legacyHooks s
    bs <- listValToBS sv
    lblT <- newWHNFThunk (VLabel bs)
    proxyT <- newWHNFThunk (VCon "Proxy" [lblT])
    pure (VCon "SomeSymbol" [proxyT])

-- | @someNatVal :: Integer -> Maybe SomeNat@ — returns @Nothing@ for
-- negatives, @Just (SomeNat (Proxy \@n))@ otherwise.
someNatValB :: IO Val
someNatValB = pure $ VFun $ \n -> do
    nv <- force legacyHooks n
    case nv of
        VInt k | k < 0 -> pure (VCon "Nothing" [])
               | otherwise -> do
                   intT <- newWHNFThunk (VInt k)
                   proxyT <- newWHNFThunk (VCon "Proxy" [intT])
                   smT <- newWHNFThunk (VCon "SomeNat" [proxyT])
                   pure (VCon "Just" [smT])
        other -> error ("someNatVal: expected an Integer, got: "
                         <> showValForDebug other)

-- | @someCharVal :: Char -> SomeChar@.
someCharValB :: IO Val
someCharValB = pure $ VFun $ \c -> do
    cv <- force legacyHooks c
    case cv of
        VChar ch -> do
            chT <- newWHNFThunk (VChar ch)
            proxyT <- newWHNFThunk (VCon "Proxy" [chT])
            pure (VCon "SomeChar" [proxyT])
        other -> error ("someCharVal: expected a Char, got: "
                         <> showValForDebug other)

-- | Extract the symbol-shaped payload bytes from whatever a user passed
-- as the first argument of 'symbolVal'. See 'symbolValB' for the
-- accepted shapes.
proxySymbolPayload :: Val -> IO ByteString
proxySymbolPayload (VCon "Proxy" (t : _)) = do
    x <- force legacyHooks t
    case x of
        VLabel s -> pure s
        VInt   n -> pure (BC.pack (show n))
        VChar  c -> pure (BC.pack [c])
        other    -> pure (BC.pack (showValForDebug other))
proxySymbolPayload (VCon "Proxy" []) = pure BC.empty
proxySymbolPayload (VLabel s) = pure s
proxySymbolPayload (VCon "SomeSymbol" [t]) = do
    x <- force legacyHooks t
    proxySymbolPayload x
proxySymbolPayload other = error
    ("symbolVal: expected a Proxy or SomeSymbol, got: "
     <> showValForDebug other)

-- | Walk a Haskell 'String' (cons-list of 'VChar') down to a ByteString.
listValToBS :: Val -> IO ByteString
listValToBS = go []
  where
    go acc (VCon "[]" _)       = pure (BC.pack (reverse acc))
    go acc (VCon ":"  [h, t])  = do
        hv <- force legacyHooks h
        tv <- force legacyHooks t
        case hv of
            VChar c -> go (c : acc) tv
            _       -> error ("listValToBS: list element is not a Char: "
                                <> showValForDebug hv)
    go _   (VStr s)            = pure s
    go _   other               = error
        ("listValToBS: expected a String, got: " <> showValForDebug other)

-- 'showB' was the original Phase 2.2 single-implementation shim;
-- replaced in Phase 2.3 by 'showDispatch reg' (Show-class registry
-- entry point), then removed entirely in the "minimum surface only"
-- cleanup — @show@ now resolves through the demand-driven
-- 'classMethodDispatcher' for the source-loaded @class Show a@ in
-- @GHC.Internal.Show@, with 'IHC.Scheduler.hostShowFallback'
-- delegating back to 'showValWith' above for placeholder cases.

-- | Build a cons-chain of VChar from a host 'String' (in IO — needs
-- to allocate thunks).
stringToListValIO :: String -> IO Val
stringToListValIO []     = pure (VCon "[]" [])
stringToListValIO (c:cs) = do
    hT   <- newWHNFThunk (VChar c)
    restV <- stringToListValIO cs
    tT   <- newWHNFThunk restV
    pure (VCon ":" [hT, tT])

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- 'putStrLnB' was removed in the "minimum surface only" cleanup —
-- 'putStrLn' is now interpreted from
-- ~/.cache/ihc/sources/base-4.19.0.0/System/IO.hs:282-283.
-- See the comment next to the (now-deleted) "putStrLn" entry in
-- 'builtins' above.

-- 'putStrB' / 'putCharB' were removed in the slice-2 cleanup —
-- 'putStr' and 'putChar' are now interpreted from
-- ~/.cache/ihc/sources/base-4.19.0.0/System/IO.hs:278 / :272.
-- See the comment next to the (now-deleted) "putStr"/"putChar" entries
-- in 'builtins' above.

-- 'printDispatch' was removed in the slice-4 cleanup — 'print' is now
-- interpreted from ~/.cache/ihc/sources/base-4.19.0.0/System/IO.hs:296-297
-- (`print x = putStrLn (show x)`).  Both the @putStrLn@ and the @show@
-- inside the source body resolve through the demand-driven Prelude
-- and 'classMethodDispatcher' respectively (see the @show@ omission
-- comment in 'builtins' above).

-- 'getLineB' was removed in the "minimum surface only" cleanup —
-- 'getLine' is now interpreted from
-- ~/.cache/ihc/sources/base-4.19.0.0/System/IO.hs:308-309.
-- See the comment next to the (now-deleted) "getLine" entry in
-- 'builtins' above.

-- | B.1: debug-only probe of the global superclass-relation map.
-- Takes a class name (as a [Char] list) and returns the list of
-- direct superclass names ([[Char]]).  Used by fixtures to verify
-- that @class Eq a => Ord a@ et al. are captured by the scanner.
classSupersProbeB :: IO Val
classSupersProbeB = pure $ VFun $ \aT -> pure $ VIO $ do
    av    <- force legacyHooks aT
    cls   <- valToString av
    supers <- IHC.Classes.lookupSuperclasses (BC.pack cls)
    -- Build a Haskell-level [String] cons list from the result.
    let buildList []     = pure (VCon "[]" [])
        buildList (n:ns) = do
            headV <- stringToListValIO (BC.unpack n)
            headT <- newWHNFThunk headV
            restV <- buildList ns
            restT <- newWHNFThunk restV
            pure (VCon ":" [headT, restT])
    buildList supers

-- | Dispatching @fmap@. Forces the container argument and looks up a
-- @(Functor, typeTagOf mv)@ entry in the 'ClassRegistry'. If one is
-- registered (either hand-written or synthesised from a @deriving
-- Functor@ clause), that instance's @fmap@ is applied. Otherwise we
-- fall back to the @VIO@-only behaviour of 'fmapB' — so existing IO
-- uses keep working, and a @fmap@ on a container type that truly has
-- no registered instance still produces a runtime error from the IO
-- path rather than silently misbehaving.
-- Legacy host-backed fmap dispatcher -- kept out of the registry as a
-- reference while completing the remaining class-method dispatcher
-- removals.
_fmapDispatch :: ClassRegistry -> IO Val
_fmapDispatch reg = pure $ VFun $ \ft -> pure $ VFun $ \mt -> do
    mv <- force legacyHooks mt
    let tag = typeTagOf mv
    mFmapMethod <- lookupInstanceMethod reg (BC.pack "Functor") tag (BC.pack "fmap") >>= forceInstanceMethod
    case mFmapMethod of
        Just fmapMethod -> do
            -- Re-supply the original thunks; the instance implementation
            -- is free to evaluate @mv@ lazily via its own pattern match.
            mT <- newWHNFThunk mv
            r1 <- apply legacyHooks fmapMethod ft
            apply legacyHooks r1 mT
        _ -> case mv of
            VIO _ -> pure $ VIO $ do
                v  <- runIOVal legacyHooks mv
                fv <- force legacyHooks ft
                vT <- newWHNFThunk v
                r  <- apply legacyHooks fv vT
                runIOVal legacyHooks r
            _ -> error
                ( "fmap: no Functor instance registered for type `"
                  <> BC.unpack tag <> "`" )

-- | @join mm = do { m <- mm; m }@.
-- 'joinB' was removed in slice 5b — 'join' is now interpreted from
-- ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Base.hs:1292-1293
-- ('join x = x >>= id'). See the comment next to the (now-deleted)
-- "join" entry in 'builtins' above.

-- 'voidB', 'firstFnB', 'secondFnB' removed — 'void', 'first',
-- 'second' graduated to pure source.  See the comment next to the
-- (now-deleted) builtins-table entries above.

-- runIOVal lives in 'IHC.Eval' (and now also covers STM, which used to
-- be a separate copy here).  We import it from there.

--------------------------------------------------------------------------------
-- MutVar# primops (Phase 3.6).
--
-- GHC.Prim has no .hs source; MutVar# is wired-in by the GHC build system.
-- GHC.ST and Data.STRef are source-loaded from base — they use these primops.
-- We back MutVar# with the existing PrimIORef (IORef Thunk) representation.
--
-- The GHC State# threading convention:
--   newMutVar# init s  = (# s', MutVar# ref #)
--   readMutVar# mv s   = (# s', val #)
--   writeMutVar# mv v s = (# s' #)
-- We erase the State# token and return/accept it as VUnit (or the interpreter's
-- unboxed-tuple convention).  The 'ST s' newtype wrapper in GHC.ST rewraps these.
--------------------------------------------------------------------------------

-- | @newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)@
-- Returns @(# s, MutVar# ref #)@ directly (no VIO wrapper) so that
-- source-loaded callers (GHC.STRef.newSTRef) can case-match on the result.
newMutVarB :: IO Val
newMutVarB = pure $ VFun $ \initThunk -> pure $ VFun $ \_st -> do
    rf <- newIORef initThunk
    stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
    refT <- newWHNFThunk (VPrimObj (PrimIORef rf))
    pure (VCon "(#,#)" [stT, refT])

-- | @readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)@
-- Returns the unboxed tuple directly (no VIO wrapper).
readMutVarB :: IO Val
readMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            vT  <- readIORef rf
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            pure (VCon "(#,#)" [stT, vT])
        _ -> error ("readMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @writeMutVar# :: MutVar# s a -> a -> State# s -> State# s@
-- Returns the new state token directly (no VIO wrapper).
writeMutVarB :: IO Val
writeMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \valThunk ->
               pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            writeIORef rf valThunk
            pure (VPrimObj PrimRealWorld)
        _ -> error ("writeMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, b #)@
atomicModifyMutVarB :: IO Val
atomicModifyMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                      pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force legacyHooks fThunk
            curT <- readIORef rf
            -- f cur returns a (a, b) pair
            res  <- apply legacyHooks fv curT
            resV <- runIOVal legacyHooks res
            case resV of
                VCon _ [newT, bT] -> do
                    writeIORef rf newT
                    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
                    pure (VCon "(#,#)" [stT, bT])
                _ -> error ("atomicModifyMutVar#: f did not return a pair: "
                            <> showValForDebug resV)
        _ -> error ("atomicModifyMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar2# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, a, (a, b) #)@
atomicModifyMutVar2B :: IO Val
atomicModifyMutVar2B = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                       pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force legacyHooks fThunk
            curT <- readIORef rf
            res  <- apply legacyHooks fv curT
            resV <- runIOVal legacyHooks res
            case resV of
                VCon _ [newT, bT] -> do
                    writeIORef rf newT
                    stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
                    pairT <- newWHNFThunk (VCon "(,)" [newT, bT])
                    pure (VCon "(#,,#)" [stT, curT, pairT])
                _ -> error ("atomicModifyMutVar2#: f did not return a pair: "
                            <> showValForDebug resV)
        _ -> error ("atomicModifyMutVar2#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)@
-- Returns @(# s, old, new #)@.
atomicModifyMutVarUB :: IO Val
atomicModifyMutVarUB = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                       pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force legacyHooks fThunk
            oldT <- readIORef rf
            new  <- apply legacyHooks fv oldT
            newT <- newWHNFThunk new
            writeIORef rf newT
            stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
            pure (VCon "(#,,#)" [stT, oldT, newT])
        _ -> error ("atomicModifyMutVar_#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicSwapMutVar# :: MutVar# s a -> a -> State# s -> (# State# s, a #)@
-- GHC.Prim has no .hs source; use the same IORef-backed MutVar# storage as
-- the other MutVar# primops and return the previous value.
atomicSwapMutVarB :: IO Val
atomicSwapMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \newThunk ->
                    pure $ VFun $ \_st -> do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            oldT <- readIORef rf
            writeIORef rf newThunk
            stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
            pure (VCon "(#,#)" [stT, oldT])
        _ -> error ("atomicSwapMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)@
-- Non-atomic CAS — always succeeds (returns 0# = success).
casMutVarB :: IO Val
casMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \_expectedThunk ->
             pure $ VFun $ \newThunk -> pure $ VFun $ \_st -> pure $ VIO $ do
    mvV <- force legacyHooks mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            writeIORef rf newThunk
            -- Return (# s, 0#, new #) — 0# means success
            stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
            zT   <- newWHNFThunk (VInt 0)
            pure (VCon "(#,,#)" [stT, zT, newThunk])
        _ -> error ("casMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @reallyUnsafePtrEquality# :: a -> b -> Int#@.
-- GHC.Prim.PtrEq has source for its typed wrappers; this is the raw
-- compiler primop they bottom out on.  Do not force arbitrary lifted
-- values: callers commonly use this only as an optimisation guard.
reallyUnsafePtrEqualityHashB :: IO Val
reallyUnsafePtrEqualityHashB = pure $ VFun $ \lhsT -> pure $ VFun $ \rhsT -> do
    same <- sameThunkObject lhsT rhsT
    pure (primBoolVal same)
  where
    sameThunkObject lhsT rhsT
        | lhsT == rhsT = pure True
        | otherwise = do
            lhs <- peekRuntimeObject 8 lhsT
            rhs <- peekRuntimeObject 8 rhsT
            case (lhs, rhs) of
                (Just lhsV, Just rhsV) -> pure (sameRuntimeObject lhsV rhsV)
                _ -> pure False

    peekRuntimeObject :: Int -> Thunk -> IO (Maybe Val)
    peekRuntimeObject 0 _ = pure Nothing
    peekRuntimeObject depth t = do
        state <- readIORef t
        case state of
            Evaluated v -> pure (Just v)
            Unevaluated (Closure env _ expr)
                | Just t' <- chaseExpr env expr
                , t' /= t -> peekRuntimeObject (depth - 1) t'
            _ -> pure Nothing

    chaseExpr env (EVar name) = lookupEnv name env
    chaseExpr env (ETyApp expr _) = chaseExpr env expr
    chaseExpr _ _ = Nothing

    sameRuntimeObject (VPrimObj (PrimIORef l)) (VPrimObj (PrimIORef r)) = l == r
    sameRuntimeObject (VPrimObj (PrimByteArray l)) (VPrimObj (PrimByteArray r)) = l == r
    sameRuntimeObject (VPrimObj (PrimArray l)) (VPrimObj (PrimArray r)) = l == r
    sameRuntimeObject (VPrimObj (PrimBoxedArray _ l)) (VPrimObj (PrimBoxedArray _ r)) = l == r
    sameRuntimeObject (VPrimObj (PrimMVar l)) (VPrimObj (PrimMVar r)) = l == r
    sameRuntimeObject (VPrimObj (PrimTVar l)) (VPrimObj (PrimTVar r)) = l == r
    sameRuntimeObject (VPrimObj (PrimThreadId l)) (VPrimObj (PrimThreadId r)) = l == r
    sameRuntimeObject (VPrimObj (PrimPtr l)) (VPrimObj (PrimPtr r)) = l == r
    sameRuntimeObject (VPrimObj (PrimForeignPtr l)) (VPrimObj (PrimForeignPtr r)) =
        unsafeForeignPtrToPtr l == unsafeForeignPtrToPtr r
    sameRuntimeObject (VPrimObj PrimRealWorld) (VPrimObj PrimRealWorld) = True
    sameRuntimeObject _ _ = False

mkWeakHashB :: IO Val
mkWeakHashB = pure $ VFun $ \_keyT -> pure $ VFun $ \valT -> pure $ VFun $ \_finalizerT -> pure $ VFun $ \_stT -> do
    valV <- force legacyHooks valT
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    -- Strong ref under interpretation: GC never drops keys, so the
    -- "weak" object is just the value itself.  deRefWeak# always succeeds.
    weakT <- newWHNFThunk valV
    pure (VCon "(#,#)" [stT, weakT])

mkWeakNoFinalizerHashB :: IO Val
mkWeakNoFinalizerHashB = pure $ VFun $ \_keyT -> pure $ VFun $ \valT -> pure $ VFun $ \_stT -> do
    valV <- force legacyHooks valT
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    weakT <- newWHNFThunk valV
    pure (VCon "(#,#)" [stT, weakT])

-- | @deRefWeak# :: Weak# v -> State# s -> (# State# s, Int#, v #)@.
-- Flag 0# = dead, non-zero = alive (see GHC.Internal.Weak.deRefWeak).
-- Our mkWeak* always keep the value alive, so flag is always 1#.
deRefWeakHashB :: IO Val
deRefWeakHashB = pure $ VFun $ \weakT -> pure $ VFun $ \_stT -> do
    valV <- force legacyHooks weakT
    stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
    flagT <- newWHNFThunk (VInt 1)
    valOut <- newWHNFThunk valV
    pure (VCon "(#,,#)" [stT, flagT, valOut])

-- | @finalizeWeak# :: Weak# v -> State# s
--                 -> (# State# s, Int#, State# s -> (# State# s, () #) #)@.
-- Flag 0# = already dead / no finalizer.  We never store finalizers on
-- the strong-ref Weak# representation, so always return 0#.
finalizeWeakHashB :: IO Val
finalizeWeakHashB = pure $ VFun $ \_weakT -> pure $ VFun $ \_stT -> do
    stT   <- newWHNFThunk (VPrimObj PrimRealWorld)
    flagT <- newWHNFThunk (VInt 0)
    -- Dummy finalizer thunk (unused when flag is 0#).
    unitT <- newWHNFThunk VUnit
    dummy <- newWHNFThunk (VFun $ \sT -> pure (VCon "(#,#)" [sT, unitT]))
    pure (VCon "(#,,#)" [stT, flagT, dummy])

--------------------------------------------------------------------------------
-- File IO primops.
--------------------------------------------------------------------------------

-- | Construct a source-loaded @FileHandle path (MVar Handle__)@ value.
-- The MVar keeps the host 'Handle' in @haDevice@ and exposes enough of
-- GHC's source-level @Handle__@ record shape for source handle helpers
-- to pattern-match on @Handle__ {..}@.  The buffers only carry enough
-- shape for source handle helpers to allocate their own spare buffers;
-- ordinary byte/char IO is still performed by host-backed primitives such
-- as 'hPutCharB' and 'hGetLineB'.
mkFileHandleVal :: String -> Handle -> IOMode -> IO Val
mkFileHandleVal path h mode = do
    pathT <- newWHNFThunk =<< stringToListValIO path
    handleState <- mkHandleStateVal h mode
    mv    <- newMVar handleState
    mvarT <- newWHNFThunk (VPrimObj (PrimMVar mv))
    pure (VCon "FileHandle" [pathT, mvarT])

mkHandleStateVal :: Handle -> IOMode -> IO Val
mkHandleStateVal h mode = do
    handleT     <- newWHNFThunk (VPrimObj (PrimHandle h))
    typeT       <- newWHNFThunk (handleTypeVal mode)
    byteBufT    <- newWHNFThunk =<< (mkEmptyReadBufferVal >>= mkIORefVal)
    modeT       <- newWHNFThunk (VCon "LineBuffering" [])
    unitT       <- newWHNFThunk VUnit
    lastBufT    <- newWHNFThunk =<< mkEmptyReadBufferVal
    lastDecodeT <- newWHNFThunk =<< mkIORefVal (VCon "(,)" [unitT, lastBufT])
    charBufT    <- newWHNFThunk =<< (mkEmptyReadBufferVal >>= mkIORefVal)
    spareBufsT  <- newWHNFThunk =<< mkIORefVal (VCon "BufferListNil" [])
    encoderT    <- newWHNFThunk nothingVal
    decoderT    <- newWHNFThunk nothingVal
    codecT      <- newWHNFThunk nothingVal
    inputNLT    <- newWHNFThunk newlineVal
    outputNLT   <- newWHNFThunk newlineVal
    otherSideT  <- newWHNFThunk nothingVal
    pure (VCon "Handle__"
        [ handleT
        , typeT
        , byteBufT
        , modeT
        , lastDecodeT
        , charBufT
        , spareBufsT
        , encoderT
        , decoderT
        , codecT
        , inputNLT
        , outputNLT
        , otherSideT
        ])

mkIORefVal :: Val -> IO Val
mkIORefVal v = do
    t <- newWHNFThunk v
    ref <- newIORef t
    pure (VPrimObj (PrimIORef ref))

handleTypeVal :: IOMode -> Val
handleTypeVal ReadMode      = VCon "ReadHandle" []
handleTypeVal WriteMode     = VCon "WriteHandle" []
handleTypeVal AppendMode    = VCon "AppendHandle" []
handleTypeVal ReadWriteMode = VCon "ReadWriteHandle" []

mkEmptyReadBufferVal :: IO Val
mkEmptyReadBufferVal = do
    fp      <- mallocForeignPtrBytes (handleBufferSize * 4)
    markWord8PtrRange (castPtr (unsafeForeignPtrToPtr fp)) (handleBufferSize * 4)
    rawT    <- newWHNFThunk =<< mkForeignPtrVal fp
    stateT  <- newWHNFThunk (VCon "ReadBuffer" [])
    -- GHC's source text output path asks for a spare char buffer sized
    -- from haCharBuffer.  A one-slot placeholder makes writeLines commit
    -- zero chars forever because it always keeps room for the next char.
    sizeT   <- newWHNFThunk (VInt (fromIntegral handleBufferSize))
    offsetT <- newWHNFThunk (VInt 0)
    leftT   <- newWHNFThunk (VInt 0)
    rightT  <- newWHNFThunk (VInt 0)
    pure (VCon "Buffer" [rawT, stateT, sizeT, offsetT, leftT, rightT])

handleBufferSize :: Int
handleBufferSize = 4096

-- | Flush a source-level @Buffer Word8@ through a synthetic host-backed
-- Handle__ device.  The source @BufferedIO FD@ instance cannot apply to
-- our @PrimHandle@ device, but the actual device write is RTS-exclusive:
-- copy the pending byte range to the host 'Handle' and return an emptied
-- source buffer.
flushHostHandleBuffer :: Val -> Val -> IO (Maybe Val)
flushHostHandleBuffer devV bufV = case (devV, bufV) of
    (VPrimObj (PrimHandle h), VCon "Buffer" [rawT, stateT, sizeT, offsetT, leftT, rightT]) -> do
        rawV   <- force legacyHooks rawT
        stateV <- force legacyHooks stateT
        sizeV  <- force legacyHooks sizeT
        offV   <- force legacyHooks offsetT
        leftV  <- force legacyHooks leftT
        rightV <- force legacyHooks rightT
        fp <- foreignPtrValToForeignPtr rawV
        case (leftV, rightV) of
            (VInt l, VInt r)
                | r > l -> withForeignPtr fp $ \p ->
                    hPutBuf h (castPtr (p `plusPtr` fromIntegral l))
                        (fromIntegral (r - l))
            _ -> pure ()
        hFlush h
        rawT'   <- newWHNFThunk rawV
        stateT' <- newWHNFThunk stateV
        sizeT'  <- newWHNFThunk sizeV
        offT'   <- newWHNFThunk offV
        leftT'  <- newWHNFThunk (VInt 0)
        rightT' <- newWHNFThunk (VInt 0)
        pure (Just (VCon "Buffer" [rawT', stateT', sizeT', offT', leftT', rightT']))
    _ -> pure Nothing

nothingVal :: Val
nothingVal = VCon "Nothing" []

newlineVal :: Val
newlineVal = VCon "LF" []

-- | Extract the host Handle from a source-loaded FileHandle/DuplexHandle
-- or a legacy VPrimObj PrimHandle.
requireHandle :: String -> Val -> IO Handle
requireHandle fnName v = case v of
    VCon "FileHandle" [_pathT, mvarT]    -> extractFromMVar mvarT
    VCon "DuplexHandle" [_pathT, mvarT, _] -> extractFromMVar mvarT
    VPrimObj (PrimHandle h)              -> pure h  -- legacy
    _ -> error (fnName <> ": not a Handle: " <> showValForDebug v)
  where
    extractFromMVar mvarT = do
        mvarV <- force legacyHooks mvarT
        case mvarV of
            VPrimObj (PrimMVar mv) -> do
                inner <- readMVar mv
                extractHandle inner
            VPrimObj (PrimHandle h) -> pure h
            _ -> error "requireHandle: not an MVar"
    extractHandle (VCon "Handle__" (hT:_)) = do
        hV <- force legacyHooks hT
        case hV of
            VPrimObj (PrimHandle h) -> pure h
            _ -> error "requireHandle: Handle__ field not a PrimHandle"
    extractHandle (VPrimObj (PrimHandle h)) = pure h
    extractHandle v' = error ("requireHandle: unexpected MVar contents: " <> showValForDebug v')

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
    pv  <- force legacyHooks a
    path <- valToString pv
    mv  <- force legacyHooks b
    let mode = ioModeFromVal mv
    h <- openFile path mode
    mkFileHandleVal path h mode

hCloseB :: IO Val
hCloseB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hClose"
    hClose h
    pure VUnit

hPutStrB :: IO Val
hPutStrB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hPutStr"
    sv <- force legacyHooks b
    s  <- valToString sv
    hPutStr h s
    pure VUnit

hPutCharB :: IO Val
hPutCharB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hPutChar"
    cv <- force legacyHooks b
    c <- case cv of
        VChar ch -> pure ch
        _        -> error ("hPutChar: not a Char: " <> showValForDebug cv)
    hPutChar h c
    pure VUnit

hPutStrLnB :: IO Val
hPutStrLnB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hPutStrLn"
    sv <- force legacyHooks b
    s  <- valToString sv
    hPutStrLn h s
    pure VUnit

hGetLineB :: IO Val
hGetLineB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hGetLine"
    s <- hGetLine h
    stringToListValIO s

hFlushB :: IO Val
hFlushB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hFlush"
    hFlush h
    pure VUnit

hSetBufferingB :: IO Val
hSetBufferingB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hSetBuffering"
    mv <- force legacyHooks b
    hSetBuffering h (bufferModeFromVal mv)
    pure VUnit

-- | @withFile path mode action@ — open, run @action@ with the host
-- 'Handle', close on success or exception.  Host-backed (Handle-device
-- carve-out): source @withFile@ is @bracket (openFile …) hClose@, but
-- the full source Handle/encoding layer is not modelled yet.  Lets
-- source-loaded 'writeFile' / 'appendFile' (and direct 'withFile'
-- users) bottom out here instead of a whole-file host shim.
withFileB :: IO Val
withFileB = pure $ VFun $ \pathT -> pure $ VFun $ \modeT -> pure $ VFun $ \kT -> pure $ VIO $ do
    pv   <- force legacyHooks pathT
    path <- valToString pv
    mv   <- force legacyHooks modeT
    let mode = ioModeFromVal mv
    kv   <- force legacyHooks kT
    CE.bracket
        (do
            h    <- openFile path mode
            hVal <- mkFileHandleVal path h mode
            pure (h, hVal))
        (\(h, _) -> hClose h)
        (\(_, hVal) -> do
            hT <- newWHNFThunk hVal
            r  <- apply legacyHooks kv hT
            runIOVal legacyHooks r)

-- | @hGetContents h@ — read the remainder of the handle as a String
-- ([Char]).  Host-backed (same Handle-device carve-out as 'hGetLine');
-- source-loaded 'readFile' / 'getContents' bottom out here.
hGetContentsB :: IO Val
hGetContentsB = pure $ VFun $ \a -> pure $ VIO $ do
    h <- force legacyHooks a >>= requireHandle "hGetContents"
    s <- hGetContents h
    stringToListValIO s

--------------------------------------------------------------------------------
-- Control flow.
--------------------------------------------------------------------------------

-- | @seq a b@: force @a@ to WHNF, then return @b@.
seqB :: IO Val
seqB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    _ <- force legacyHooks a
    force legacyHooks b

--------------------------------------------------------------------------------
-- Char / numeric conversions.
--------------------------------------------------------------------------------

ordB :: IO Val
ordB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VChar c -> pure (VInt (fromIntegral (ord c)))
        _ -> error ("ord: not a Char: " <> showValForDebug av)

chrB :: IO Val
chrB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VChar (chr (fromIntegral n)))
        _ -> error ("chr: not an Int: " <> showValForDebug av)

-- | @isTrue# :: Int# -> Bool@.  The result of @==#@ / @<#@ / @># etc.
-- is an @Int#@ with the convention that @1#@ = True and @0#@ = False.
-- @isTrue#@ is how Haskell-level code observes that bit.
isTrueHashB :: IO Val
isTrueHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt 0 -> pure (VCon (BC.pack "False") [])
        VInt _ -> pure (VCon (BC.pack "True")  [])
        _      -> error ("isTrue#: not an Int: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 1+2.A: BigNat# runtime representation + comparison primops
--------------------------------------------------------------------------------

-- | @bigNatFromWord# :: Word# -> BigNat#@ — first of the
-- ghc-bignum BigNat# primop suite (Phase 2.D conversion tranche),
-- landed early to enable the Phase 1 'PrimBigNat' runtime smoke
-- fixture.  Source-level definition lives in 'GHC.Num.BigNat' but
-- bottoms out in WordArray# / ByteArray# limb manipulation that
-- doesn't match IHC's chosen 'Natural'-backed runtime; host-shimming
-- the whole @bigNat*#@ family is intentional per
-- @plans/full-ghc-bignum-source-load.md@.
--
-- 'Word#' is stored as 'VInt' per IHC convention.  We reinterpret
-- the Int64 bits as 'Word' (handling the high-bit-set case) before
-- widening to host 'Natural'.
bigNatFromWordB :: IO Val
bigNatFromWordB = pure $ VFun $ \w -> do
    wv <- force legacyHooks w
    case wv of
        VInt n ->
            pure (VPrimObj (PrimBigNat (fromIntegral (fromIntegral n :: Word))))
        -- Word# literals like @0xFFFFFFFFFFFFFFFF##@ parse to VInteger
        -- because their value exceeds maxBound :: Int64.  Accept that
        -- shape too; the value is already an unsigned Natural in
        -- spirit, so just clamp/truncate to the low 64 bits.
        VInteger n -> pure (VPrimObj (PrimBigNat (fromInteger (n `mod` (1 `shiftL` 64)))))
        _ -> error ("bigNatFromWord#: not a Word#: " <> showValForDebug wv)

-- | Extract a host 'Natural' from a 'VPrimObj (PrimBigNat _)'
-- argument or fail with a context-tagged error.  Used by the
-- retained @bigNat*#@ family that still sits on the PrimBigNat
-- representation boundary.
extractBigNat :: String -> Val -> IO Natural
extractBigNat _   (VPrimObj (PrimBigNat n)) = pure n
extractBigNat ctx v = error (ctx <> ": not a BigNat#: " <> showValForDebug v)

-- | Count 64-bit limbs in a 'Natural'.  Zero is canonically
-- represented with size 0 in ghc-bignum.  Used by the retained
-- sizeofByteArray# PrimBigNat leaf so source-loaded wordArraySize#
-- observes the same limb count.
bigNatLimbCount :: Natural -> Int
bigNatLimbCount = go 0
  where
    go !i 0 = i
    go !i n = go (i + 1) (n `shiftR` 64)

-- | Phase 2.B: binary BigNat# arithmetic primop returning BigNat#.
-- Both args are 'VPrimObj (PrimBigNat _)'.
makeBigNatBinOp :: String -> (Natural -> Natural -> Natural) -> IO Val
makeBigNatBinOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat name av
    nb <- extractBigNat name bv
    pure (VPrimObj (PrimBigNat (op na nb)))

-- | Phase 2.B: @BigNat# -> Word# -> BigNat#@ primop.  The Word#
-- arg is reinterpreted from VInt's Int64 bits (matching
-- 'bigNatFromWord#') before applying the binary op over 'Natural'.
-- VInteger args (for Word# literals exceeding @maxBound :: Int64@)
-- are accepted and truncated to the low 64 bits, matching the
-- Word# semantics.
makeBigNatWordOp :: String -> (Natural -> Natural -> Natural) -> IO Val
makeBigNatWordOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat name av
    nb <- coerceWordArg name bv
    pure (VPrimObj (PrimBigNat (op na nb)))

-- | Coerce a 'Val' representing a @Word#@ argument to a host 'Natural'
-- magnitude (truncated to the low 64 bits).  Accepts:
--   * 'VInt n'      — standard Word# storage (Int64 bit-reinterpret)
--   * 'VInteger n'  — large hex/decimal Word# literals that exceed
--                     @maxBound :: Int64@
coerceWordArg :: String -> Val -> IO Natural
coerceWordArg _   (VInt w)     =
    pure (fromIntegral (fromIntegral w :: Word))
coerceWordArg _   (VInteger n) =
    pure (fromInteger (n `mod` (1 `shiftL` 64)))
coerceWordArg ctx v =
    error (ctx <> ": not a Word#: " <> showValForDebug v)

-- | Build an unboxed-sum value @(# … | … #)@ — see the @(#|#)@
-- constructor registration in 'builtinEnv'.  @tag@ is the 1-based
-- alternative index; @payload@ is that alternative's value.
mkUnboxedSum :: Int -> Val -> IO Val
mkUnboxedSum tag payload = do
    tagT <- newWHNFThunk (VInt (fromIntegral tag))
    pT   <- newWHNFThunk payload
    pure (VCon "(#|#)" [tagT, pT])

-- | The empty/left injection @(# (# #) | #)@ — alternative 1,
-- payload is the nullary unboxed tuple.
unboxedSumLeftUnit :: IO Val
unboxedSumLeftUnit = mkUnboxedSum 1 (VCon "(##)" [])

-- | @bigNatSub :: BigNat# -> BigNat# -> (# (# #) | BigNat# #)@.
-- ghc-bignum (BigNat.hs:546): @(# | a-b #)@ when @a >= b@,
-- else @(# (# #) | #)@ (would-underflow).  The @b == 0@ fast path
-- in the source is subsumed by @a >= b@ → @a - 0 == a@.
bigNatSubB :: IO Val
bigNatSubB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatSub" av
    nb <- extractBigNat "bigNatSub" bv
    if na >= nb
        then mkUnboxedSum 2 (VPrimObj (PrimBigNat (na - nb)))
        else unboxedSumLeftUnit

-- | @bigNatIsPowerOf2# :: BigNat# -> (# (# #) | Word# #)@.
-- ghc-bignum (BigNat.hs:135): @(# | k #)@ (k = exponent) when the
-- BigNat is exactly @2^k@ (k ≥ 0), else @(# (# #) | #)@ (incl. 0).
-- A 'Natural' is a power of two iff exactly one bit is set
-- (@popCount == 1@); the exponent is then the trailing-zero count.
bigNatIsPowerOf2HashB :: IO Val
bigNatIsPowerOf2HashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    n  <- extractBigNat "bigNatIsPowerOf2#" av
    if n > 0 && popCount n == 1
        then mkUnboxedSum 2 (VInt (fromIntegral (naturalCtz n)))
        else unboxedSumLeftUnit

-- | @bigNatQuotRem# :: BigNat# -> BigNat# -> (# BigNat#, BigNat# #)@.
-- The unboxed tuple is encoded as @VCon "(#,#)" [qT, rT]@ (same
-- shape as 'decodeDouble_Int64#').
bigNatQuotRemHashB :: IO Val
bigNatQuotRemHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatQuotRem#" av
    nb <- extractBigNat "bigNatQuotRem#" bv
    let (q, r) = na `quotRem` nb
    qT <- newWHNFThunk (VPrimObj (PrimBigNat q))
    rT <- newWHNFThunk (VPrimObj (PrimBigNat r))
    pure (VCon "(#,#)" [qT, rT])

-- | @bigNatRemWord# :: BigNat# -> Word# -> Word#@.  The remainder
-- fits in a Word# (since @r < divisor < 2^64@).
bigNatRemWordHashB :: IO Val
bigNatRemWordHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatRemWord#" av
    wNat <- coerceWordArg "bigNatRemWord#" bv
    pure (VInt (fromIntegral (na `rem` wNat)))

-- | @bigNatQuotRemWord# :: BigNat# -> Word# -> (# BigNat#, Word# #)@.
bigNatQuotRemWordHashB :: IO Val
bigNatQuotRemWordHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatQuotRemWord#" av
    wNat <- coerceWordArg "bigNatQuotRemWord#" bv
    let (q, r) = na `quotRem` wNat
    qT <- newWHNFThunk (VPrimObj (PrimBigNat q))
    rT <- newWHNFThunk (VInt (fromIntegral r))
    pure (VCon "(#,#)" [qT, rT])

-- | Phase 2.C: @a .&. ~b@ over 'Natural'.  Since 'Natural' has no
-- 'complement' (unsigned, unbounded), implemented as
-- @a `xor` (a .&. b)@ — which clears exactly the bits of @a@ that
-- are also set in @b@, matching @a .&. ~b@ semantics.
andNotNat :: Natural -> Natural -> Natural
andNotNat a b = a `xor` (a .&. b)

-- | Phase 2.C: shift primop builder.  @BigNat# -> Word# -> BigNat#@,
-- shift amount from the Word# arg interpreted as 'Int' via the
-- standard VInt-as-Word reinterpretation chain.  Used for
-- 'bigNatShiftL#' and 'bigNatShiftR#'.
makeBigNatShiftOp :: String -> (Natural -> Int -> Natural) -> IO Val
makeBigNatShiftOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat name av
    nb <- coerceWordArg name bv
    pure (VPrimObj (PrimBigNat (op na (fromIntegral nb))))

-- | @bigNatShiftRNeg# :: BigNat# -> Word# -> BigNat#@ — arithmetic
-- right-shift of a negative-magnitude BigNat, used by ghc-bignum's
-- @integerShiftR#@ for the IN branch.  Semantically:
-- @(-x) >> k = -(ceiling(x / 2^k))@ in two's complement, so this
-- primop returns @ceiling(magnitude / 2^k)@.  Implementation:
-- @(n + 2^k - 1) `shiftR` k@.
bigNatShiftRNegHashB :: IO Val
bigNatShiftRNegHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatShiftRNeg#" av
    nb <- coerceWordArg "bigNatShiftRNeg#" bv
    let k = fromIntegral nb :: Int
    pure (VPrimObj (PrimBigNat
        (if na == 0
            then 0
            else ((na + (1 `shiftL` k) - 1) `shiftR` k))))

-- | @bigNatPopCount# :: BigNat# -> Word#@ — population count of the
-- magnitude.
bigNatPopCountHashB :: IO Val
bigNatPopCountHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    n  <- extractBigNat "bigNatPopCount#" av
    pure (VInt (fromIntegral (popCount n)))

-- | @bigNatTestBit# :: BigNat# -> Word# -> Bool#@ — test if bit @i@
-- is set.  @Bool#@ encoded as VInt 1/0 per IHC convention.
bigNatTestBitHashB :: IO Val
bigNatTestBitHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatTestBit#" av
    nb <- coerceWordArg "bigNatTestBit#" bv
    pure (primBoolVal (testBit na (fromIntegral nb)))

-- | @bigNatBit# :: Word# -> BigNat#@ — returns @2^n@ as a BigNat.
-- ghc-bignum convention: the n-th bit is set, all others zero.
bigNatBitHashB :: IO Val
bigNatBitHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    nb <- coerceWordArg "bigNatBit#" av
    pure (VPrimObj (PrimBigNat (bit (fromIntegral nb))))

-- | @bigNatAndInt# :: BigNat# -> Int# -> BigNat#@ — bitwise AND with
-- a signed @Int#@.  Two's-complement semantics: for negative @i@,
-- the upper bits of @i@ are all 1, so AND-ing clears only the
-- lower @|i| - 1@ bits set in @i@'s complement.
--
-- For positive @i@: @na .&. fromIntegral i@.
-- For negative @i@: @na `andNotNat` (|i| - 1)@ — clear the bits of
--                   @na@ that correspond to set bits of @~i = |i| - 1@.
bigNatAndIntHashB :: IO Val
bigNatAndIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    na <- extractBigNat "bigNatAndInt#" av
    case bv of
        VInt i ->
            pure (VPrimObj (PrimBigNat
                (if i >= 0
                    then na .&. fromIntegral i
                    else na `andNotNat` fromIntegral (-i - 1))))
        _ -> error
            ("bigNatAndInt#: not an Int#: " <> showValForDebug bv)

-- | Phase 2.D: @bigNatEncodeDouble# :: BigNat# -> Int# -> Double#@
-- — returns @m * 2^e@ as a Double#.  Uses host 'encodeFloat'
-- which is the canonical Haskell primitive for this conversion.
bigNatEncodeDoubleHashB :: IO Val
bigNatEncodeDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    n  <- extractBigNat "bigNatEncodeDouble#" av
    case bv of
        VInt e ->
            pure (VFloat
                (encodeFloat (toInteger n) (fromIntegral e) :: Double))
        _ -> error
            ("bigNatEncodeDouble#: not an Int#: " <> showValForDebug bv)

-- | Phase 2.D: @integerFromBigNat# :: BigNat# -> Integer@ — wraps
-- a non-negative BigNat# in the canonical 'Integer' shape:
--   bn == 0                    -> VInt 0   (matches IS via Phase 1 bridge)
--   bn <= maxBound :: Int64    -> VInt n   (matches IS via Phase 1 bridge)
--   otherwise                  -> VCon "IP" [VPrimObj (PrimBigNat bn)]
-- Source-level pattern matches on @IS k@ / @IP bn@ fire through
-- the existing matchPat bridges.
integerFromBigNatHashB :: IO Val
integerFromBigNatHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    n  <- extractBigNat "integerFromBigNat#" av
    if n <= fromIntegral (maxBound :: Int64)
        then pure (VInt (fromIntegral n))
        else do
            t <- newWHNFThunk (VPrimObj (PrimBigNat n))
            pure (VCon "IP" [t])

-- | Phase 2.D: @integerFromBigNatNeg# :: BigNat# -> Integer@ —
-- wraps a BigNat# as a negative Integer:
--   bn == 0                       -> VInt 0
--   bn <= -minBound :: Int64      -> VInt (-n)
--   otherwise                     -> VCon "IN" [VPrimObj (PrimBigNat bn)]
--
-- Note: @-minBound :: Int64 == abs (toInteger (minBound :: Int64))
-- == 2^63 == maxBound :: Word63@.  We compare against that ceiling.
integerFromBigNatNegHashB :: IO Val
integerFromBigNatNegHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    n  <- extractBigNat "integerFromBigNatNeg#" av
    let absMinBoundInt64 = 1 + fromIntegral (maxBound :: Int64) :: Natural -- 2^63
    if n == 0
        then pure (VInt 0)
        else if n <= absMinBoundInt64
            then pure (VInt (fromInteger (negate (toInteger n))))
            else do
                t <- newWHNFThunk (VPrimObj (PrimBigNat n))
                pure (VCon "IN" [t])

-- | Phase 2.D: @integerFromBigNatSign# :: Int# -> BigNat# -> Integer@.
-- Dispatch on the sign Int#: 0 means positive, non-zero means negative.
-- Mirrors the source body at GHC.Num.Integer:106.
integerFromBigNatSignHashB :: IO Val
integerFromBigNatSignHashB = pure $ VFun $ \s -> pure $ VFun $ \a -> do
    sv <- force legacyHooks s
    av <- force legacyHooks a
    n  <- extractBigNat "integerFromBigNatSign#" av
    case sv of
        VInt 0 ->
            -- positive path: same as integerFromBigNat#
            if n <= fromIntegral (maxBound :: Int64)
                then pure (VInt (fromIntegral n))
                else do
                    t <- newWHNFThunk (VPrimObj (PrimBigNat n))
                    pure (VCon "IP" [t])
        VInt _ -> do
            -- negative path: same as integerFromBigNatNeg#
            let absMinBoundInt64 = 1 + fromIntegral (maxBound :: Int64) :: Natural
            if n == 0
                then pure (VInt 0)
                else if n <= absMinBoundInt64
                    then pure (VInt (fromInteger (negate (toInteger n))))
                    else do
                        t <- newWHNFThunk (VPrimObj (PrimBigNat n))
                        pure (VCon "IN" [t])
        _ -> error
            ("integerFromBigNatSign#: not an Int#: " <> showValForDebug sv)

-- | Phase 2.D: @integerToBigNatClamp# :: Integer -> BigNat#@ —
-- clamp an Integer down to a non-negative BigNat.  Negative
-- Integers (IS k where k < 0, or IN _) become 0.
integerToBigNatClampHashB :: IO Val
integerToBigNatClampHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n
            | n >= 0    -> pure (VPrimObj (PrimBigNat (fromIntegral n)))
            | otherwise -> pure (VPrimObj (PrimBigNat 0))
        VInteger n
            | n >= 0    -> pure (VPrimObj (PrimBigNat (fromInteger n)))
            | otherwise -> pure (VPrimObj (PrimBigNat 0))
        VPrimObj (PrimBigNat n) -> pure (VPrimObj (PrimBigNat n))
        VCon "IS" [t] -> do
            v <- force legacyHooks t
            case v of
                VInt n | n >= 0    -> pure (VPrimObj (PrimBigNat (fromIntegral n)))
                       | otherwise -> pure (VPrimObj (PrimBigNat 0))
                _ -> error ("integerToBigNatClamp#: IS field not Int: " <> showValForDebug v)
        VCon "IP" [t] -> do
            v <- force legacyHooks t
            case v of
                VPrimObj (PrimBigNat n) -> pure (VPrimObj (PrimBigNat n))
                VInteger n -> pure (VPrimObj (PrimBigNat (fromInteger n)))
                _ -> error ("integerToBigNatClamp#: IP field not BigNat: " <> showValForDebug v)
        VCon "IN" _ -> pure (VPrimObj (PrimBigNat 0))
        _ -> error
            ("integerToBigNatClamp#: not an Integer: " <> showValForDebug av)

-- | @bigNatZero# :: (# #) -> BigNat#@ — constant zero BigNat.  The
-- @(# #)@ argument is the unit unboxed tuple ghc-bignum uses to
-- force evaluation at use site (see "Note [Why (# #)?]" at
-- BigNat.hs:53).  IHC doesn't track unboxed-tuple types so we
-- just accept any single argument and discard it.
bigNatZeroHashB :: IO Val
bigNatZeroHashB = pure $ VFun $ \a -> do
    _ <- force legacyHooks a
    pure (VPrimObj (PrimBigNat 0))

-- | @bigNatOne# :: (# #) -> BigNat#@ — constant one BigNat.
bigNatOneHashB :: IO Val
bigNatOneHashB = pure $ VFun $ \a -> do
    _ <- force legacyHooks a
    pure (VPrimObj (PrimBigNat 1))

-- | Pure helper: count trailing zero bits of a 'Natural'.  Returns 0
-- for n = 0 (matches ghc-bignum's @bigNatCtz# 0 == 0##@).
naturalCtz :: Natural -> Int
naturalCtz 0 = 0
naturalCtz n = go 0 n
  where
    go !i k
      | k .&. 1 == 1 = i
      | otherwise    = go (i + 1) (k `shiftR` 1)

-- | Phase 4: @bigNatFromWord2# :: Word# -> Word# -> BigNat#@ —
-- construct a BigNat from a high/low Word# pair.  Used by
-- ghc-bignum's 'integerMul' overflow path to wrap the 128-bit
-- product of two Int#-fitting Integers when the result exceeds
-- Int range.  Implementation: @h * 2^64 + l@ as a Natural.
bigNatFromWord2HashB :: IO Val
bigNatFromWord2HashB = pure $ VFun $ \h -> pure $ VFun $ \l -> do
    hv <- force legacyHooks h; lv <- force legacyHooks l
    nh <- coerceWordArg "bigNatFromWord2#" hv
    nl <- coerceWordArg "bigNatFromWord2#" lv
    pure (VPrimObj (PrimBigNat ((nh `shiftL` 64) .|. nl)))

--------------------------------------------------------------------------------
-- Phase 2.8: RealWorld / State primops
--------------------------------------------------------------------------------

realWorldB :: IO Val
realWorldB = pure (VPrimObj PrimRealWorld)

-- | noDuplicate# :: State# s -> State# s
-- No-op in the interpreter; in GHC RTS this prevents thunk duplication.
noDuplicateB :: IO Val
noDuplicateB = pure $ VFun $ \_ -> pure (VPrimObj PrimRealWorld)

-- | seq# :: a -> State# s -> (# State# s, a #)
--
-- GHC.Prim primop with no Haskell implementation (GHC/PrimopWrappers.hs
-- only re-exports @GHC.Prim.seq#@). It forces @a@ to WHNF *in the IO
-- monad* (sequenced w.r.t. the state token) and returns it paired with
-- the threaded state. Backs source-loaded
-- @evaluate a = IO $ \s -> seq# a s@ in GHC.Internal.IO: the @IO@ data
-- constructor (registered above) applies this state-passing function to
-- a RealWorld token and unwraps the @(# s, a #)@ tuple. If forcing @a@
-- throws, the host exception propagates through the surrounding VIO,
-- giving @evaluate@ its exception-uncovering semantics.
seqHashB :: IO Val
seqHashB = pure $ VFun $ \aT -> pure $ VFun $ \sT -> do
    av <- force legacyHooks aT
    avT <- newWHNFThunk av
    pure (VCon "(#,#)" [sT, avT])

-- | touch# :: a -> State# s -> State# s
--
-- GHC.Prim primop with no Haskell implementation. It only communicates a
-- liveness edge to GHC's optimiser/RTS; IHC's host-backed ForeignPtr and
-- IORef values are already retained by the Val graph while evaluated, so the
-- runtime effect here is to return the state token unchanged.
touchHashB :: IO Val
touchHashB = pure $ VFun $ \aT -> pure $ VFun $ \sT -> do
    _ <- force legacyHooks aT
    force legacyHooks sT

-- | runRW# :: (State# RealWorld -> (# State# RealWorld, a #)) -> a
-- Apply the function to the RealWorld token, run any bridged VIO layer,
-- then extract and return the result component of the unboxed tuple.
runRWB :: IO Val
runRWB = pure $ VFun $ \ft -> do
    fv <- force legacyHooks ft
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    -- runRW# :: (State# RealWorld -> o) -> o
    -- Just apply the function to the RealWorld token and return the raw
    -- result.  The *caller* (e.g. runST) does any unboxed-tuple matching.
    resRaw <- apply legacyHooks fv rwT
    -- If the result is a VIO action (ST-VIO bridge), execute it so that
    -- the caller sees the concrete value / unboxed tuple.
    runIOVal legacyHooks resRaw

--------------------------------------------------------------------------------
-- Phase 2.8: boxing/unboxing constructors
--------------------------------------------------------------------------------

iHashB :: IO Val
iHashB = pure $ VFun $ \a -> force legacyHooks a

wHashB :: IO Val
wHashB = pure $ VFun $ \a -> force legacyHooks a

-- Box Word8 as VCon "W8#" so typeTagOf can map to "Word8" and Num
-- Word8 dispatch does not collapse to Num Int (bare VInt).  Without
-- this, @(5::Word8) - 48@ evaluates as Int (-43) instead of modular
-- Word8 213, and Word8 digit math on the warp request path is wrong.
w8HashB :: IO Val
w8HashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> do
            t <- newWHNFThunk (VInt (n .&. 0xff))
            pure (VCon "W8#" [t])
        VCon "W8#" _ -> pure av
        _ -> do
            t <- newWHNFThunk av
            pure (VCon "W8#" [t])

cHashB :: IO Val
cHashB = pure $ VFun $ \a -> force legacyHooks a

-- F# / D#: source-loaded Num Float / Num Double instance bodies wrap
-- unboxed primop results in F# x / D# x.  The runtime stores both
-- Float and Double as VFloat, so boxing is a no-op force.
fHashB :: IO Val
fHashB = pure $ VFun $ \a -> force legacyHooks a

dHashB :: IO Val
dHashB = pure $ VFun $ \a -> force legacyHooks a

--------------------------------------------------------------------------------
-- Phase 2.8: Addr# primitives
--------------------------------------------------------------------------------

nullAddrB :: IO Val
nullAddrB = pure (VPrimObj (PrimPtr nullPtr))

-- | Resolve any of the runtime shapes an @Addr#@ / @Ptr@-ish value
-- can take under interpretation down to a host 'Ptr'.
--
-- @plusAddr#@ / @minusAddr#@ are genuine 'GHC.Prim' primops with no
-- @.hs@ source — they are legitimately host-backed.  Source-loaded
-- 'Foreign.Ptr.plusPtr' / 'minusPtr' (no longer host-shimmed) bottom
-- out on these, and 'Data.ByteString.foldr' reaches them via
-- @unsafeForeignPtrToPtr (ForeignPtr fo _) = Ptr fo@ — so the
-- @Addr#@ argument can arrive as a 'PrimForeignPtr' (the extracted
-- finalizer-carrying allocation), a 'VCon \"Ptr\" [_]' wrapper
-- (source-loaded @Ptr@ ctor), or an integer/'VUnit' null alias, not
-- just a bare 'PrimPtr'.  This resolver subsumes the cross-rep
-- handling the removed @plusPtrCore@/@minusPtrCore@ shims carried
-- (see commit @f902b59@ for the parallel 'eqVals' fix), keeping
-- @plusPtr@/@minusPtr@ source-loaded per the CLAUDE.md
-- minimum-surface rule while letting the primop they bottom on cope
-- with the interpreter's actual value shapes.
valToHostPtr :: Val -> IO (Ptr Word8)
valToHostPtr v = case v of
    VPrimObj (PrimPtr p)        -> pure (castPtr p)
    VPrimObj (PrimForeignPtr f) -> pure (castPtr (unsafeForeignPtrToPtr f))
    VCon "Ptr" [t]              -> force legacyHooks t >>= valToHostPtr
    VInt n                      -> pure (intPtrToPtr (fromIntegral n))
    VInteger n                  -> pure (intPtrToPtr (fromIntegral n))
    VUnit                       -> pure nullPtr
    _ -> error ("Addr#: not an address: " <> showValForDebug v)

plusAddrB :: IO Val
plusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case bv of
        VInt n  -> go av n
        _ -> error ("plusAddr#: offset not an Int: " <> showValForDebug bv)
  where
    -- A 'PrimForeignPtr' flowing through must advance by @n@ bytes
    -- while KEEPING its finalizer: 'Data.ByteString.foldr' walks
    -- @go (p `plusPtr` 1)@ off a ForeignPtr-derived pointer and
    -- compares against @end@; if the offset is dropped, @p == end@
    -- never holds and the fold diverges -> heap exhaustion.  Unwrap
    -- nested source-loaded @VCon "Ptr" [_]@ wrappers first; for any
    -- other shape resolve to a host Ptr and advance that.
    go (VPrimObj (PrimForeignPtr f)) n =
        pure (VPrimObj (PrimForeignPtr (plusForeignPtr f (fromIntegral n))))
    go (VCon "Ptr" [t]) n = force legacyHooks t >>= \t' -> go t' n
    go other n = do
        p <- valToHostPtr other
        pure (VPrimObj (PrimPtr (plusPtr p (fromIntegral n))))

minusAddrB :: IO Val
minusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    p <- valToHostPtr av
    q <- valToHostPtr bv
    pure (VInt (fromIntegral (p `minusPtr` q)))

addr2IntB :: IO Val
addr2IntB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VPrimObj (PrimPtr p) ->
            pure (VInt (fromIntegral (FP.ptrToIntPtr p)))
        _ -> error ("addr2Int#: not a Ptr: " <> showValForDebug av)

-- | @indexCharOffAddr# :: Addr# -> Int# -> Char#@.
-- Genuine GHC.Prim leaf used by the source-loaded GHC.CString
-- unpacking loops. It reads one byte from the raw address and returns
-- the byte-valued Char# that GHC.CString then decodes as ASCII/UTF-8.
indexCharOffAddrHashB :: IO Val
indexCharOffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    p <- valToHostPtr addrV
    case idxV of
        VInt i -> do
            b <- peekElemOff (p :: Ptr Word8) (fromIntegral i)
            pure (VChar (chr (fromIntegral b)))
        _ -> error ("indexCharOffAddr#: offset not an Int: "
                    <> showValForDebug idxV)

-- | @indexWord8OffAddr# :: Addr# -> Int# -> Word8#@.
-- Genuine GHC.Prim leaf: unlike readWord8OffAddr#, this is a pure indexed
-- read and therefore has no State# argument or unboxed-tuple result.
indexWord8OffAddrHashB :: IO Val
indexWord8OffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    p <- valToHostPtr addrV
    case idxV of
        VInt i -> do
            b <- peekElemOff (p :: Ptr Word8) (fromIntegral i)
            pure (VInt (fromIntegral b))
        _ -> error ("indexWord8OffAddr#: offset not an Int: "
                    <> showValForDebug idxV)

-- | @setAddrRange# :: Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld@
-- Memset primop used by source-loaded @fillBytes@.  The state value
-- IS the side-effect carrier in our interpreter: the runIOVal IO
-- unwrapper now forces the state thunk to trigger primop side
-- effects (see IHC.Eval.runIOVal).  Returns the state value passed
-- in (semantically the "new" State#) so threading continues.
setAddrRangeB :: IO Val
setAddrRangeB = pure $ VFun $ \addrT -> pure $ VFun $ \sizeT -> pure $ VFun $ \byteT -> pure $ VFun $ \stT -> do
    addrV <- force legacyHooks addrT
    sizeV <- force legacyHooks sizeT
    byteV <- force legacyHooks byteT
    stV   <- force legacyHooks stT
    case (addrV, sizeV, byteV) of
        (VPrimObj (PrimPtr p), VInt size, VInt byte) -> do
            fillBytes (p :: Ptr Word8) (fromIntegral byte) (fromIntegral size)
            pure stV
        _ -> error ("setAddrRange#: bad args: addr=" <> showValForDebug addrV
                    <> " size=" <> showValForDebug sizeV
                    <> " byte=" <> showValForDebug byteV)

-- | @copyAddrToAddrNonOverlapping# :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld@
-- Memcpy primop used by source-loaded @copyBytes@ (and any other
-- caller of @Foreign.Marshal.Utils.copyBytes@).  Same state-threading
-- shape as 'setAddrRangeB': the side effect happens when the state
-- thunk is forced.  Note the GHC primop argument order is
-- @src dest size@, NOT the Haskell wrapper's @dest src size@.
copyAddrToAddrNonOverlappingB :: IO Val
copyAddrToAddrNonOverlappingB =
    pure $ VFun $ \srcT -> pure $ VFun $ \destT -> pure $ VFun $ \sizeT -> pure $ VFun $ \stT -> do
        srcV  <- force legacyHooks srcT
        destV <- force legacyHooks destT
        sizeV <- force legacyHooks sizeT
        stV   <- force legacyHooks stT
        case (srcV, destV, sizeV) of
            (VPrimObj (PrimPtr src), VPrimObj (PrimPtr dest), VInt size) -> do
                let n = fromIntegral size
                    destWord8 = dest :: Ptr Word8
                copyBytes destWord8 (src :: Ptr Word8) n
                markWord8PtrRange destWord8 n
                pure stV
            _ -> error ("copyAddrToAddrNonOverlapping#: bad args: src=" <> showValForDebug srcV
                        <> " dest=" <> showValForDebug destV
                        <> " size=" <> showValForDebug sizeV)

-- | @copyAddrToAddr# :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld@
-- Same as 'copyAddrToAddrNonOverlappingB' but allows overlapping
-- regions; used by source-loaded @moveBytes@.  We dispatch to the
-- host 'moveBytes' (memmove) rather than 'copyBytes' (memcpy) because
-- the GHC primop spec says regions MAY overlap.
copyAddrToAddrB :: IO Val
copyAddrToAddrB =
    pure $ VFun $ \srcT -> pure $ VFun $ \destT -> pure $ VFun $ \sizeT -> pure $ VFun $ \stT -> do
        srcV  <- force legacyHooks srcT
        destV <- force legacyHooks destT
        sizeV <- force legacyHooks sizeT
        stV   <- force legacyHooks stT
        case (srcV, destV, sizeV) of
            (VPrimObj (PrimPtr src), VPrimObj (PrimPtr dest), VInt size) -> do
                let n = fromIntegral size
                    destWord8 = dest :: Ptr Word8
                moveBytes destWord8 (src :: Ptr Word8) n
                markWord8PtrRange destWord8 n
                pure stV
            _ -> error ("copyAddrToAddr#: bad args: src=" <> showValForDebug srcV
                        <> " dest=" <> showValForDebug destV
                        <> " size=" <> showValForDebug sizeV)

-- | @word8ToWord# :: Word8# -> Word#@ — widening; no-op on our Val
-- (we represent both Word8# and Word# as 'VInt').
word8ToWordB :: IO Val
word8ToWordB = pure $ VFun $ \t -> force legacyHooks t

-- | @wordToWord8# :: Word# -> Word8#@ — narrowing to the low 8 bits.
wordToWord8B :: IO Val
wordToWord8B = pure $ VFun $ \t -> do
    v <- force legacyHooks t
    case v of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word8)))
        _      -> error ("wordToWord8#: bad arg: " <> showValForDebug v)

-- | @word32ToWord# :: Word32# -> Word#@ — widening; no-op on our Val.
-- Source-loaded @GHC.Internal.Word@ uses this for @Integral Word32@.
word32ToWordB :: IO Val
word32ToWordB = pure $ VFun $ \t -> force legacyHooks t

-- | @wordToWord32# :: Word# -> Word32#@ — narrowing to the low 32 bits.
wordToWord32B :: IO Val
wordToWord32B = pure $ VFun $ \t -> do
    v <- force legacyHooks t
    case v of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word32)))
        _      -> error ("wordToWord32#: bad arg: " <> showValForDebug v)

-- | @eqAddr#@ / @neAddr#@ / @ltAddr#@ / @leAddr#@ / @gtAddr#@ /
-- @geAddr#@ — host-backed comparison primops on the unboxed @Addr#@.
-- All six produce @Int#@ in source semantics (1 / 0); we map to the
-- isomorphic 'VInt' representation since 'isTrue#' converts to 'Bool'
-- via @tagToEnum#@ at the source level.
--
-- Cross-representation note: when one operand is a libffi-backed
-- 'VPrimObj (PrimPtr p)' and the other is a source-loaded
-- 'VCon "Ptr" [VInt n]' (the @Ptr 0@ pattern), we still want a
-- meaningful comparison.  These primops are reached through the
-- derived @instance Eq (Ptr a)@ body @isTrue# (eqAddr# a b)@, which
-- runs after 'matchPat' has bridged @PCon \"Ptr\" [_]@ to extract
-- both shapes — so by the time this builtin fires the two arguments
-- already share the @VPrimObj PrimPtr@ form.  We still tolerate
-- lingering 'VInt' / 'VInteger' / 'VUnit' inputs (the addr might be
-- @0@ from a literal, @nullPtr@ from a 'VUnit' alias, etc.) by
-- converting to the corresponding host @Ptr@ for the comparison.
addrCmpHashB :: (Ptr Word8 -> Ptr Word8 -> Bool) -> IO Val
addrCmpHashB cmp = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
    let p1 = ptrOf av
        p2 = ptrOf bv
    pure (VInt (if cmp p1 p2 then 1 else 0))
  where
    ptrOf (VPrimObj (PrimPtr p)) = p
    ptrOf (VInt n)               = FP.intPtrToPtr (fromIntegral n)
    ptrOf (VInteger n)           = FP.intPtrToPtr (fromIntegral n)
    ptrOf VUnit                  = nullPtr
    ptrOf other = error ("Addr# compare: not an address: "
                          <> showValForDebug other)

-- | @readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)@.
-- GHC.Prim raw-address access used by source-loaded GHC.Internal.Storable.
readIntOffAddrHashB :: IO Val
readIntOffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                      pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    case (addrV, idxV) of
        (VPrimObj (PrimPtr p), VInt i) -> do
            n   <- peekElemOff (castPtr p :: Ptr Int) (fromIntegral i)
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            nT  <- newWHNFThunk (VInt (fromIntegral n))
            pure (VCon "(#,#)" [stT, nT])
        _ -> error ("readIntOffAddr#: bad args: " <> showValForDebug addrV)

-- | @writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s@.
-- GHC.Prim raw-address access used by source-loaded GHC.Internal.Storable.
writeIntOffAddrHashB :: IO Val
writeIntOffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                       pure $ VFun $ \valT -> pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    valV  <- force legacyHooks valT
    case (addrV, idxV, valV) of
        (VPrimObj (PrimPtr p), VInt i, VInt n) -> do
            pokeElemOff (castPtr p :: Ptr Int) (fromIntegral i)
                        (fromIntegral n :: Int)
            pure (VPrimObj PrimRealWorld)
        _ -> error ("writeIntOffAddr#: bad args: " <> showValForDebug addrV)

-- | @readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word8# #)@.
-- GHC.Prim raw-address access used by source-loaded @Storable Word8@.
readWord8OffAddrHashB :: IO Val
readWord8OffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                        pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    case (addrV, idxV) of
        (VPrimObj (PrimPtr p), VInt i) -> do
            n   <- peekElemOff (p :: Ptr Word8) (fromIntegral i)
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            nT  <- newWHNFThunk (VInt (fromIntegral n))
            pure (VCon "(#,#)" [stT, nT])
        _ -> error ("readWord8OffAddr#: bad args: " <> showValForDebug addrV)

-- | @writeWord8OffAddr# :: Addr# -> Int# -> Word8# -> State# s -> State# s@.
-- GHC.Prim raw-address access used by source-loaded @Storable Word8@.
writeWord8OffAddrHashB :: IO Val
writeWord8OffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                         pure $ VFun $ \valT -> pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    valV  <- force legacyHooks valT
    case (addrV, idxV, valV) of
        (VPrimObj (PrimPtr p), VInt i, VInt n) -> do
            pokeElemOff (p :: Ptr Word8) (fromIntegral i)
                        (fromIntegral n :: Word8)
            pure (VPrimObj PrimRealWorld)
        (VPrimObj (PrimPtr p), VInt i, VInteger n) -> do
            pokeElemOff (p :: Ptr Word8) (fromIntegral i)
                        (fromInteger n :: Word8)
            pure (VPrimObj PrimRealWorld)
        _ -> error ("writeWord8OffAddr#: bad args: "
                    <> showValForDebug addrV <> ", "
                    <> showValForDebug idxV <> ", "
                    <> showValForDebug valV)

-- | @readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)@.
-- GHC.Prim raw-address access used by source-loaded @Storable Char@.
readWideCharOffAddrHashB :: IO Val
readWideCharOffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                           pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    case (addrV, idxV) of
        (VPrimObj (PrimPtr p), VInt i) -> do
            n   <- peekElemOff (castPtr p :: Ptr Word32) (fromIntegral i)
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            cT  <- newWHNFThunk (VChar (chr (fromIntegral n)))
            pure (VCon "(#,#)" [stT, cT])
        _ -> error ("readWideCharOffAddr#: bad args: " <> showValForDebug addrV)

-- | @writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s@.
-- GHC.Prim raw-address access used by source-loaded @Storable Char@.
writeWideCharOffAddrHashB :: IO Val
writeWideCharOffAddrHashB = pure $ VFun $ \addrT -> pure $ VFun $ \idxT ->
                            pure $ VFun $ \valT -> pure $ VFun $ \_stT -> do
    addrV <- force legacyHooks addrT
    idxV  <- force legacyHooks idxT
    valV  <- force legacyHooks valT
    case (addrV, idxV, valV) of
        (VPrimObj (PrimPtr p), VInt i, VChar c) -> do
            pokeElemOff (castPtr p :: Ptr Word32) (fromIntegral i)
                        (fromIntegral (ord c) :: Word32)
            pure (VPrimObj PrimRealWorld)
        _ -> error ("writeWideCharOffAddr#: bad args: "
                    <> showValForDebug addrV <> ", "
                    <> showValForDebug idxV <> ", "
                    <> showValForDebug valV)

--------------------------------------------------------------------------------
-- Phase 2.8: ForeignPtr
--------------------------------------------------------------------------------

mallocForeignPtrBytesB :: IO Val
mallocForeignPtrBytesB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force legacyHooks a
    case av of
        VInt n -> do
            fp <- mallocForeignPtrBytes (fromIntegral n)
            markWord8PtrRange (castPtr (unsafeForeignPtrToPtr fp)) (fromIntegral n)
            mkForeignPtrVal fp
        _ -> error ("mallocForeignPtrBytes: not an Int: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- ByteString representation helpers
--
-- A ByteString is represented at runtime as @VCon "BS" [ForeignPtr, length]@,
-- matching Data.ByteString.Internal.Type.ByteString's real constructor.
-- These helpers unpack that runtime representation for class-dispatch
-- bridges and RTS/FFI boundaries. They are not registered as Data.ByteString
-- API functions; pack/length/append/etc. source-load from bytestring.
--------------------------------------------------------------------------------

-- | Unpack a bytestring into its '(ForeignPtr Word8, Int)' payload.
bsValPayload :: Val -> IO (ForeignPtr Word8, Int)
bsValPayload v = case v of
    VCon "PS" [fpT, offT, lenT] -> do
        fpv  <- force legacyHooks fpT
        offv <- force legacyHooks offT
        lenv <- force legacyHooks lenT
        fp0  <- foreignPtrValToForeignPtr fpv
        case (offv, lenv) of
            (VInt off, VInt n) -> pure (plusForeignPtr fp0 (fromIntegral off), fromIntegral n)
            _ -> error ("PS: offset/length are not Ints: " <> showValForDebug offv <> ", " <> showValForDebug lenv)
    VCon "BS" [fpT, lenT] -> do
        fpv  <- force legacyHooks fpT
        lenv <- force legacyHooks lenT
        fp   <- foreignPtrValToForeignPtr fpv
        case lenv of
            VInt n -> pure (fp, fromIntegral n)
            _      -> error ("BS.length: second field is not Int: " <> showValForDebug lenv)
    _ -> do
        -- Optimistic OverloadedStrings bridge: without full typechecking,
        -- string literals can reach ByteString operations as [Char]. Treat
        -- those as Char8 bytes at the boundary where a ByteString payload is
        -- demanded.
        bs <- listValToBS v
        fp <- mallocForeignPtrBytes (BS.length bs)
        withForeignPtr fp $ \dst ->
            BS.useAsCStringLen bs $ \(src, n) ->
                copyBytes (castPtr dst) (castPtr src) n
        markWord8PtrRange (castPtr (unsafeForeignPtrToPtr fp)) (BS.length bs)
        pure (fp, BS.length bs)

{-# NOINLINE uniqueCounterRef #-}
uniqueCounterRef :: IORef Int64
uniqueCounterRef = unsafePerformIO (newIORef 0)

-- | Extract the underlying BS ByteString from a 'VCon "BS"' payload.
bsValToBS :: Val -> IO BS.ByteString
bsValToBS v = do
    (fp, len) <- bsValPayload v
    withForeignPtr fp $ \ptr ->
        BS.packCStringLen (castPtr ptr, len)

-- | Host content-equality for ByteString values, coercing [Char]/VStr
-- on either side (OverloadedStrings second arg of @bs == "server"@).
-- Used by class-method Eq dispatch for tag @BS@ so warp's
-- responseKeyIndex works without a prior warm-up of Eq ByteString.
eqByteStringHost :: Val -> Val -> IO Bool
eqByteStringHost a b = do
    ma <- asHostBS a
    mb <- asHostBS b
    pure (case (ma, mb) of
            (Just x, Just y) -> x == y
            _                -> False)
  where
    asHostBS v@(VCon "BS" _) = Just <$> bsValToBS v
    asHostBS v = do
        isChars <- isCharList v
        if isChars
            then Just . BC.pack <$> valToString v
            else pure Nothing

addForeignPtrFinalizerB :: IO Val
addForeignPtrFinalizerB = pure $ VFun $ \_finalizerT -> pure $ VFun $ \fpT -> pure $ VIO $ do
    fpv <- force legacyHooks fpT
    _ <- foreignPtrValToForeignPtr fpv
    pure VUnit

--------------------------------------------------------------------------------
-- Phase 2.8: Storable ops on Ptr
--------------------------------------------------------------------------------

peekB :: IO Val
peekB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force legacyHooks a
    p <- ptrValToPtr av
    mTyped <- lookupTypedHostPtr p
    case mTyped of
        Just "Word32" -> peekTypedWord32Ptr p
        _ -> do
            isWord8 <- isMarkedWord8Ptr p
            if isWord8
                then do
                    w <- peek (p :: Ptr Word8)
                    pure (VInt (fromIntegral w))
                else do
                    flags <- peekByteOff (castPtr p :: Ptr Word32) 0
                    family <- peekByteOff (castPtr p :: Ptr Word32) 4
                    socktype <- peekByteOff (castPtr p :: Ptr Word32) 8
                    protocol <- peekByteOff (castPtr p :: Ptr Word32) 12
                    if looksLikeAddrInfo flags family socktype protocol
                        then peekAddrInfoVal p flags family socktype protocol
                        else do
                            ptrWord <- peek (castPtr p :: Ptr Word64)
                            if ptrWord >= 4096
                                then pure (VPrimObj (PrimPtr (wordPtrToPtr ptrWord)))
                                else do
                                    w <- peek (p :: Ptr Word8)
                                    pure (VInt (fromIntegral w))

peekTypedWord32Ptr :: Ptr Word8 -> IO Val
peekTypedWord32Ptr p = do
    w <- peek (castPtr p :: Ptr Word32)
    t <- newWHNFThunk (VInt (fromIntegral w))
    pure (VCon "W32#" [t])

looksLikeAddrInfo :: Word32 -> Word32 -> Word32 -> Word32 -> Bool
looksLikeAddrInfo flags family socktype protocol =
    flags <= 0x1fff
    && family `elem` [0, 1, 2, 10, 30]  -- AF_INET6 is 10 on Linux, 30 on macOS
    && socktype <= 10
    && protocol <= 255

peekAddrInfoVal :: Ptr Word8 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Val
peekAddrInfoVal p flags family socktype protocol = do
    -- struct addrinfo layout differs in pointer field order:
    -- Linux:  ai_addr at 24, ai_canonname at 32, ai_next at 40.
    -- Darwin: ai_canonname at 24, ai_addr at 32, ai_next at 40.
    (addrPtrWord, canonPtrWord) <-
        if isDarwin
            then do
                canon <- peekByteOff (castPtr p :: Ptr Word64) 24
                addr <- peekByteOff (castPtr p :: Ptr Word64) 32
                pure (addr, canon)
            else do
                addr <- peekByteOff (castPtr p :: Ptr Word64) 24
                canon <- peekByteOff (castPtr p :: Ptr Word64) 32
                pure (addr, canon)
    flagsT <- newWHNFThunk =<< addrInfoFlagsVal flags
    familyT <- newWHNFThunk =<< oneFieldCon "Family" family
    socktypeT <- newWHNFThunk =<< oneFieldCon "SocketType" socktype
    protocolT <- newWHNFThunk (VInt (fromIntegral protocol))
    addrT <- newWHNFThunk =<< peekSockAddrVal (wordPtrToPtr addrPtrWord)
    canonT <- newWHNFThunk =<< maybeCStringVal canonPtrWord
    pure (VCon "AddrInfo" [flagsT, familyT, socktypeT, protocolT, addrT, canonT])

pokeAddrInfoHintsVal :: Ptr Word8 -> Val -> IO ()
pokeAddrInfoHintsVal p val = do
    (flags, family, socktype, protocol) <- addrInfoHintFields val
    fillBytes p 0 48
    pokeByteOff (castPtr p :: Ptr Word32) 0  flags
    pokeByteOff (castPtr p :: Ptr Word32) 4  family
    pokeByteOff (castPtr p :: Ptr Word32) 8  socktype
    pokeByteOff (castPtr p :: Ptr Word32) 12 protocol

addrInfoHintFields :: Val -> IO (Word32, Word32, Word32, Word32)
addrInfoHintFields val = case val of
    VCon "AddrInfo" (flagsT : familyT : socktypeT : protocolT : _) -> do
        flagsV    <- force legacyHooks flagsT
        familyV   <- force legacyHooks familyT
        socktypeV <- force legacyHooks socktypeT
        protocolV <- force legacyHooks protocolT
        flags    <- addrInfoFlagBits flagsV
        family   <- socketConInt familyV
        socktype <- socketConInt socktypeV
        protocol <- socketConInt protocolV
        pure (flags, family, socktype, protocol)
    _ -> pure (0, 0, 0, 0)

addrInfoFlagBits :: Val -> IO Word32
addrInfoFlagBits = go 0
  where
    go !acc v = case v of
        VCon "[]" []      -> pure acc
        VCon ":" [hT, tT] -> do
            hV <- force legacyHooks hT
            tV <- force legacyHooks tT
            go (acc .|. addrInfoFlagBit hV) tV
        _ -> pure acc

addrInfoFlagBit :: Val -> Word32
addrInfoFlagBit (VCon "AI_ADDRCONFIG" _) = 1024
addrInfoFlagBit (VCon "AI_ALL" _)        = 256
addrInfoFlagBit (VCon "AI_CANONNAME" _)  = 2
addrInfoFlagBit (VCon "AI_NUMERICHOST" _) = 4
addrInfoFlagBit (VCon "AI_NUMERICSERV" _) = 4096
addrInfoFlagBit (VCon "AI_PASSIVE" _)    = 1
addrInfoFlagBit (VCon "AI_V4MAPPED" _)   = 2048
addrInfoFlagBit _                        = 0

socketConInt :: Val -> IO Word32
socketConInt v = case v of
    VCon _ [innerT] -> do
        inner <- force legacyHooks innerT
        case inner of
            VInt n -> pure (fromIntegral n)
            _      -> socketConByName v
    VInt n -> pure (fromIntegral n)
    _      -> socketConByName v

socketConByName :: Val -> IO Word32
socketConByName (VCon n _) =
    case Map.lookup n socketConMap of
        Just v  -> pure v
        Nothing -> error ("socket con: unknown constructor " <> BC.unpack n)
socketConByName v =
    error ("socket con: not a constructor: " <> showValForDebug v)

socketConMap :: Map.Map ByteString Word32
socketConMap = Map.fromList
    [ ("NoSocketType", 0), ("Stream", 1), ("Datagram", 2)
    , ("Raw", 3), ("RDM", 4), ("SeqPacket", 5)
    , ("AF_UNSPEC", 0), ("AF_UNIX", 1), ("AF_INET", 2)
    , ("AF_INET6", if isDarwin then 30 else 10)
    ]

addrInfoFlagsVal :: Word32 -> IO Val
addrInfoFlagsVal flags =
    valsToConsList
        [ VCon name []
        | (name, bit) <-
            [ ("AI_ADDRCONFIG", 1024)
            , ("AI_ALL", 256)
            , ("AI_CANONNAME", 2)
            , ("AI_NUMERICHOST", 4)
            , ("AI_NUMERICSERV", 4096)
            , ("AI_PASSIVE", 1)
            , ("AI_V4MAPPED", 2048)
            ]
        , flags .&. bit /= 0
        ]

oneFieldCon :: Name -> Word32 -> IO Val
oneFieldCon name n = do
    t <- newWHNFThunk (VInt (fromIntegral n))
    pure (VCon name [t])

maybeCStringVal :: Word64 -> IO Val
maybeCStringVal 0 = pure (VCon "Nothing" [])
maybeCStringVal ptrWord = do
    s <- peekCAString (wordPtrToPtr ptrWord)
    strV <- stringToListValIO s
    strT <- newWHNFThunk strV
    pure (VCon "Just" [strT])

-- | Read @sa_family@ from a @struct sockaddr@, handling both macOS
-- (1-byte @sa_family@ at offset 1, after @sa_len@) and Linux
-- (2-byte @sa_family_t@ at offset 0, no @sa_len@).  On
-- little-endian, reading a Word16 at offset 0 on macOS gives
-- @sa_family << 8 | sa_len@ (≥ 256 for any family > 0 with
-- sa_len > 0); on Linux it gives the plain family value (≤ 255).
peekSaFamily :: Ptr Word8 -> IO Word8
peekSaFamily p = do
    raw16 <- peekByteOff p 0 :: IO Word16
    if raw16 <= 255
        then pure (fromIntegral raw16)         -- Linux: plain sa_family
        else peekByteOff p 1 :: IO Word8       -- macOS: sa_family after sa_len

-- | Detect macOS at runtime via System.Info.
isDarwin :: Bool
isDarwin = System.Info.os == "darwin"

peekSockAddrVal :: Ptr Word8 -> IO Val
peekSockAddrVal p
    | p == nullPtr = do
        portT <- newWHNFThunk (VInt 0)
        addrT <- newWHNFThunk (VInt 0)
        pure (VCon "SockAddrInet" [portT, addrT])
    | otherwise = do
        family <- peekSaFamily p
        case family of
            1 -> do
                s <- peekCAString (castPtr (p `plusPtr` 2))
                strV <- stringToListValIO s
                strT <- newWHNFThunk strV
                pure (VCon "SockAddrUnix" [strT])
            2 -> do
                portRaw <- peekByteOff (castPtr p :: Ptr Word16) 2 :: IO Word16
                addr <- peekByteOff (castPtr p :: Ptr Word32) 4 :: IO Word32
                let port = byteSwap16 portRaw  -- ntohs: PortNumber holds host order
                portT <- newWHNFThunk (VInt (fromIntegral port))
                addrT <- newWHNFThunk (VInt (fromIntegral addr))
                pure (VCon "SockAddrInet" [portT, addrT])
            -- AF_INET6: 30 on macOS, 10 on Linux
            f | f == 30 || f == 10 -> do
                portRaw <- peekByteOff (castPtr p :: Ptr Word16) 2 :: IO Word16
                flowRaw <- peekByteOff (castPtr p :: Ptr Word32) 4 :: IO Word32
                a0Raw <- peekByteOff (castPtr p :: Ptr Word32) 8 :: IO Word32
                a1Raw <- peekByteOff (castPtr p :: Ptr Word32) 12 :: IO Word32
                a2Raw <- peekByteOff (castPtr p :: Ptr Word32) 16 :: IO Word32
                a3Raw <- peekByteOff (castPtr p :: Ptr Word32) 20 :: IO Word32
                scope <- peekByteOff (castPtr p :: Ptr Word32) 24 :: IO Word32
                let port = byteSwap16 portRaw                     -- ntohs
                    flow = byteSwap32 flowRaw                     -- ntohl
                    a0 = byteSwap32 a0Raw                         -- ntohl each
                    a1 = byteSwap32 a1Raw
                    a2 = byteSwap32 a2Raw
                    a3 = byteSwap32 a3Raw
                portT <- newWHNFThunk (VInt (fromIntegral port))
                flowT <- newWHNFThunk (VInt (fromIntegral flow))
                addrT <- newWHNFThunk =<< fourTupleVal (map (VInt . fromIntegral) [a0, a1, a2, a3])
                scopeT <- newWHNFThunk (VInt (fromIntegral scope))
                pure (VCon "SockAddrInet6" [portT, flowT, addrT, scopeT])
            _ -> do
                portT <- newWHNFThunk (VInt 0)
                addrT <- newWHNFThunk (VInt 0)
                pure (VCon "SockAddrInet" [portT, addrT])

fourTupleVal :: [Val] -> IO Val
fourTupleVal [a, b, c, d] = do
    aT <- newWHNFThunk a
    bT <- newWHNFThunk b
    cT <- newWHNFThunk c
    dT <- newWHNFThunk d
    pure (VCon "(,,,)" [aT, bT, cT, dT])
fourTupleVal xs = error ("fourTupleVal: expected four fields, got " <> show (length xs))

valsToConsList :: [Val] -> IO Val
valsToConsList [] = pure (VCon "[]" [])
valsToConsList (x:xs) = do
    hT <- newWHNFThunk x
    tV <- valsToConsList xs
    tT <- newWHNFThunk tV
    pure (VCon ":" [hT, tT])

wordPtrToPtr :: Word64 -> Ptr a
wordPtrToPtr w = castPtr (intPtrToPtr (fromIntegral w :: IntPtr))

htons16 :: Word16 -> Word16
htons16 = byteSwap16

htonl32 :: Word32 -> Word32
htonl32 = byteSwap32

hostAddress6Fields :: Thunk -> IO (Int64, Int64, Int64, Int64)
hostAddress6Fields addrT = do
    addrV <- force legacyHooks addrT
    case addrV of
        VCon "(,,,)" [aT, bT, cT, dT] -> do
            a <- intField "HostAddress6.0" aT
            b <- intField "HostAddress6.1" bT
            c <- intField "HostAddress6.2" cT
            d <- intField "HostAddress6.3" dT
            pure (a, b, c, d)
        other -> error ("bind: bad HostAddress6: " <> showValForDebug other)

intField :: String -> Thunk -> IO Int64
intField label t = do
    v <- force legacyHooks t
    case v of
        VInt n -> pure n
        other  -> error (label <> " is not an Int: " <> showValForDebug other)

pokeB :: IO Val
pokeB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b
    p <- ptrValToPtr av
    case bv of
        VCon "AddrInfo" _ -> do
            pokeAddrInfoHintsVal p bv
            pure VUnit
        VInt n  -> do { poke (p :: Ptr Word8) (fromIntegral n); pure VUnit }
        -- Boxed Word8 (after W8# carrier for Num Word8 dispatch).
        VCon "W8#" [t] -> do
            inner <- force legacyHooks t
            case inner of
                VInt n -> do { poke (p :: Ptr Word8) (fromIntegral n); pure VUnit }
                _      -> error ("poke: W8# inner not an Int: " <> showValForDebug inner)
        -- Small-Integer 'IS' cross-rep: 'fromIntegral'-chained Word8
        -- values reaching @poke@ via the Char8 path arrive as
        -- 'VCon "IS" [VInt n]' rather than the bare 'VInt' the
        -- type-correct path would land — accept both shapes.
        VCon "IS" [t] -> do
            inner <- force legacyHooks t
            case inner of
                VInt n -> do { poke (p :: Ptr Word8) (fromIntegral n); pure VUnit }
                _      -> error ("poke: IS inner not an Int: " <> showValForDebug inner)
        -- Accept 'VChar' for the @Ptr Word8@ default.  ihc skips type
        -- checking, so a 'VChar' can flow into a Word8-typed slot
        -- when a user writes e.g. @Data.ByteString.pack "test"@ — a
        -- type error in stock Haskell, but ihc's optimistic semantics
        -- treats it as the Char8 path's @c2w = fromIntegral . ord@
        -- coercion and lets it through.  Landing the conversion at
        -- the FFI boundary lets source-loaded
        -- 'Data.ByteString.Internal.Type.unsafePackLenBytes' work
        -- without any Hackage-library shim (CLAUDE.md rule 4).
        VChar c -> do { poke (p :: Ptr Word8) (fromIntegral (fromEnum c)); pure VUnit }
        _ -> error ("poke: value not an Int or Char: " <> showValForDebug bv)

peekByteOffB :: IO Val
peekByteOffB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b
    p <- ptrValToPtr av
    case bv of
        VInt off -> do
            isWord8 <- isMarkedWord8Ptr p
            if isWord8
                then do
                    w <- peekByteOff (p :: Ptr Word8) (fromIntegral off)
                    pure (VInt (fromIntegral (w :: Word8)))
                else if off == 24 || off == 32 || off == 40
                then do
                    ptrWord <- peekByteOff (castPtr p :: Ptr Word64) (fromIntegral off) :: IO Word64
                    pure (VPrimObj (PrimPtr (castPtr (intPtrToPtr (fromIntegral ptrWord :: IntPtr)))))
                else do
                    w <- peekByteOff (p :: Ptr Word8) (fromIntegral off)
                    pure (VInt (fromIntegral (w :: Word8)))
        _ -> error ("peekByteOff: bad args: " <> showValForDebug av)

pokeByteOffB :: IO Val
pokeByteOffB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b; cv <- force legacyHooks c
    p <- ptrValToPtr av
    case bv of
        VInt off -> do
            handled <- pokeSockAddrByteOff p (fromIntegral off) cv
            if handled
                then pure VUnit
                else case cv of
                    VInt n -> do
                        pokeByteOff (p :: Ptr Word8) (fromIntegral off) (fromIntegral n :: Word8)
                        pure VUnit
                    -- Num Word8 boxes as VCon "W8#" after W8# carrier work.
                    VCon c [t]
                        | c == BC.pack "W8#" || c == BC.pack "W#"
                          || c == BC.pack "IS" -> do
                            inner <- force legacyHooks t
                            case inner of
                                VInt n -> do
                                    pokeByteOff (p :: Ptr Word8) (fromIntegral off)
                                        (fromIntegral n :: Word8)
                                    pure VUnit
                                _ -> error ("pokeByteOff: W8#/IS inner not Int: "
                                            <> showValForDebug inner)
                    _ | off == 24 || off == 32 || off == 40 -> do
                        ptr <- ptrValToPtr cv
                        pokeByteOff (castPtr p :: Ptr Word64) (fromIntegral off)
                            (fromIntegral (ptrToIntPtr (castPtr ptr)) :: Word64)
                        pure VUnit
                    _ -> error ("pokeByteOff: value not an Int: " <> showValForDebug cv)
        _ -> error ("pokeByteOff: bad args: " <> showValForDebug av)

pokeSockAddrByteOff :: Ptr Word8 -> Int -> Val -> IO Bool
pokeSockAddrByteOff p off v = do
    mLen <- lookupSockAddrBuffer p
    case (mLen, off) of
        (Just 16, 0) -> pokeWord16Raw p off v
        (Just 16, 2) -> pokePortNumber p off v
        (Just 16, 4) -> pokeWord32Raw p off v
        (Just 28, 0) -> pokeWord16Raw p off v
        (Just 28, 2) -> pokePortNumber p off v
        (Just 28, 4) -> pokeWord32Raw p off v
        (Just 28, 8) -> pokeIn6Addr p off v
        (Just 28, 24) -> pokeWord32Raw p off v
        _ -> pure False
  where
    pokeWord16Raw ptr byteOff val = do
        n <- intVal "pokeByteOff sockaddr Word16" val
        pokeByteOff (castPtr ptr :: Ptr Word16) byteOff (fromIntegral n :: Word16)
        pure True
    pokePortNumber ptr byteOff val = do
        n <- intVal "pokeByteOff sockaddr PortNumber" val
        pokeByteOff (castPtr ptr :: Ptr Word16) byteOff (htons16 (fromIntegral n) :: Word16)
        pure True
    pokeWord32Raw ptr byteOff val = do
        n <- intVal "pokeByteOff sockaddr Word32" val
        pokeByteOff (castPtr ptr :: Ptr Word32) byteOff (fromIntegral n :: Word32)
        pure True
    pokeIn6Addr ptr byteOff (VCon "In6Addr" [addrT]) = do
        (a0, a1, a2, a3) <- hostAddress6Fields addrT
        pokeNetwork32 ptr byteOff      a0
        pokeNetwork32 ptr (byteOff + 4)  a1
        pokeNetwork32 ptr (byteOff + 8)  a2
        pokeNetwork32 ptr (byteOff + 12) a3
        pure True
    pokeIn6Addr ptr byteOff other = do
        n <- intVal "pokeByteOff sockaddr In6Addr" other
        pokeByteOff (castPtr ptr :: Ptr Word32) byteOff (fromIntegral n :: Word32)
        pure True
    pokeNetwork32 ptr byteOff n =
        pokeByteOff (castPtr ptr :: Ptr Word32) byteOff (htonl32 (fromIntegral n) :: Word32)

intVal :: String -> Val -> IO Int64
intVal _ (VInt n) = pure n
intVal _ (VInteger n)
    | n >= toInteger (minBound :: Int64)
    , n <= toInteger (maxBound :: Int64) = pure (fromInteger n)
intVal label (VCon con [t])
    | bareRuntimeCon con `elem` numericRuntimeNewtypes =
        force legacyHooks t >>= intVal label
intVal label other =
    error (label <> ": expected numeric value, got " <> showValForDebug other)

bareRuntimeCon :: ByteString -> ByteString
bareRuntimeCon n = case BC.elemIndexEnd '.' n of
    Just idx -> BC.drop (idx + 1) n
    Nothing  -> n

numericRuntimeNewtypes :: [ByteString]
numericRuntimeNewtypes =
    map BC.pack
        [ "CSize", "CInt", "CLong", "CULong", "CUInt", "CChar", "CUChar"
        , "CShort", "CUShort", "CLLong", "CULLong"
        , "CSsize", "CSSize", "CIntPtr", "CUIntPtr", "CPtrdiff"
        , "Int8", "Int16", "Int32", "Int64"
        , "Word", "Word8", "Word16", "Word32", "Word64"
        , "PortNum", "Family"
        ]

--------------------------------------------------------------------------------
-- Phase 2.8: MutableByteArray# family (backed by IORef ByteString)
--------------------------------------------------------------------------------

newByteArrayB :: IO Val
newByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; stv <- force legacyHooks stT
    let n = case av of { VInt i -> fromIntegral i; _ -> 0 }
    ref  <- newIORef (BS.replicate n 0)
    baT  <- newWHNFThunk (VPrimObj (PrimByteArray ref))
    stT' <- newWHNFThunk stv
    pure (VCon "(#,#)" [stT', baT])

newPinnedByteArrayB :: IO Val
newPinnedByteArrayB = newByteArrayB

newAlignedPinnedByteArrayB :: IO Val
newAlignedPinnedByteArrayB = pure $ VFun $ \nT -> pure $ VFun $ \_alignT -> do
    newPinned <- newPinnedByteArrayB
    apply legacyHooks newPinned nT

writeWord8ArrayB :: IO Val
writeWord8ArrayB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b; cv <- force legacyHooks c; stv <- force legacyHooks stT
    case av of
        VPrimObj (PrimByteArray ref) ->
            case (bv, cv) of
                (VInt idx, VInt val) -> do
                    bs <- readIORef ref
                    let bs' = BS.concat
                                [ BS.take (fromIntegral idx) bs
                                , BS.singleton (fromIntegral val)
                                , BS.drop (fromIntegral idx + 1) bs
                                ]
                    writeIORef ref bs'
                    pure stv
                _ -> error "writeWord8Array#: bad index/val"
        _ -> error ("writeWord8Array#: not a MutableByteArray: " <> showValForDebug av)

readWord8ArrayB :: IO Val
readWord8ArrayB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b; stv <- force legacyHooks stT
    case av of
        VPrimObj (PrimByteArray ref) ->
            case bv of
                VInt idx -> do
                    bs <- readIORef ref
                    let w = fromIntegral (BS.index bs (fromIntegral idx)) :: Int64
                    wT   <- newWHNFThunk (VInt w)
                    stT' <- newWHNFThunk stv
                    pure (VCon "(#,#)" [stT', wT])
                _ -> error "readWord8Array#: bad index"
        _ -> error ("readWord8Array#: not a MutableByteArray: " <> showValForDebug av)

indexWord8ArrayB :: IO Val
indexWord8ArrayB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case av of
        VPrimObj (PrimByteArray ref) ->
            case bv of
                VInt idx -> do
                    bs <- readIORef ref
                    pure (VInt (fromIntegral (BS.index bs (fromIntegral idx))))
                _ -> error "indexWord8Array#: bad index"
        _ -> error ("indexWord8Array#: not a MutableByteArray: " <> showValForDebug av)

indexWordArrayHashB :: IO Val
indexWordArrayHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    idx <- case bv of
        VInt i | i >= 0 -> pure i
        _ -> error ("indexWordArray#: not a non-negative Int#: " <> showValForDebug bv)
    case av of
        VPrimObj (PrimBigNat n) ->
            pure (VInt (word64ToInt64 (bigNatWordLimbAt n idx)))
        VPrimObj (PrimByteArray ref) -> do
            bs <- readIORef ref
            pure (VInt (word64ToInt64 (byteStringWord64At bs idx)))
        _ -> error ("indexWordArray#: not a WordArray#: " <> showValForDebug av)

indexIntArrayHashB :: IO Val
indexIntArrayHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    idx <- case bv of
        VInt i | i >= 0 -> pure i
        _ -> error ("indexIntArray#: not a non-negative Int#: " <> showValForDebug bv)
    case av of
        VPrimObj (PrimBigNat n) ->
            pure (VInt (word64ToInt64 (bigNatWordLimbAt n idx)))
        VPrimObj (PrimByteArray ref) -> do
            bs <- readIORef ref
            pure (VInt (word64ToInt64 (byteStringWord64At bs idx)))
        _ -> error ("indexIntArray#: not a IntArray#: " <> showValForDebug av)

bigNatWordLimbAt :: Natural -> Int64 -> Word64
bigNatWordLimbAt n idx =
    fromIntegral (n `shiftR` (64 * fromIntegral idx))

byteStringWord64At :: ByteString -> Int64 -> Word64
byteStringWord64At bs idx
    | off < 0 || off + 8 > BS.length bs =
        error "indexWordArray#: index out of bounds"
    | otherwise =
        foldl
            (\acc (shiftBy, byte) ->
                acc .|. (fromIntegral byte `shiftL` shiftBy))
            0
            (zip [0,8..56] (BS.unpack (BS.take 8 (BS.drop off bs))))
  where
    off = fromIntegral idx * 8

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral

unsafeFreezeByteArrayB :: IO Val
unsafeFreezeByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; stv <- force legacyHooks stT
    case av of
        VPrimObj (PrimByteArray _) -> do
            aT   <- newWHNFThunk av
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', aT])
        _ -> error ("unsafeFreezeByteArray#: not a MutableByteArray: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Boxed Array#/MutableArray# family (backed by IORef [Thunk])
--------------------------------------------------------------------------------

newArrayHashB :: IO Val
newArrayHashB = pure $ VFun $ \nT -> pure $ VFun $ \initT -> pure $ VFun $ \stT -> do
    nv <- force legacyHooks nT
    stv <- force legacyHooks stT
    let n = case nv of
            VInt i -> max 0 (fromIntegral i)
            _      -> 0
    ref <- newIORef (replicate n initT)
    arrT <- newWHNFThunk (VPrimObj (PrimArray ref))
    stT' <- newWHNFThunk stv
    pure (VCon "(#,#)" [stT', arrT])

writeArrayHashB :: IO Val
writeArrayHashB = pure $ VFun $ \arrT -> pure $ VFun $ \idxT -> pure $ VFun $ \valT -> pure $ VFun $ \stT -> do
    arrV <- force legacyHooks arrT
    idxV <- force legacyHooks idxT
    stv <- force legacyHooks stT
    case (arrV, idxV) of
        (VPrimObj (PrimArray ref), VInt idx) -> do
            cells <- readIORef ref
            let i = fromIntegral idx
            if i < 0 || i >= length cells
                then error ("writeArray#: index out of bounds: " <> show idx)
                else do
                    writeIORef ref (replaceAt i valT cells)
                    pure stv
        _ -> error "writeArray#: bad args"

readArrayHashB :: IO Val
readArrayHashB = pure $ VFun $ \arrT -> pure $ VFun $ \idxT -> pure $ VFun $ \stT -> do
    arrV <- force legacyHooks arrT
    idxV <- force legacyHooks idxT
    stv <- force legacyHooks stT
    case (arrV, idxV) of
        (VPrimObj (PrimArray ref), VInt idx) -> do
            cell <- readArrayCell ref idx "readArray#"
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', cell])
        _ -> error "readArray#: bad args"

indexArrayHashB :: IO Val
indexArrayHashB = pure $ VFun $ \arrT -> pure $ VFun $ \idxT -> do
    arrV <- force legacyHooks arrT
    idxV <- force legacyHooks idxT
    case (arrV, idxV) of
        (VPrimObj (PrimArray ref), VInt idx) -> do
            cell <- readArrayCell ref idx "indexArray#"
            pure (VCon "(##)" [cell])
        _ -> error "indexArray#: bad args"

unsafeFreezeArrayHashB :: IO Val
unsafeFreezeArrayHashB = pure $ VFun $ \arrT -> pure $ VFun $ \stT -> do
    arrV <- force legacyHooks arrT
    stv <- force legacyHooks stT
    case arrV of
        VPrimObj (PrimArray _) -> do
            arrT' <- newWHNFThunk arrV
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', arrT'])
        _ -> error ("unsafeFreezeArray#: not a MutableArray#: " <> showValForDebug arrV)

unsafeThawArrayHashB :: IO Val
unsafeThawArrayHashB = unsafeFreezeArrayHashB

sizeofArrayHashB :: IO Val
sizeofArrayHashB = pure $ VFun $ \arrT -> do
    arrV <- force legacyHooks arrT
    case arrV of
        VPrimObj (PrimArray ref) -> VInt . fromIntegral . length <$> readIORef ref
        _ -> error ("sizeofArray#: not an Array#: " <> showValForDebug arrV)

sizeofMutableArrayHashB :: IO Val
sizeofMutableArrayHashB = sizeofArrayHashB

readArrayCell :: IORef [Thunk] -> Int64 -> String -> IO Thunk
readArrayCell ref idx label = do
    cells <- readIORef ref
    let i = fromIntegral idx
    if i < 0 || i >= length cells
        then error (label <> ": index out of bounds: " <> show idx)
        else pure (cells !! i)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs =
    let (prefix, suffix) = splitAt i xs
    in case suffix of
        []       -> xs
        (_:rest) -> prefix ++ x : rest

byteArrayContentsB :: IO Val
byteArrayContentsB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VPrimObj (PrimByteArray ref) -> do
            bs <- readIORef ref
            p <- mallocBytes (max 1 (BS.length bs))
            BS.useAsCStringLen bs $ \(src, len) ->
                copyBytes (castPtr p) (castPtr src) len
            pure (VPrimObj (PrimPtr p))
        _ -> error ("byteArrayContents#: not a ByteArray: " <> showValForDebug av)

mutableByteArrayContentsB :: IO Val
mutableByteArrayContentsB = byteArrayContentsB

getSizeofMutableByteArrayB :: IO Val
getSizeofMutableByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; stv <- force legacyHooks stT
    case av of
        VPrimObj (PrimByteArray ref) -> do
            bs   <- readIORef ref
            nT   <- newWHNFThunk (VInt (fromIntegral (BS.length bs)))
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', nT])
        _ -> error ("getSizeofMutableByteArray#: not a MutableByteArray: " <> showValForDebug av)

sizeofByteArrayB :: IO Val
sizeofByteArrayB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VPrimObj (PrimByteArray ref) -> do
            bs <- readIORef ref
            pure (VInt (fromIntegral (BS.length bs)))
        VPrimObj (PrimBigNat n) ->
            pure (VInt (fromIntegral (8 * bigNatLimbCount n)))
        _ -> error ("sizeofByteArray#: not a ByteArray: " <> showValForDebug av)

resizeMutableByteArrayB :: IO Val
resizeMutableByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b; stv <- force legacyHooks stT
    case (av, bv) of
        (VPrimObj (PrimByteArray ref), VInt n) -> do
            bs <- readIORef ref
            let newLen = max 0 (fromIntegral n)
                oldLen = BS.length bs
                bs'
                    | newLen <= oldLen = BS.take newLen bs
                    | otherwise = bs <> BS.replicate (newLen - oldLen) 0
            writeIORef ref bs'
            baT  <- newWHNFThunk av
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', baT])
        _ -> error "resizeMutableByteArray#: bad args"

shrinkMutableByteArrayB :: IO Val
shrinkMutableByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b; stv <- force legacyHooks stT
    case (av, bv) of
        (VPrimObj (PrimByteArray ref), VInt n) -> do
            bs <- readIORef ref
            writeIORef ref (BS.take (max 0 (fromIntegral n)) bs)
            pure stv
        _ -> error "shrinkMutableByteArray#: bad args"

setByteArrayB :: IO Val
setByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force legacyHooks a; offV <- force legacyHooks b; lenV <- force legacyHooks c; valV <- force legacyHooks d; stv <- force legacyHooks stT
    case (av, offV, lenV, valV) of
        (VPrimObj (PrimByteArray ref), VInt off, VInt len, VInt val) -> do
            bs <- readIORef ref
            let start = max 0 (fromIntegral off)
                count = max 0 (fromIntegral len)
                fill  = BS.replicate count (fromIntegral val)
                bs'   = BS.concat
                    [ BS.take start bs
                    , fill
                    , BS.drop (start + count) bs
                    ]
            writeIORef ref bs'
            pure stv
        _ -> error "setByteArray#: bad args"

copyMutableByteArrayB :: IO Val
copyMutableByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \e -> pure $ VFun $ \stT -> pure $ VIO $ do
    srcV <- force legacyHooks a; srcOffV <- force legacyHooks b; dstV <- force legacyHooks c
    dstOffV <- force legacyHooks d; lenV <- force legacyHooks e; stv <- force legacyHooks stT
    case (srcV, srcOffV, dstV, dstOffV, lenV) of
        (VPrimObj (PrimByteArray srcRef), VInt srcOff, VPrimObj (PrimByteArray dstRef), VInt dstOff, VInt len) -> do
            srcBs <- readIORef srcRef
            dstBs <- readIORef dstRef
            let srcStart = max 0 (fromIntegral srcOff)
                dstStart = max 0 (fromIntegral dstOff)
                count    = max 0 (fromIntegral len)
                chunk    = BS.take count (BS.drop srcStart srcBs)
                dstBs'   = BS.concat
                    [ BS.take dstStart dstBs
                    , chunk
                    , BS.drop (dstStart + count) dstBs
                    ]
            writeIORef dstRef dstBs'
            pure stv
        _ -> error "copyMutableByteArray#: bad args"

copyByteArrayB :: IO Val
copyByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \e -> pure $ VFun $ \stT -> pure $ VIO $ do
    srcV <- force legacyHooks a; srcOffV <- force legacyHooks b; dstV <- force legacyHooks c
    dstOffV <- force legacyHooks d; lenV <- force legacyHooks e; stv <- force legacyHooks stT
    case (srcV, srcOffV, dstV, dstOffV, lenV) of
        (VPrimObj (PrimByteArray srcRef), VInt srcOff, VPrimObj (PrimByteArray dstRef), VInt dstOff, VInt len) -> do
            srcBs <- readIORef srcRef
            dstBs <- readIORef dstRef
            let srcStart = max 0 (fromIntegral srcOff)
                dstStart = max 0 (fromIntegral dstOff)
                count    = max 0 (fromIntegral len)
                chunk    = BS.take count (BS.drop srcStart srcBs)
                dstBs'   = BS.concat
                    [ BS.take dstStart dstBs
                    , chunk
                    , BS.drop (dstStart + count) dstBs
                    ]
            writeIORef dstRef dstBs'
            pure stv
        _ -> error "copyByteArray#: bad args"

copyAddrToByteArrayB :: IO Val
copyAddrToByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \stT -> pure $ VIO $ do
    srcV    <- force legacyHooks a; baV <- force legacyHooks b
    dstOffV <- force legacyHooks c; lenV <- force legacyHooks d; stv <- force legacyHooks stT
    case (srcV, baV, dstOffV, lenV) of
        (VPrimObj (PrimPtr src), VPrimObj (PrimByteArray ref), VInt dstOff, VInt len) -> do
            bs    <- readIORef ref
            chunk <- BS.packCStringLen (castPtr src, fromIntegral len)
            let bs' = BS.concat
                    [ BS.take (fromIntegral dstOff) bs
                    , chunk
                    , BS.drop (fromIntegral dstOff + fromIntegral len) bs
                    ]
            writeIORef ref bs'
            pure stv
        _ -> error "copyAddrToByteArray#: bad args"

copyByteArrayToAddrB :: IO Val
copyByteArrayToAddrB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \stT -> pure $ VIO $ do
    baV     <- force legacyHooks a; srcOffV <- force legacyHooks b
    dstV    <- force legacyHooks c; lenV    <- force legacyHooks d; stv <- force legacyHooks stT
    case (baV, srcOffV, dstV, lenV) of
        (VPrimObj (PrimByteArray ref), VInt srcOff, VPrimObj (PrimPtr dst), VInt len) -> do
            bs <- readIORef ref
            let chunk = BS.take (fromIntegral len) (BS.drop (fromIntegral srcOff) bs)
            BS.useAsCStringLen chunk $ \(src, _n) ->
                copyBytes (castPtr dst) (castPtr src :: Ptr Word8) (fromIntegral len)
            pure stv
        _ -> error "copyByteArrayToAddr#: bad args"

compareByteArraysB :: IO Val
compareByteArraysB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \e -> do
    aV <- force legacyHooks a; bV <- force legacyHooks b; cV <- force legacyHooks c; dV <- force legacyHooks d; eV <- force legacyHooks e
    case (aV, bV, cV, dV, eV) of
        (VPrimObj (PrimByteArray lhsRef), VInt lhsOff, VPrimObj (PrimByteArray rhsRef), VInt rhsOff, VInt len) -> do
            lhsBs <- readIORef lhsRef
            rhsBs <- readIORef rhsRef
            let count = max 0 (fromIntegral len)
                lhsChunk = BS.take count (BS.drop (max 0 (fromIntegral lhsOff)) lhsBs)
                rhsChunk = BS.take count (BS.drop (max 0 (fromIntegral rhsOff)) rhsBs)
                cmp = case compare lhsChunk rhsChunk of
                    LT -> -1
                    EQ -> 0
                    GT -> 1
            pure (VInt cmp)
        _ -> error "compareByteArrays#: bad args"

--------------------------------------------------------------------------------
-- Phase 2.8: buffered I/O
--------------------------------------------------------------------------------

hPutBufB :: IO Val
hPutBufB = pure $ VFun $ \hT -> pure $ VFun $ \pT -> pure $ VFun $ \nT -> pure $ VIO $ do
    hv <- force legacyHooks hT; pv <- force legacyHooks pT; nv <- force legacyHooks nT
    h  <- requireHandle "hPutBuf" hv
    p <- ptrValToPtr pv
    case nv of
        VInt n -> do
            hPutBuf h (castPtr p) (fromIntegral n)
            pure VUnit
        _ -> error "hPutBuf: bad args"

--------------------------------------------------------------------------------
-- Phase 2.8: Int/Word coercions + bit ops
--------------------------------------------------------------------------------

int2WordB :: IO Val
int2WordB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word64)))
        _      -> force legacyHooks a

word2IntB :: IO Val
word2IntB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word64)))
        _      -> force legacyHooks a

orHashB :: IO Val
orHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .|. y))
        _ -> error ("or#: bad args: " <> showValForDebug av)

andHashB :: IO Val
andHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .&. y))
        _ -> error ("and#: bad args: " <> showValForDebug av)

xorHashB :: IO Val
xorHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `xor` y))
        _ -> error ("xor#: bad args: " <> showValForDebug av)

notHashB :: IO Val
notHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt x -> pure (VInt (complement x))
        _      -> error ("not#: bad arg: " <> showValForDebug av)

uncheckedShiftLB :: IO Val
uncheckedShiftLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftL` fromIntegral n))
        _ -> error ("uncheckedShiftL#: bad args: " <> showValForDebug av)

uncheckedShiftRLB :: IO Val
uncheckedShiftRLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) ->
            pure (VInt (fromIntegral (fromIntegral x `shiftR` fromIntegral n :: Word64)))
        _ -> error ("uncheckedShiftRL#: bad args: " <> showValForDebug av)

uncheckedIShiftRAB :: IO Val
uncheckedIShiftRAB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftR` fromIntegral n))
        _ -> error ("uncheckedIShiftRA#: bad args: " <> showValForDebug av)

plusIntHashB :: IO Val
plusIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x + y))
        _ -> error ("+#: bad args: " <> showValForDebug av)

minusIntHashB :: IO Val
minusIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x - y))
        _ -> error ("-#: bad args: " <> showValForDebug av)

timesIntHashB :: IO Val
timesIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x * y))
        _ -> error ("*#: bad args: " <> showValForDebug av)

ltIntHashB, leIntHashB, eqIntHashB, gtIntHashB, geIntHashB, neIntHashB :: IO Val
ltIntHashB = makeIntCmpOp "<#"  (<)
leIntHashB = makeIntCmpOp "<=#" (<=)
eqIntHashB = makeIntCmpOp "==#" (==)
gtIntHashB = makeIntCmpOp ">#"  (>)
geIntHashB = makeIntCmpOp ">=#" (>=)
neIntHashB = makeIntCmpOp "/=#" (/=)

ltCharHashB, leCharHashB, eqCharHashB, gtCharHashB, geCharHashB, neCharHashB :: IO Val
ltCharHashB = makeCharCmpOp (<)
leCharHashB = makeCharCmpOp (<=)
eqCharHashB = makeCharCmpOp (==)
gtCharHashB = makeCharCmpOp (>)
geCharHashB = makeCharCmpOp (>=)
neCharHashB = makeCharCmpOp (/=)

charPrimOrd :: Val -> Int
charPrimOrd (VChar c) = ord c
charPrimOrd (VInt n)  = fromIntegral n
charPrimOrd v         = error ("char primop: bad arg: " <> showValForDebug v)

-- | @timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)@
--
-- ghc-prim spec (changelog 0.7+): returns @(# isHighNeeded, high, low #)@
-- where @high@ and @low@ are the bits of the double-word product and
-- @isHighNeeded@ is 1# when @high@ differs from the sign-extension of
-- @low@ (i.e. the product doesn't fit in a single Int#).
--
-- Phase 4 fix: the previous implementation returned a 2-tuple with a
-- single overflow flag and lost the high bits — breaking ghc-bignum's
-- @integerMul (IS x) (IS y)@ overflow path, which produces silently-
-- truncated 0 for @2 ^ 100 :: Integer@.
timesInt2B :: IO Val
timesInt2B = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            -- Compute full 128-bit product via Integer arithmetic, then
            -- split into low / high 64-bit halves (reinterpreted as Int64).
            let prod         = toInteger x * toInteger y
                low          = fromInteger prod                  :: Int64
                high         = fromInteger (prod `shiftR` 64)    :: Int64
                signExtended = if low < 0 then -1 else 0         :: Int64
                ovf          = if high /= signExtended then 1 else 0 :: Int64
            ovfT  <- newWHNFThunk (VInt ovf)
            highT <- newWHNFThunk (VInt high)
            lowT  <- newWHNFThunk (VInt low)
            pure (VCon "(#,,#)" [ovfT, highT, lowT])
        _ -> error ("timesInt2#: bad args: " <> showValForDebug av)

-- | @timesWord2# :: Word# -> Word# -> (# Word#, Word# #)@ — returns
-- the high and low 64-bit halves of the unsigned 128-bit product.
--
-- Phase 4 fix: previously returned @(# 0, low #)@, always dropping
-- the high word — silently truncated all overflows.  Now computes
-- via host 'Natural' to get the full 128-bit product.
timesWord2B :: IO Val
timesWord2B = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            -- Reinterpret as unsigned, multiply over Natural, split.
            let xNat = fromIntegral (fromIntegral x :: Word) :: Natural
                yNat = fromIntegral (fromIntegral y :: Word) :: Natural
                prod = xNat * yNat
                low  = fromIntegral (fromIntegral prod :: Word)            :: Int64
                high = fromIntegral (fromIntegral (prod `shiftR` 64) :: Word) :: Int64
            hiT <- newWHNFThunk (VInt high)
            loT <- newWHNFThunk (VInt low)
            pure (VCon "(#,#)" [hiT, loT])
        _ -> error ("timesWord2#: bad args: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: GHC.Exts Word# comparison + arithmetic primops
--------------------------------------------------------------------------------

ltWordB, leWordB, eqWordB, gtWordB, geWordB :: IO Val
ltWordB = makeWordCmpOp "ltWord#" (<)
leWordB = makeWordCmpOp "leWord#" (<=)
eqWordB = makeWordCmpOp "eqWord#" (==)
gtWordB = makeWordCmpOp "gtWord#" (>)
geWordB = makeWordCmpOp "geWord#" (>=)

ltWord8B, leWord8B, eqWord8B, gtWord8B, geWord8B, neWord8B :: IO Val
ltWord8B = makeWord8CmpOp "ltWord8#" (<)
leWord8B = makeWord8CmpOp "leWord8#" (<=)
eqWord8B = makeWord8CmpOp "eqWord8#" (==)
gtWord8B = makeWord8CmpOp "gtWord8#" (>)
geWord8B = makeWord8CmpOp "geWord8#" (>=)
neWord8B = makeWord8CmpOp "neWord8#" (/=)

plusWordB, minusWordB, timesWordB, quotWordB, remWordB :: IO Val
plusWordB  = makeWordArithOp "plusWord#"  (+)
minusWordB = makeWordArithOp "minusWord#" (-)
timesWordB = makeWordArithOp "timesWord#" (*)
quotWordB  = makeWordArithOp "quotWord#"  quot
remWordB   = makeWordArithOp "remWord#"   rem

popCntB :: IO Val
popCntB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (fromIntegral (popCount (fromIntegral n :: Word64))))
        _      -> error ("popCnt#: bad arg: " <> showValForDebug av)

ctzHashB :: IO Val
ctzHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let w = fromIntegral n :: Word64
                z | w == 0    = 64
                  | otherwise = countTrailingZeros w
            in pure (VInt (fromIntegral z))
        _ -> error ("ctz#: bad arg: " <> showValForDebug av)

clzHashB :: IO Val
clzHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let w = fromIntegral n :: Word64
                z | w == 0    = 64
                  | otherwise = countLeadingZeros w
            in pure (VInt (fromIntegral z))
        _ -> error ("clz#: bad arg: " <> showValForDebug av)

-- | Sized @popCntN# :: Word# -> Word#@ — popcount of the low N bits.
popCntWidthB :: Int -> IO Val
popCntWidthB width = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let mask = if width >= 64 then maxBound else (1 `shiftL` width) - 1
                w    = fromIntegral n .&. mask :: Word64
            in pure (VInt (fromIntegral (popCount w)))
        _ -> error ("popCnt" <> show width <> "#: bad arg: " <> showValForDebug av)

-- | Sized @clzN# :: Word# -> Word#@ — leading zeros among the low N bits.
-- For @w == 0@ the result is @N@ (matches GHC).
clzWidthB :: Int -> IO Val
clzWidthB width = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let mask = if width >= 64 then maxBound else (1 `shiftL` width) - 1
                w    = fromIntegral n .&. mask :: Word64
                z | w == 0    = width
                  | otherwise = countLeadingZeros w - (64 - width)
            in pure (VInt (fromIntegral z))
        _ -> error ("clz" <> show width <> "#: bad arg: " <> showValForDebug av)

-- | Sized @ctzN# :: Word# -> Word#@ — trailing zeros among the low N bits.
ctzWidthB :: Int -> IO Val
ctzWidthB width = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let mask = if width >= 64 then maxBound else (1 `shiftL` width) - 1
                w    = fromIntegral n .&. mask :: Word64
                z | w == 0    = width
                  | otherwise = countTrailingZeros w
            in pure (VInt (fromIntegral z))
        _ -> error ("ctz" <> show width <> "#: bad arg: " <> showValForDebug av)

indexOfTheOnlyBitB :: IO Val
indexOfTheOnlyBitB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n ->
            let w   = fromIntegral n :: Word64
                lsb = w .&. (complement w + 1)
                pos = finiteBitSize lsb - 1 - countLeadingZeros lsb
            in pure (VInt (fromIntegral pos))
        _ -> error ("indexOfTheOnlyBit#: bad arg: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: Int# arithmetic primops
--------------------------------------------------------------------------------

negateIntB :: IO Val
negateIntB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (negate n))
        _      -> error ("negateInt#: bad arg: " <> showValForDebug av)

quotIntB :: IO Val
quotIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `quot` y))
        _ -> error "quotInt#: bad args"

remIntB :: IO Val
remIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `rem` y))
        _ -> error "remInt#: bad args"

divIntHashB :: IO Val
divIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `div` y))
        _ -> error "divInt#: bad args"

modIntHashB :: IO Val
modIntHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `mod` y))
        _ -> error "modInt#: bad args"

-- Float# / Double# arithmetic primops.  The runtime represents both
-- Float and Double as VFloat (Double precision internally) — same
-- conflation the deleted binOpNum/binOpFloat performed — so each
-- primop is a single Haskell op on Double.
plusFloatHashB :: IO Val
plusFloatHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x + y))
        _ -> error ("plusFloat#: bad args: " <> showValForDebug av)

minusFloatHashB :: IO Val
minusFloatHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x - y))
        _ -> error ("minusFloat#: bad args: " <> showValForDebug av)

timesFloatHashB :: IO Val
timesFloatHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x * y))
        _ -> error ("timesFloat#: bad args: " <> showValForDebug av)

divideFloatHashB :: IO Val
divideFloatHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x / y))
        _ -> error ("divideFloat#: bad args: " <> showValForDebug av)

plusDoubleHashB :: IO Val
plusDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x + y))
        _ -> error ("+##: bad args: " <> showValForDebug av)

minusDoubleHashB :: IO Val
minusDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x - y))
        _ -> error ("-##: bad args: " <> showValForDebug av)

timesDoubleHashB :: IO Val
timesDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x * y))
        _ -> error ("*##: bad args: " <> showValForDebug av)

divideDoubleHashB :: IO Val
divideDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x / y))
        _ -> error ("/##: bad args: " <> showValForDebug av)


-- | Identity primop for representation-shared conversions.
-- IHC stores @Int#@, @Int64#@, @Word64#@ all as 'VInt'
-- (Int64-backed); the @intToInt64#@ \/ @int64ToInt#@ \/
-- @int64ToWord64#@ \/ @word64ToInt64#@ primops are therefore
-- runtime no-ops on the value side.
identityIntPrimop :: IO Val
identityIntPrimop = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case asInt64 av of
        Just n -> pure (VInt n)
        Nothing -> error ("identityIntPrimop: not an Int#: "
                         <> showValForDebug av)


-- | Coerce a 'Val' into an 'Int64' if its representation
-- supports it.  Both 'VInt' (Int64-backed) and 'VInteger'
-- whose value fits in Int64 are accepted.  Used by primop
-- shims that need Int64 args but may receive 'VInteger' from
-- source-loaded code where literal overflow routed an
-- in-range value through 'LInteger' (e.g. @-2^63@ via
-- NegativeLiterals on @-0x8000000000000000@).
-- 'floatToIntB' and the floor / ceiling / round / truncate shims were
-- removed in Phase 5.  Those methods now source-load through RealFrac /
-- RealFloat and bottom out on the Double#/Integer primops below.


asInt64 :: Val -> Maybe Int64
asInt64 (VInt n) = Just n
asInt64 (VInteger n)
  | n >= toInteger (minBound :: Int64)
  , n <= toInteger (maxBound :: Int64) = Just (fromInteger n)
asInt64 _ = Nothing


-- | @decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)@
-- GHC primop: decompose a Double into mantissa (Int64) and
-- base-2 exponent (Int).  For finite non-zero @d@,
-- @d = m * 2^e@ with @m@ in Int64 range (Double mantissa is
-- 53 bits) and @e@ in Int range.  Edge cases — zero, +/-Inf,
-- NaN, denormals — follow Haskell's 'decodeFloat', which
-- returns @(0, 0)@ for zero and unspecified values for the
-- non-finite cases.
--
-- IHC represents both Int# and Int64# as 'VInt' (storage type
-- 'Int64'), and the unboxed pair @(# a, b #)@ as
-- @VCon \"(#,#)\" [aT, bT]@.
decodeDoubleInt64HashB :: IO Val
decodeDoubleInt64HashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> do
            let (m, e) = decodeFloat d   -- (Integer, Int) per RealFloat Double
            mT <- newWHNFThunk (VInt (fromInteger m))
            eT <- newWHNFThunk (VInt (fromIntegral e))
            pure (VCon "(#,#)" [mT, eT])
        _ -> error
            ("decodeDouble_Int64#: not a Double: " <> showValForDebug av)

-- | @int2Double# :: Int# -> Double#@ — widen Int# to Double#.
-- Needed by source-loaded 'integerEncodeDouble#' (ghc-bignum
-- @GHC.Num.Integer:1052@): @integerEncodeDouble# (IS i) 0# =
-- int2Double# i@.
int2DoubleHashB :: IO Val
int2DoubleHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VFloat (fromIntegral n))
        _ -> error ("int2Double#: not an Int: " <> showValForDebug av)

-- | @intEncodeDouble# :: Int# -> Int# -> Double#@ — @m * 2^e@ as
-- a Double#.  Mirrors host 'encodeFloat' for Int mantissa.
intEncodeDoubleHashB :: IO Val
intEncodeDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt m, VInt e) ->
            pure (VFloat (encodeFloat (toInteger m) (fromIntegral e) :: Double))
        _ -> error
            ("intEncodeDouble#: bad args: " <> showValForDebug av
             <> ", " <> showValForDebug bv)

-- | @negateDouble# :: Double# -> Double#@.
negateDoubleHashB :: IO Val
negateDoubleHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VFloat (negate d))
        _ -> error ("negateDouble#: not a Double: " <> showValForDebug av)

-- | @double2Int# :: Double# -> Int#@ — truncating conversion
-- (matches GHC's Double->Int primop: round toward zero).
double2IntHashB :: IO Val
double2IntHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VInt (truncate d))
        _ -> error ("double2Int#: not a Double: " <> showValForDebug av)

-- | @double2Float# :: Double# -> Float#@ — IHC stores both as
-- 'VFloat' so the primop is identity.
double2FloatHashB :: IO Val
double2FloatHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VFloat d)
        _ -> error ("double2Float#: not a Double: " <> showValForDebug av)

-- | @float2Double# :: Float# -> Double#@ — identity (see above).
float2DoubleHashB :: IO Val
float2DoubleHashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VFloat d)
        _ -> error ("float2Double#: not a Float: " <> showValForDebug av)

-- | Binary Double# comparison primop builder.  Result is Bool#
-- (VInt 1/0).  Both args must be 'VFloat'.
makeDoubleCmpOp :: String -> (Double -> Double -> Bool) -> IO Val
makeDoubleCmpOp name op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (primBoolVal (op x y))
        _ -> error (name <> ": bad args: " <> showValForDebug av
                    <> ", " <> showValForDebug bv)

quotRemIntB :: IO Val
quotRemIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let (q, r) = x `quotRem` y
            qT <- newWHNFThunk (VInt q)
            rT <- newWHNFThunk (VInt r)
            pure (VCon "(#,#)" [qT, rT])
        _ -> error "quotRemInt#: bad args"

addIntCB :: IO Val
addIntCB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            rT <- newWHNFThunk (VInt (x + y))
            cT <- newWHNFThunk (VInt 0)
            pure (VCon "(#,#)" [rT, cT])
        _ -> error "addIntC#: bad args"

subIntCB :: IO Val
subIntCB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            rT <- newWHNFThunk (VInt (x - y))
            cT <- newWHNFThunk (VInt 0)
            pure (VCon "(#,#)" [rT, cT])
        _ -> error "subIntC#: bad args"

mulIntMayOfloB :: IO Val
mulIntMayOfloB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt _, VInt _) -> pure (VInt 0)
        _ -> error "mulIntMayOflo#: bad args"

-- Foreign.C.String helpers.  The real @Foreign.C.String.withCString@ in
-- base reaches for @getForeignEncoding@, which sits on RTS locale
-- plumbing we don't model.  We register a direct host shortcut that
-- packs each 'Char' as one byte — matching GHC's ASCII-only default
-- good enough for the libc-FFI common case — and feeds a raw 'Ptr
-- CChar' to the callback.
withCStringB :: IO Val
withCStringB = pure $ VFun $ \sT -> pure $ VFun $ \kT -> pure $ VIO $ do
    sv <- force legacyHooks sT
    s  <- valToString sv
    kv <- force legacyHooks kT
    BS.useAsCString (BC.pack s) $ \p -> do
        argT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr p)))
        r    <- apply legacyHooks kv argT
        case r of
            VIO io -> io
            _      -> pure r

-- Like 'withCString' but also passes the length to the callback, as a
-- 2-tuple @(Ptr CChar, Int)@.  'Foreign.C.String.withCStringLen' has
-- the same RTS-encoding dependency as 'withCString' and is trivially
-- derived here.
withCStringLenB :: IO Val
withCStringLenB = pure $ VFun $ \sT -> pure $ VFun $ \kT -> pure $ VIO $ do
    sv <- force legacyHooks sT
    s  <- valToString sv
    kv <- force legacyHooks kT
    let bs = BC.pack s
    BS.useAsCString bs $ \p -> do
        ptrT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr p)))
        lenT <- newWHNFThunk (VInt (fromIntegral (BS.length bs)))
        tupT <- newWHNFThunk (VCon "(,)" [ptrT, lenT])
        r    <- apply legacyHooks kv tupT
        case r of
            VIO io -> io
            _      -> pure r

-- Pure pointer peek — materialise a 'Ptr CChar' as a Haskell @String@.
peekCStringB :: IO Val
peekCStringB = pure $ VFun $ \pT -> pure $ VIO $ do
    pv <- force legacyHooks pT
    p  <- ptrValToPtr pv
    s  <- peekCAString (castPtr p)
    stringToListValIO s

-- Allocate a fresh NUL-terminated C string (via 'mallocBytes') and
-- return its raw pointer.  Caller is responsible for freeing.
newCStringB :: IO Val
newCStringB = pure $ VFun $ \sT -> pure $ VIO $ do
    sv <- force legacyHooks sT
    s  <- valToString sv
    let bs  = BC.pack s
        len = BS.length bs
    cp <- mallocBytes (len + 1)
    BS.useAsCString bs $ \src -> copyBytes cp (castPtr src) len
    poke (plusPtr cp len :: Ptr Word8) (0 :: Word8)
    pure (VPrimObj (PrimPtr cp))

sizeOfB :: IO Val
sizeOfB = pure $ VFun $ \a -> do
    let _ = a
    pure (VInt 64)

alignmentB :: IO Val
alignmentB = pure $ VFun $ \a -> do
    let _ = a
    pure (VInt 8)

--------------------------------------------------------------------------------
-- Phase 2.8: additional numeric / bit ops
--------------------------------------------------------------------------------

-- Phase (builtins minimum-surface): the bare-name @divMod@ / @quotRem@
-- shims were removed.  Resolution now flows through the source-loaded
-- @Integral Int@ instance in GHC/Internal/Real.hs (see the registry
-- comment near the old @("divMod", …)@ entry).  No host helper here.

bitAndB :: IO Val
bitAndB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .&. y))
        _ -> error "(.&.): bad args"

bitOrB :: IO Val
bitOrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .|. y))
        _ -> error "(.|.): bad args"

bitXorB :: IO Val
bitXorB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `xor` y))
        _ -> error "xor: bad args"

-- | @notI# :: Int# -> Int#@ — GHC.Prim bitwise-complement primop.
-- No Haskell source exists (carve-out, see registry comment); this
-- is the leaf the source-loaded @complement (I# x#) = I# (notI# x#)@
-- from @instance Bits Int@ bottoms on.  IHC stores @Int#@ as a
-- Haskell 'Int' inside 'VInt', so this is just 'complement'.
notIB :: IO Val
notIB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt x -> pure (VInt (complement x))
        _ -> error ("notI#: bad arg: " <> showValForDebug av)

-- popCountB / bitB / testBitB / clearBitB / setBitB removed — see the
-- @class Bits@ removal note at their former registry entries above.

--------------------------------------------------------------------------------
-- Power primops (GHC.Prim intrinsics, no .hs source)
--------------------------------------------------------------------------------

-- | @(**##) :: Double# -> Double# -> Double#@ — GHC.Prim floating
-- exponentiation.  Bottom of the source-loaded @Floating Double@
-- instance: @powerDouble (D# x) (D# y) = D# (x **## y)@.
powerDoubleHashB :: IO Val
powerDoubleHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x ** y))
        _ -> error ("**##: bad args: " <> showValForDebug av)

-- | @powerFloat# :: Float# -> Float# -> Float#@ — GHC.Prim floating
-- exponentiation.  Bottom of the source-loaded @Floating Float@
-- instance: @powerFloat (F# x) (F# y) = F# (powerFloat# x y)@.
-- IHC stores both Float# and Double# as 'VFloat' (Double-backed).
powerFloatHashB :: IO Val
powerFloatHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VFloat x, VFloat y) -> pure (VFloat (x ** y))
        _ -> error ("powerFloat#: bad args: " <> showValForDebug av)


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
    -- Populate the global ctor-index map as a side effect so
    -- 'structuralOrdering' can derive Ord by declaration order.
    populateCtorIndex reg
    pairs <- mapM mkBinding (Map.toList reg)
    pure (extendEnvMany pairs emptyEnv)
  where
    -- Arity-0 ctors are a single cheap 'VCon' — keep them eager since
    -- they're constants. Arity-n ctors build a chain of n VFun
    -- closures; defer that allocation until the ctor is actually
    -- referenced.
    mkBinding (name, (_tyName, 0, _idx)) = do
        t <- newWHNFThunk (VCon name [])
        pure (name, t)
    mkBinding (name, (_tyName, arity, _idx)) = do
        t <- newLazyBuiltinThunk (pure (buildLam name arity []))
        pure (name, t)

    -- A.5: per Haskell Report §4.2.1, a strict field annotation
    -- (`MkT !Int Int`) forces the corresponding argument thunk to
    -- WHNF at construction time.  We look up the constructor's
    -- strictness bitmap (populated by 'IHC.Scan.scanDataDecls') and,
    -- when at least one field is strict, force those thunks before
    -- returning the 'VCon'.  All-lazy ctors keep the original cheap
    -- path with no extra IO.
    --
    -- Phase 3: source-loaded @data Integer = IS !Int# | IP !BigNat# |
    -- IN !BigNat#@ from @ghc-bignum@ — we intercept at construction
    -- time and collapse to plain 'VInt' / 'VInteger' so the runtime
    -- never carries @VCon "IS"/"IP"/"IN"@ shapes.  Mirrors how
    -- @I#@ / @F#@ / @D#@ are already transparent; see
    -- 'tryIntegerCollapse' below.
    buildLam :: Name -> Int -> [Thunk] -> Val
    buildLam name 0    acc = VCon name (reverse acc)
    buildLam name left acc = VFun $ \t ->
        if left == 1
            then do
                let thunks = reverse (t : acc)
                strict <- lookupCtorStrictness name
                forceStrictFields strict thunks
                tryIntegerCollapse name thunks
            else pure (buildLam name (left - 1) (t : acc))

    -- | Walk the strict-field bitmap and force each marked thunk in
    -- place, ignoring extra/missing entries gracefully.
    forceStrictFields :: [Bool] -> [Thunk] -> IO ()
    forceStrictFields []         _      = pure ()
    forceStrictFields _          []     = pure ()
    forceStrictFields (s : ss) (t : ts) = do
        when s (() <$ force legacyHooks t)
        forceStrictFields ss ts

-- | Phase 3: construct-direction collapse for ghc-bignum's
-- @data Integer = IS !Int# | IP !BigNat# | IN !BigNat#@.
--
-- The runtime never carries @VCon "IS"/"IP"/"IN"@ shapes after
-- this — source-level @IS k@ / @IP bn@ / @IN bn@ produce plain
-- 'VInt' (in-Int64 range) or 'VInteger' (out-of-range) directly.
-- Mirrors how @I#@ / @F#@ / @D#@ are already handled transparently.
--
-- Sign-preservation: small IP and IN both fit in Int64 with
-- opposite signs (@VInt n@ vs @VInt (-n)@); larger values become
-- 'VInteger' with the appropriate sign.  Phase 1's matchPat
-- bridges already discriminate via range guards.
--
-- Fall-through (default 'VCon name thunks') applies when the field
-- isn't a recognised numeric shape (defensive — shouldn't happen
-- for well-typed source); matchPat's generic VCon case will still
-- handle it.
tryIntegerCollapse :: Name -> [Thunk] -> IO Val
tryIntegerCollapse "IS" [t] = do
    v <- force legacyHooks t
    case v of
        VInt _ -> pure v
        VInteger n
            | n >= toInteger (minBound :: Int64)
            , n <= toInteger (maxBound :: Int64)
            -> pure (VInt (fromInteger n))
        _ -> pure (VCon "IS" [t])
tryIntegerCollapse "IP" [t] = do
    v <- force legacyHooks t
    case v of
        VPrimObj (PrimBigNat n) ->
            if n <= fromIntegral (maxBound :: Int64)
                then pure (VInt (fromIntegral n))
                else pure (VInteger (fromIntegral n))
        _ -> pure (VCon "IP" [t])
tryIntegerCollapse "IN" [t] = do
    v <- force legacyHooks t
    case v of
        VPrimObj (PrimBigNat n)
            | n == 0    -> pure (VInt 0)
            | otherwise ->
                let absMinBoundInt64 = 1 + fromIntegral (maxBound :: Int64) :: Natural
                in if n <= absMinBoundInt64
                    then pure (VInt (fromInteger (negate (toInteger n))))
                    else pure (VInteger (negate (toInteger n)))
        _ -> pure (VCon "IN" [t])
-- Natural = NS !Word# | NB !BigNat# — collapse like Integer so
-- @(5 :: Natural)@ and @NS w@ share the same VInt/VInteger runtime
-- as Word/Integer paths; matchPat bridges re-expose NS/NB.
tryIntegerCollapse "NS" [t] = do
    v <- force legacyHooks t
    case v of
        VInt _ -> pure v
        VInteger n
            | n >= 0
            , n <= toInteger (maxBound :: Word64)
            -> pure (VInt (fromIntegral n))
        VCon "W#" [inner] -> force legacyHooks inner
        _ -> pure (VCon "NS" [t])
tryIntegerCollapse "NB" [t] = do
    v <- force legacyHooks t
    case v of
        VPrimObj (PrimBigNat n) ->
            if n <= fromIntegral (maxBound :: Word64)
                then pure (VInt (fromIntegral n))
                else pure (VInteger (fromIntegral n))
        _ -> pure (VCon "NB" [t])
tryIntegerCollapse name thunks = pure (VCon name thunks)

-- | Build an environment binding each record-field name to an accessor
-- function.  For a field @f@ that lives at index @i@ in constructor @Con@,
-- the accessor is equivalent to:
--
-- > f (Con _ _ ... x _ ...) = x  -- where x is at position i
--
-- When a field name appears in multiple constructors we generate a single
-- accessor that dispatches on the VCon's constructor name at runtime.
-- This is what makes @DuplicateRecordFields@ work: given
--
-- > data User    = User    { name :: String, age   :: Int }
-- > data Product = Product { name :: String, price :: Int }
--
-- Scan merges both @name@ entries into the FieldRegistry as
-- @name -> [("User", 0), ("Product", 0)]@; the accessor below then
-- looks up the actual constructor name of its VCon argument. The
-- runtime constructor name IS the type tag, so no separate "pick
-- accessor matching typeTagOf arg" step is needed.
--
-- Record construction (@User { name = "Alice" }@) is already
-- constructor-qualified at the AST level (@ERecordCon "User" ...@),
-- so Scheduler.desugarRecordCons picks the right field index via the
-- registry's @(conName, idx)@ pairs. Record update (@u { name = ... }@)
-- case-splits on every constructor that owns the field — see
-- @desugarRecordCons@'s ERecordUpdate arm.
--
-- The accessor is a plain @VFun@ so it participates in lazy evaluation.
buildFieldEnv :: FieldRegistry -> IO Env
buildFieldEnv reg = do
    pairs <- mapM mkAccessor (Map.toList reg)
    pure (HashMap.fromList pairs)
  where
    -- Defer VFun allocation for each record-field accessor. A program that
    -- never projects out that particular field never pays the cost.
    mkAccessor (fieldName, clauses) = do
        t <- newLazyBuiltinThunk (pure (VFun (access fieldName clauses)))
        pure (fieldName, t)

    -- Build a function that, given a VCon, extracts the right field.
    access fieldName clauses argThunk = do
        v <- force legacyHooks argThunk
        case v of
            -- 'SomeException' is the universal exception wrapper; it
            -- holds the real exception in its single field.  Code that
            -- accesses concrete-exception fields (e.g. @ioe_errno e@
            -- where @e :: SomeException@ post-'try', as in warp's
            -- 'acceptNewConnection') needs us to descend through the
            -- wrap.  Project the inner field and recurse so the
            -- accessor sees the underlying 'IOError' / etc.
            VCon "SomeException" [innerT] ->
                access fieldName clauses innerT
            VCon conName args ->
                case lookup conName clauses of
                    Just idx | idx < length args ->
                        force legacyHooks (args !! idx)
                    Just idx ->
                        throwIO (userError
                            ("record accessor `" <> BC.unpack fieldName
                             <> "`: constructor `" <> BC.unpack conName
                             <> "` has only " <> show (length args)
                             <> " fields, index " <> show idx
                             <> " out of range"))
                    Nothing -> tryIsStringFallback fieldName clauses v conName
            -- Nullary class methods like @mempty@ need result-type
            -- evidence before a newtype field accessor can project them.
            -- A single-constructor field registry gives us that evidence.
            VClassMethod _ _ _ _ go
              | [(conName, _)] <- clauses -> do
                  dummyT <- newWHNFThunk VUnit
                  resolved <- go [conName] dummyT
                  resolvedT <- newWHNFThunk resolved
                  access fieldName clauses resolvedT
            _
              -- Newtype-transparent fallback: if the field-registry
              -- entry for this name has a SINGLE constructor with a
              -- SINGLE field (i.e. a newtype), and the runtime value
              -- isn't wrapped in 'VCon' (because IHC's evaluator
              -- elides newtype constructors at runtime), return the
              -- raw value as-is.  Mirrors GHC's runtime behaviour
              -- where 'ParsecT body' === 'body'.
              | [(_, 0)] <- clauses -> pure v
              | otherwise ->
                  throwIO (userError
                      ("record accessor `" <> BC.unpack fieldName
                       <> "` applied to non-constructor value: "
                       <> showValForDebug v))

    -- Optimistic OverloadedStrings bridge for record accessors.
    --
    -- Source-loaded blaze-html declares @h1 = Parent "h1" "<h1" "</h1>"@
    -- where 'Parent' expects 'StaticString' arguments.  IHC keeps the
    -- literal "h1" as a [Char] cons list (no type-driven 'IsString'
    -- elaboration), so the slot holds a list when 'renderString' later
    -- projects 'getString' out of it.
    --
    -- Mirror the existing 'IHC.Eval.charListToByteStringVal' bridge:
    -- when the accessor sees a [Char] where it expected a record, do
    -- the 'IsString.fromString' conversion at the accessor boundary.
    --
    -- We only know how to synthesise a small set of record types whose
    -- 'IsString' instance has a fixed shape.  Currently:
    --   * 'StaticString' — first field 'getString :: String -> String'
    --     is the appending closure @(s ++)@; the other two fields
    --     (UTF-8 bytes, lazy 'Text') aren't reached by blaze's
    --     'renderString' but we still need a thunk that won't crash if
    --     forced; we make those a 'VLazyMethod' that errors with a
    --     helpful diagnostic.
    --
    -- Other accessor failures fall through to the original error.
    tryIsStringFallback fieldName clauses v conName
        | isCharConsList v
        , Just resultVal <- synthesiseFromCharList fieldName clauses v
        = resultVal
    tryIsStringFallback fieldName _clauses _v conName =
        throwIO (userError
            ("record accessor `" <> BC.unpack fieldName
             <> "`: constructor `" <> BC.unpack conName
             <> "` has no such field"))

    -- Per-record synthesis table.  Returns 'Just (IO Val)' if the field
    -- + target-constructor pair is one we know how to materialise from
    -- a [Char] without going through real instance dispatch.
    synthesiseFromCharList fieldName clauses listVal
        | any ((BC.pack "StaticString" ==) . fst) clauses
        , fieldName == BC.pack "getString"
        = Just (pure (charListAppender listVal))
        -- CI ByteString / HeaderName (http-types, warp).  Pre-fix
        -- foldedCase returned a host VStr; original returned the raw
        -- [Char] list.  BS.== / BS.length then either hung (Eq on VStr
        -- vs real BS via class dispatch) or missed, so
        -- indexResponseHeader [("Server",…)] never matched / hung.
        -- Pack real @BS ForeignPtr Int@ payloads (ASCII foldCase).
        | any ((BC.pack "CI" ==) . fst) clauses
        , fieldName == BC.pack "original"
        = Just (stringToByteStringVal listVal id)
        | any ((BC.pack "CI" ==) . fst) clauses
        , fieldName == BC.pack "foldedCase"
        = Just (stringToByteStringVal listVal (map toLower))
        | otherwise = Nothing

    -- Pack a [Char]/VStr as a source-shaped ByteString, optionally
    -- mapping the characters first (ASCII case fold for foldedCase).
    stringToByteStringVal listVal mapChars = do
        s <- valToString listVal
        let bs = BC.pack (mapChars s)
            len = BS.length bs
        fp <- mallocForeignPtrBytes (max 1 len)
        withForeignPtr fp $ \dst ->
            when (len > 0) $
                BS.useAsCStringLen bs $ \(src, n) ->
                    copyBytes (castPtr dst) (castPtr src) n
        markWord8PtrRange (castPtr (unsafeForeignPtrToPtr fp)) (max 1 len)
        markTypedHostPtr (castPtr (unsafeForeignPtrToPtr fp) :: Ptr Word8)
            (BC.pack "Word8")
        fpV <- mkForeignPtrVal fp
        fpT <- newWHNFThunk fpV
        lenT <- newWHNFThunk (VInt (fromIntegral len))
        pure (VCon "BS" [fpT, lenT])
    -- @(listVal ++)@: a 'VFun' that, given another list, returns
    -- @listVal ++ that@.  Built by walking 'listVal' once and chaining
    -- cons cells.  Mirrors the runtime shape of 'StaticString's
    -- 'getString' field for an 'IsString.fromString'-converted literal.
    charListAppender :: Val -> Val
    charListAppender listVal = VFun $ \tailT ->
        appendCharList listVal tailT

    -- Drive 'listVal' to its '[]' tail, prepending each cons cell onto
    -- the supplied tail thunk.  Returns the result as a fully-forced
    -- cons chain of 'VChar's.  Only used by 'charListAppender'.
    appendCharList (VCon "[]" []) tailT = force legacyHooks tailT
    appendCharList (VCon ":" [hT, restT]) tailT = do
        restV  <- force legacyHooks restT
        rest'  <- appendCharList restV tailT
        rest'T <- newWHNFThunk rest'
        pure (VCon ":" [hT, rest'T])
    appendCharList other _ =
        throwIO (userError
            ("appendCharList: not a [Char] cons list: " <> showValForDebug other))

    -- A VCon is a [Char] cons list iff it's [] or (h:t).  We don't
    -- force the tail — accepting list-shaped values whose head is
    -- char is sufficient for the OverloadedStrings pattern.
    isCharConsList (VCon "[]" []) = True
    isCharConsList (VCon ":"  [_, _]) = True
    isCharConsList _ = False

--------------------------------------------------------------------------------
-- Phase 2.10a: concurrency - thread primitives
--------------------------------------------------------------------------------

-- | @fork# :: IO () -> State# RealWorld -> (# State# RealWorld, ThreadId# #)@
-- — GHC primop used by source-loaded @forkIO@ and warp's @defaultFork@
-- (the latter inlines the primop call).  No Haskell source in
-- @ghc-prim@; we host it.  Wraps the host 'forkIO' and packages the
-- result as a state-passing unboxed tuple so source-side
-- @case fork# io s of (# s', tid #) -> ...@ matches our pattern bridge.
--
-- Critical: warp's 'defaultFork' inlines the primop call as
--   @IO $ \\s0 -> case io unsafeUnmask of { IO io' -> case fork# io' s0 of ... }@
-- so the first argument is @io'@ — the @State# RealWorld -> (# State# RealWorld, () #)@
-- function EXTRACTED from the @IO io'@ pattern match, not a wrapped
-- @IO ()@ Val.  In our value world that arrives as a 'VFun' (or
-- 'VFunIP' for ImplicitParam-carrying closures), and 'runIOVal' on a
-- 'VFun' falls through to @pure v@ — so the action body never runs
-- and warp's connection handler never executes.  Apply the function
-- with a state token here so the action runs in the new thread.
forkHashB :: IO Val
forkHashB = pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    av <- force legacyHooks aT
    tid <- forkIO $ do
        _ <- runActionVal av
        pure ()
    registerSpawnedThread tid
    rwT  <- newWHNFThunk (VPrimObj PrimRealWorld)
    tidT <- newWHNFThunk (VPrimObj (PrimThreadId tid))
    pure (VCon "(#,#)" [rwT, tidT])
  where
    -- A forked action can arrive in any of these shapes:
    --   * @VIO action@ — already-wrapped host IO
    --   * @VCon \"IO\" [ft]@ — source-constructed @IO $ \\s -> ...@
    --   * @VFun _ / VFunIP _ _@ — the State#-passing function extracted
    --     from an @IO io'@ pattern match (warp's defaultFork shape)
    -- Apply state token to the function variants so the body actually
    -- runs in the new thread.
    runActionVal v = case v of
        VFun f -> do
            stok <- newWHNFThunk (VPrimObj PrimRealWorld)
            r <- f stok
            -- State# transformer may return VIO, unboxed (# s, a #), or
            -- already a WHNF result. Always drive through runIOVal so
            -- nested IO (makeGracefulRecv, http1, the app) actually runs
            -- when the body is a deferred VIO rather than an already-
            -- forced unboxed pair.
            runIOVal legacyHooks r
        VFunIP _ f -> do
            stok <- newWHNFThunk (VPrimObj PrimRealWorld)
            r <- f Map.empty stok
            runIOVal legacyHooks r
        _ -> runIOVal legacyHooks v

-- | @killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld@.
-- GHC.Prim primop. Source-loaded @throwTo (ThreadId tid) ex@ bottoms out
-- here after wrapping @ex@ with @toException@.
killThreadHashB :: IO Val
killThreadHashB = pure $ VFun $ \tidT -> pure $ VFun $ \excT -> pure $ VFun $ \_stT -> do
    tidV <- force legacyHooks tidT
    excV <- force legacyHooks excT
    tid <- extractThreadIdHash tidV
    exc <- valToIhcException excV
    throwTo tid exc
    pure (VPrimObj PrimRealWorld)
  where
    extractThreadIdHash (VPrimObj (PrimThreadId tid)) = pure tid
    extractThreadIdHash (VCon "ThreadId" [innerT]) = do
        inner <- force legacyHooks innerT
        extractThreadIdHash inner
    extractThreadIdHash other =
        error ("killThread#: not a ThreadId#: " <> showValForDebug other)

myThreadIdHashB :: IO Val
myThreadIdHashB = pure $ VFun $ \stT -> do
    tid <- myThreadId
    tidT <- newWHNFThunk (VPrimObj (PrimThreadId tid))
    pure (VCon "(#,#)" [stT, tidT])

-- | @labelThread# :: ThreadId# -> ByteArray# -> State# RealWorld -> State# RealWorld@.
-- GHC.Prim primop. GHC uses it for eventlog/debug labels; IHC does not
-- expose an eventlog, so preserve only the state-threading shape.
labelThreadHashB :: IO Val
labelThreadHashB = pure $ VFun $ \_tidT -> pure $ VFun $ \_labelT -> pure $ VFun $ \sT ->
    force legacyHooks sT

-- | @delay# :: Int# -> State# RealWorld -> State# RealWorld@.
-- GHC.Prim primop. Source-loaded @threadDelay@ wraps this in IO and adds
-- the unit result; the primop itself only threads the state token.
delayHashB :: IO Val
delayHashB = pure $ VFun $ \nT -> pure $ VFun $ \sT -> do
    nv <- force legacyHooks nT
    case nv of
        VInt n -> do
            threadDelay (fromIntegral n)
            force legacyHooks sT
        _ -> error ("delay#: not an Int#: " <> showValForDebug nv)

closeFdWithB :: IO Val
closeFdWithB = pure $ VFun $ \closeT -> pure $ VFun $ \fdT -> pure $ VIO $ do
    CE.catch
        (do
            closeV <- force legacyHooks closeT
            r <- apply legacyHooks closeV fdT
            _ <- runIOVal legacyHooks r
            pure VUnit)
        (\(LoopException _) -> pure VUnit)

-- | @threadWaitRead :: Fd -> IO ()@ — delegate to host RTS.
threadWaitReadB :: IO Val
threadWaitReadB = pure $ VFun $ \fdT -> pure $ VIO $ do
    n <- fdArgToInt fdT "threadWaitRead"
    threadWaitRead (fromIntegral n)
    pure VUnit

-- | @threadWaitWrite :: Fd -> IO ()@ — delegate to host RTS.
threadWaitWriteB :: IO Val
threadWaitWriteB = pure $ VFun $ \fdT -> pure $ VIO $ do
    n <- fdArgToInt fdT "threadWaitWrite"
    threadWaitWrite (fromIntegral n)
    pure VUnit

-- | @threadWaitReadSTM :: Fd -> IO (STM (), IO ())@.
threadWaitReadSTMB :: IO Val
threadWaitReadSTMB = pure $ VFun $ \fdT -> pure $ VIO $
    threadWaitSTMimpl fdT
        (\i -> threadWaitRead (fromIntegral i))
        "threadWaitReadSTM"

threadWaitWriteSTMB :: IO Val
threadWaitWriteSTMB = pure $ VFun $ \fdT -> pure $ VIO $
    threadWaitSTMimpl fdT
        (\i -> threadWaitWrite (fromIntegral i))
        "threadWaitWriteSTM"


-- | Build @(STM (), IO ())@ for a host-backed fd wait.
threadWaitSTMimpl
    :: Thunk
    -> (Int -> IO ())
    -> String
    -> IO Val
threadWaitSTMimpl fdT waitIO primName = do
    n <- fdArgToInt fdT primName
    threadWaitSTMimplInt n waitIO

threadWaitSTMimplInt :: Int64 -> (Int -> IO ()) -> IO Val
threadWaitSTMimplInt n waitIO = do
    ready <- newTVarIO False
    _ <- forkIO $ do
        waitIO (fromIntegral n)
        atomically (writeTVar ready True)
    let waitStm = VFun $ \_sT -> pure $ VIO $ do
            go
            unitT <- newWHNFThunk VUnit
            sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
            pure (VCon "(#,#)" [sT', unitT])
          where
            go = do
                ok <- readTVarIO ready
                if ok then pure ()
                else threadDelay 1000 >> go
    let cancel = VIO (pure VUnit)
    waitStmT <- newWHNFThunk waitStm
    stmT <- newWHNFThunk (VCon "STM" [waitStmT])
    cancelT <- newWHNFThunk cancel
    pure (VCon "(,)" [stmT, cancelT])

-- | @threadWait :: Event -> Fd -> IO ()@ (GHC.Internal.Event.Thread).
-- The source body dispatches to the RTS event manager (registerFd + an MVar
-- handshake), which IHC does not run.  Delegate straight to the host RTS IO
-- manager, selecting read vs. write from the 'Event' bitmask
-- (@evtRead = Event 1@, @evtWrite = Event 2@; @Event@ is a newtype so the
-- runtime value is either the bare Int or @Con [Int]@).
threadWaitB :: IO Val
threadWaitB = pure $ VFun $ \evtT -> pure $ VFun $ \fdT -> pure $ VIO $ do
    fd  <- fdArgToInt fdT "threadWait"
    evt <- eventBitsOf evtT
    if evt .&. 2 /= 0
        then threadWaitWrite (fromIntegral fd)
        else threadWaitRead  (fromIntegral fd)
    pure VUnit

-- | Extract the 'GHC.Internal.Event.Internal.Types.Event' bitmask
-- (@evtRead = Event 1@, @evtWrite = Event 2@) from a runtime value.  'Event'
-- is a newtype, so the value is either the bare Int or @Con [Int]@.  Defaults
-- to read (1) for any unexpected shape.
eventBitsOf :: Thunk -> IO Int64
eventBitsOf t = do
    v <- force legacyHooks t
    case v of
        VInt n          -> pure n
        VCon _ [innerT]  -> do
            iv <- force legacyHooks innerT
            case iv of
                VInt n -> pure n
                _      -> pure 1
        _ -> pure 1

-- | @registerFd :: EventManager -> IOCallback -> Fd -> Event -> Lifetime -> IO FdKey@
-- (GHC.Internal.Event.Manager).  IHC does not run the RTS event manager, so we
-- emulate a OneShot registration with a host thread: wait on the fd through the
-- host RTS IO manager (read or write per the 'Event' bitmask), then fire the
-- interpreted callback as @cb fdKey evt@.  This is the single choke point both
-- the IO wait path ('threadWait') and the STM wait path ('threadWaitSTM', used
-- by network/warp for cancellable recv) reduce to after 'getSystemEventManager_'.
-- The 'EventManager' and 'FdKey' are opaque: the callback ignores the FdKey and
-- our 'unregisterFd_' ignores both.
registerFdB :: IO Val
registerFdB = pure $ VFun $ \_mgrT -> pure $ VFun $ \cbT -> pure $ VFun $ \fdT ->
    pure $ VFun $ \evtT -> pure $ VFun $ \_lifeT -> pure $ VIO $ do
        fd  <- fdArgToInt fdT "registerFd"
        evt <- eventBitsOf evtT
        cbV <- force legacyHooks cbT
        fdKeyT <- newWHNFThunk (VCon "FdKey" [])
        _ <- forkIO $ do
            if evt .&. 2 /= 0
                then threadWaitWrite (fromIntegral fd)
                else threadWaitRead  (fromIntegral fd)
            -- cb fdKey evt :: IO ()
            r1 <- apply legacyHooks cbV fdKeyT
            r2 <- apply legacyHooks r1 evtT
            _  <- runIOVal legacyHooks r2
            pure ()
        force legacyHooks fdKeyT

-- | @unregisterFd_ :: EventManager -> FdKey -> IO Bool@.  The OneShot waiter
-- 'registerFdB' spawned self-completes, so there is nothing to deregister;
-- report success.  (On the cancel path the waiter may linger until the fd
-- becomes ready — harmless for the common warp recv path.)
unregisterFd_B :: IO Val
unregisterFd_B = pure $ VFun $ \_mgrT -> pure $ VFun $ \_regT -> pure $ VIO $
    pure (boolVal True)

-- | Unwrap a @Fd@/@CInt@-like argument to its underlying @Int@.
-- Source newtypes nest: @Fd (CInt n)@, @CInt (I32# n)@, bare 'VInt',
-- or a single-field 'VCon'.  Walk constructors until we hit 'VInt'.
-- (Bug: @threadWaitReadSTM . Fd@ with nested CInt used to fail as
-- "Fd payload not VInt: <function>" when the newtype ctor was still a
-- VFun shell, hanging warp's makeGracefulRecv forever.)
fdArgToInt :: Thunk -> String -> IO Int64
fdArgToInt t primName = force legacyHooks t >>= go 0
  where
    go :: Int -> Val -> IO Int64
    go depth v
        | depth > 8 = error (primName <> ": Fd nest too deep: " <> showValForDebug v)
        | otherwise = case v of
            VInt n -> pure n
            VInteger n -> pure (fromIntegral n)
            -- Char#/Int# leakage (fd 36 → '$').
            VChar c -> pure (fromIntegral (fromEnum c))
            -- Newtype / data wrappers: peel one field.
            VCon _ [inner] -> force legacyHooks inner >>= go (depth + 1)
            -- Nullary / multi-field: not an Fd.
            VCon n _ ->
                error (primName <> ": unexpected Fd ctor " <> BC.unpack n
                       <> ": " <> showValForDebug v)
            -- Newtype constructor still a function (unapplied shell) —
            -- should not reach here if applied; report clearly.
            VFun{} ->
                error (primName <> ": Fd still a function (unapplied newtype?): "
                       <> showValForDebug v)
            _ -> error (primName <> ": not Fd-like: " <> showValForDebug v)

-- | @getSystemEventManager :: IO (Maybe EventManager)@.  Returns a stub
-- manager so the threaded socket-wait path (threadWait / threadWaitSTM via
-- @Just mgr <- getSystemEventManager@) proceeds; the manager is opaque and only
-- ever passed back to our host-backed 'registerFdB' / 'unregisterFd_B', which
-- ignore it.  (Was @Nothing@; that dead-ended network/warp's recv on
-- "Just mgr <- getSystemEventManager".)
getSystemEventManagerB :: IO Val
getSystemEventManagerB = pure $ VIO $ do
    mgrT <- newWHNFThunk (VCon "IhcEventManager" [])
    pure (VCon "Just" [mgrT])

getSystemTimerManagerB :: IO Val
getSystemTimerManagerB = pure $ VIO $ pure (VCon "TimerManager" [])

-- | @registerTimeout :: TimerManager -> Int -> IO () -> IO TimeoutKey@.
-- Implemented as @forkIO $ threadDelay usec >> callback@ rather than
-- delegated to the host @GHC.Event@ TimerManager: warp's only use is
-- registering connection-idle/slowloris timeouts, which only need
-- "fire roughly N microseconds from now" semantics.  Limitations:
-- * 'unregisterTimeout' is a no-op (no cancellation),
-- * timing is via 'threadDelay', not the host monotonic-clock manager.
-- These are acceptable for the warp request-handling path; revisit if
-- a fixture starts depending on real cancellation.
registerTimeoutB :: IO Val
registerTimeoutB = pure $ VFun $ \_mgrT -> pure $ VFun $ \usecT -> pure $ VFun $ \cbT -> pure $ VIO $ do
    usecV <- force legacyHooks usecT
    case usecV of
        VInt usec -> do
            cbV <- force legacyHooks cbT
            _ <- forkIO $ do
                threadDelay (fromIntegral usec)
                _ <- runIOVal legacyHooks cbV
                pure ()
            n <- atomicModifyIORef' uniqueCounterRef $ \x ->
                let x' = x + 1 in (x', x')
            nT <- newWHNFThunk (VInt n)
            pure (VCon "TK" [nT])
        _ -> error ("registerTimeout: timeout is not an Int: " <> showValForDebug usecV)

unregisterTimeoutB :: IO Val
unregisterTimeoutB = pure $ VFun $ \_mgrT -> pure $ VFun $ \_keyT ->
    pure $ VIO $ pure VUnit

updateTimeoutB :: IO Val
updateTimeoutB = pure $ VFun $ \_mgrT -> pure $ VFun $ \_keyT -> pure $ VFun $ \usecT ->
    pure $ VIO $ do
        usecV <- force legacyHooks usecT
        case usecV of
            VInt _ -> pure VUnit
            _ -> error ("updateTimeout: timeout is not an Int: " <> showValForDebug usecV)

--------------------------------------------------------------------------------
-- Phase 2.10: STM primops (# -suffixed, GHC.Prim)
--
-- GHC.Prim STM primops, compiler-intrinsic — no Haskell source. The
-- source-loaded @GHC.Conc.Sync@ wrappers (@atomically@, @retry@,
-- @newTVar@, @readTVar@, @writeTVar@, @catchSTM@, @orElse@) all bottom
-- out into these. The RTS provides the underlying transactional
-- machinery; our interpreter is single-threaded at the eval level, so
-- STM collapses cleanly onto IO — same bridge strategy as @ST s a ≈
-- IO a@ (commit 1ed2881). Justification per CLAUDE.md: compiler-
-- intrinsic / RTS-exclusive, no userland Haskell could implement the
-- transactional scheduler.
--------------------------------------------------------------------------------

-- | Extract the host 'TVar' from either a raw 'PrimTVar' (our builtin-
-- returned shape) or the source-wrapped @TVar tvar#@ VCon.
requireTVarPrim :: String -> Val -> IO (TVar Val)
requireTVarPrim fn v = case v of
    VPrimObj (PrimTVar tv) -> pure tv
    VCon "TVar" [tvT]      -> force legacyHooks tvT >>= requireTVarPrim fn
    _ -> error (fn <> ": not a TVar#: " <> showValForDebug v)

-- | Wrap a 'Val' as an unboxed-pair @(# State#, a #)@ result. Used by
-- the @#@-suffixed primops that return their value threaded through a
-- State# token. Pass-through if the value is already shaped correctly.
ensureStatePair :: Val -> IO Val
ensureStatePair v = case v of
    VCon "(#,#)" _ -> pure v
    _ -> do
        sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
        vT  <- newWHNFThunk v
        pure (VCon "(#,#)" [sT', vT])

-- | @atomically# :: (State# RealWorld -> (# State# RealWorld, a #))
--                 -> State# RealWorld
--                 -> (# State# RealWorld, a #)@
--
-- Source-loaded: @atomically (STM m) = IO (\\s -> (atomically# m) s)@.
-- Single-threaded STM-as-IO: apply the state transformer, but on
-- 'retry#' re-execute after a short delay so concurrent writers
-- (e.g. 'registerFd' callback writing a TVar for
-- 'threadWaitReadSTM') can make progress.  Without the re-exec loop,
-- warp's 'makeGracefulRecv' hangs forever on
-- @atomically (checkShutdown \<|> sockWait)@ because the first
-- 'retry' would surface and the second branch would also retry out.
atomicallyHashB :: IO Val
atomicallyHashB = pure $ VFun $ \stmT -> pure $ VFun $ \sT -> do
    let loop = do
            stmV <- force legacyHooks stmT
            r <- CE.try @CE.SomeException $ do
                rRaw <- apply legacyHooks stmV sT
                runIOVal legacyHooks rRaw
            case r of
                Right v -> ensureStatePair v
                Left e
                    | isStmRetryException e -> do
                        -- Yield to forked waiters (registerFd) and re-run.
                        threadDelay 1000  -- 1 ms
                        loop
                    | otherwise -> CE.throwIO e
    loop

-- | @retry# :: State# RealWorld -> (# State# RealWorld, a #)@.
--
-- Signals the enclosing 'atomically#' to re-execute (and
-- source-loaded 'orElse' via 'catchRetry#').  Must not be a bare
-- forever-block: IHC is single-threaded at the eval level, so
-- concurrent TVar writers only run between atomically iterations.
retryHashB :: IO Val
retryHashB = pure $ VFun $ \_sT -> CE.throwIO StmRetryException

-- | Marker for STM 'retry#'.  Distinct from arbitrary IOErrors so
-- 'atomically#' / 'catchRetry#' do not treat every failure as retry.
data StmRetryException = StmRetryException
    deriving (Show)
instance CE.Exception StmRetryException

isStmRetryException :: CE.SomeException -> Bool
isStmRetryException e =
    case CE.fromException e of
        Just StmRetryException -> True
        Nothing ->
            -- Legacy: older retry# used userError "STM retry"
            case CE.fromException e of
                Just (CE.ErrorCall msg) -> msg == "STM retry"
                Nothing -> False

-- | @catchRetry# :: (State# RealWorld -> (# State# RealWorld, a #))
--                -> (State# RealWorld -> (# State# RealWorld, a #))
--                -> State# RealWorld
--                -> (# State# RealWorld, a #)@
--
-- Source-loaded: @orElse (STM m) e = STM $ \\s -> catchRetry# m (unSTM e) s@.
-- Try the first action; if it raises a *retry* exception, fall back to
-- the second.  Non-retry exceptions propagate.  If the second also
-- retries, re-raise 'StmRetryException' so the enclosing 'atomically#'
-- re-executes the whole transaction (both arms get another chance).
catchRetryHashB :: IO Val
catchRetryHashB = pure $ VFun $ \aT -> pure $ VFun $ \bT -> pure $ VFun $ \sT -> do
    aV <- force legacyHooks aT
    bV <- force legacyHooks bT
    let runAction stm = do
            rRaw <- apply legacyHooks stm sT
            runIOVal legacyHooks rRaw
    r <- CE.try @CE.SomeException (runAction aV)
    case r of
        Right v -> ensureStatePair v
        Left e
            | isStmRetryException e -> do
                r2 <- CE.try @CE.SomeException (runAction bV)
                case r2 of
                    Right v -> ensureStatePair v
                    Left e2
                        | isStmRetryException e2 -> CE.throwIO StmRetryException
                        | otherwise -> CE.throwIO e2
            | otherwise -> CE.throwIO e

-- | @catchSTM# :: (State# RealWorld -> (# State# RealWorld, a #))
--              -> (b -> State# RealWorld -> (# State# RealWorld, a #))
--              -> State# RealWorld
--              -> (# State# RealWorld, a #)@
--
-- Source-loaded: @catchSTM (STM m) handler = STM $ catchSTM# m handler'@.
-- Same shape as 'catch#' but for STM actions — in our single-threaded
-- STM-as-IO bridge the implementation is identical.
catchSTMHashB :: IO Val
catchSTMHashB = pure $ VFun $ \ioT -> pure $ VFun $ \hT -> pure $ VFun $ \sT -> do
    ioV <- force legacyHooks ioT
    hV  <- force legacyHooks hT
    let runAction = do
            rRaw <- apply legacyHooks ioV sT
            runIOVal legacyHooks rRaw
    rRes <- CE.try @IhcException (CE.try @SomeException runAction)
    case rRes of
        Right (Right v) -> ensureStatePair v
        Right (Left se) -> do
            let msg = BC.pack (show se)
            excT <- newWHNFThunk (VStr msg)
            invokeHandler hV excT
        Left exc -> do
            excVal <- ihcExceptionToVal exc
            excT   <- newWHNFThunk excVal
            invokeHandler hV excT
  where
    invokeHandler hV excT = do
        r1 <- apply legacyHooks hV excT
        case r1 of
            VFun _ -> do
                sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
                rRaw <- apply legacyHooks r1 sT'
                v    <- runIOVal legacyHooks rRaw
                ensureStatePair v
            _ -> do
                v <- runIOVal legacyHooks r1
                ensureStatePair v

-- | @newTVar# :: a -> State# s -> (# State# s, TVar# s a #)@.
-- Source-loaded @newTVar@ / @newTVarIO@ bottom out here.
newTVarHashB :: IO Val
newTVarHashB = pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    av  <- force legacyHooks aT
    tv  <- newTVarIO av
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    tvT <- newWHNFThunk (VPrimObj (PrimTVar tv))
    pure (VCon "(#,#)" [sT', tvT])

-- | @readTVar# :: TVar# s a -> State# s -> (# State# s, a #)@.
readTVarHashB :: IO Val
readTVarHashB = pure $ VFun $ \tvT -> pure $ VFun $ \_sT -> do
    tvv <- force legacyHooks tvT
    tv  <- requireTVarPrim "readTVar#" tvv
    v   <- atomically (readTVar tv)
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    vT  <- newWHNFThunk v
    pure (VCon "(#,#)" [sT', vT])

-- | @readTVarIO# :: TVar# s a -> State# s -> (# State# s, a #)@.
-- Like readTVar# but without a surrounding transaction.
readTVarIOHashB :: IO Val
readTVarIOHashB = pure $ VFun $ \tvT -> pure $ VFun $ \_sT -> do
    tvv <- force legacyHooks tvT
    tv  <- requireTVarPrim "readTVarIO#" tvv
    v   <- readTVarIO tv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    vT  <- newWHNFThunk v
    pure (VCon "(#,#)" [sT', vT])

-- | @writeTVar# :: TVar# s a -> a -> State# s -> State# s@.
writeTVarHashB :: IO Val
writeTVarHashB = pure $ VFun $ \tvT -> pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    tvv <- force legacyHooks tvT
    tv  <- requireTVarPrim "writeTVar#" tvv
    av  <- force legacyHooks aT
    atomically (writeTVar tv av)
    pure (VPrimObj PrimRealWorld)

--------------------------------------------------------------------------------
-- Phase 2.10a: exception primitives
--------------------------------------------------------------------------------

-- | Wrap a 'Val' in an 'IhcException' for host-level throwing.
valToIhcException :: Val -> IO IhcException
valToIhcException v = do
    msg <- extractExceptionMessage v
    t   <- newWHNFThunk v
    pure (IhcException msg t)

-- | Extract a user-readable message from an exception value.
-- Unwraps 'SomeException' and pulls the payload out of 'ErrorCall' /
-- 'ErrorCallWithLocation' so e.g. @head []@ reports
-- @Prelude.head: empty list@ instead of just @ErrorCallWithLocation@.
extractExceptionMessage :: Val -> IO ByteString
extractExceptionMessage val = case val of
    VStr s -> pure s
    VCon "SomeException" [innerT] -> do
        inner <- force legacyHooks innerT
        extractExceptionMessage inner
    VCon "ErrorCall" [msgT] ->
        tryValToString msgT (BC.pack "ErrorCall")
    VCon "ErrorCallWithLocation" (msgT : _) ->
        tryValToString msgT (BC.pack "ErrorCallWithLocation")
    -- IOError record: ioe_handle, ioe_type, ioe_location, ioe_description, ioe_errno, ioe_filename
    VCon "IOError" [_handleT, _typeT, locT, descT, _errnoT, _fileT] -> do
        loc <- tryValToString locT (BC.pack "")
        desc <- tryValToString descT (BC.pack "")
        pure (BC.pack "IOError: " <> loc <> BC.pack ": " <> desc)
    VCon n _ -> pure n
    _        -> pure (BC.pack (showValForDebug val))
  where
    tryValToString :: Thunk -> ByteString -> IO ByteString
    tryValToString t fallback = do
        r <- CE.try @SomeException (force legacyHooks t >>= valToString)
        pure $ case r of
            Right s -> BC.pack s
            Left  _ -> fallback

-- | Extract the 'Val' from an 'IhcException'.
ihcExceptionToVal :: IhcException -> IO Val
ihcExceptionToVal (IhcException _ t) = force legacyHooks t

-- | @raise# :: a -> b@ — GHC primop. Compiler-intrinsic; no Haskell
-- source. Source-loaded @error@ / @throw@ / @undefined@ and the
-- partial functions in @GHC.List@ (e.g. @head []@, @tail []@) bottom
-- out into @raise#@. We wrap the exception 'Val' as an 'IhcException'
-- and throw it on the host, so the interpreter's existing exception
-- path catches it and surfaces a readable message.
--
-- We force the exception argument through 'forceToException' so that
-- if evaluating the exception value itself throws (e.g. a thunk
-- referencing an unbound helper), we still raise an informative
-- 'IhcException' instead of propagating the inner crash.
raiseHashB :: IO Val
raiseHashB = pure $ VFun $ \eT -> do
    exc <- forceToException eT
    throwIO exc

-- | @unsafeCoerce :: a -> b@ / @unsafeCoerce# :: a -> b@ — compiler-intrinsic.
--
-- GHC implements @unsafeCoerce@ in @Unsafe.Coerce@ via the magical
-- @unsafeEqualityProof@, whose recursive source body
-- @case unsafeEqualityProof of UnsafeRefl -> UnsafeRefl@ is rewritten
-- at CoreToStg.Prep time to the identity @UnsafeRefl@ (see Note
-- [Implementing unsafeCoerce], point U5, in base's @Unsafe/Coerce.hs@).
-- Without that rewrite the source definition diverges, so the module
-- cannot be source-interpreted faithfully — it is therefore a
-- legitimate whitelist entry under the project no-shim rule:
-- compiler-intrinsic, not a shim around an ordinary Haskell library.
--
-- At the 'Val' level there is no static type to violate — the value
-- representation is already dynamically tagged — so @unsafeCoerce@ is
-- simply the identity. This is the same pattern as @lazy@, @I#@, @W#@.
-- Used pervasively by @typerep-map@, @Data.Vault@, @bytestring@/@text@
-- internals, and many other libraries.
unsafeCoerceB :: IO Val
unsafeCoerceB = pure $ VFun $ \t -> force legacyHooks t

-- | @raiseIO# :: a -> State# RealWorld -> (# State# RealWorld, b #)@.
-- Backs source-loaded @throwIO e = IO (raiseIO# (toException e))@. We
-- take the exception eagerly but only raise when the world token is
-- threaded through, matching IO semantics.
raiseIOHashB :: IO Val
raiseIOHashB = pure $ VFun $ \eT -> pure $ VFun $ \_rwT -> do
    exc <- forceToException eT
    throwIO exc

-- | Force an exception-argument thunk into an 'IhcException', tolerating
-- failures during the force itself. Source-loaded exception constructors
-- (e.g. @errorCallWithCallStackException@) may reference bindings that
-- aren't yet in scope; without this guard, forcing the exception value
-- would itself crash the interpreter with an @unbound variable@ error
-- instead of raising a proper Haskell exception.
--
-- Before falling back to the raw SomeException show-text, we inspect
-- the unevaluated thunk for well-known error-constructor applications
-- (@errorCallWithCallStackException s _@, @errorCallException s@,
-- @error s@, @toException (ErrorCall s)@) and evaluate just the
-- message sub-expression.  That avoids the case where the enclosing
-- helper's body references a transitively-unbound name
-- (e.g. @currentCallStack@, which lives in @.hsc@ source we can't
-- load) but the message itself is a perfectly fine @VStr@.  The
-- result is that @head []@ reports @Prelude.head: empty list@
-- instead of @unbound variable \`currentCallStack\`@.
forceToException :: Thunk -> IO IhcException
forceToException t = do
    mShortcut <- tryShortcutMessage t
    case mShortcut of
        Just (msg, payloadT) ->
            pure (IhcException msg payloadT)
        Nothing -> do
            r <- CE.try @SomeException (force legacyHooks t)
            case r of
                Right v  -> valToIhcException v
                Left  se -> do
                    let msg = BC.pack (show se)
                    t' <- newWHNFThunk (VStr msg)
                    pure (IhcException msg t')

-- | Peek at a thunk's unevaluated expression, looking for the
-- well-known shape @HELPER msgExpr [stackExpr]@ where @HELPER@ is
-- one of the source-loaded error constructors.  If matched,
-- evaluate @msgExpr@ alone (via the thunk's own closure env) and
-- package it as a ready-made 'IhcException' message.
--
-- Returns @Nothing@ when the thunk is already evaluated or the
-- expression shape doesn't match a known helper — callers then fall
-- through to the normal force-and-unwrap path.
tryShortcutMessage :: Thunk -> IO (Maybe (ByteString, Thunk))
tryShortcutMessage t = do
    state <- readIORef t
    case state of
        Unevaluated (Closure env ipm expr) ->
            case stripHelperApp expr of
                Just msgExpr -> do
                    r <- CE.try @SomeException (do
                        msgT <- newThunkIP env ipm msgExpr
                        v    <- force legacyHooks msgT
                        pure (v, msgT))
                    case r of
                        Right (v, msgT) -> case v of
                            VStr s -> pure (Just (s, msgT))
                            _ -> do
                                -- [Char] list → try to decode.
                                r2 <- CE.try @SomeException (valToString v)
                                case r2 of
                                    Right s -> do
                                        payloadT <- newWHNFThunk (VStr (BC.pack s))
                                        pure (Just (BC.pack s, payloadT))
                                    Left _ -> pure Nothing
                        Left _ -> pure Nothing
                Nothing -> pure Nothing
        _ -> pure Nothing

-- | Recognise an @error@-family application and return the message
-- sub-expression.  Matches:
--
--   * @errorCallWithCallStackException msg stk@
--   * @errorCallException msg@
--   * @error msg@               (source-loaded)
--   * @toException (ErrorCall msg)@
stripHelperApp :: Expr -> Maybe Expr
stripHelperApp = go
  where
    go e = case e of
        -- errorCallWithCallStackException msg stk → msg
        EApp (EApp (EVar n) msg) _stk
            | bare n == BC.pack "errorCallWithCallStackException" -> Just msg
        -- errorCallException msg  or  error msg
        EApp (EVar n) msg
            | bare n == BC.pack "errorCallException" -> Just msg
            | bare n == BC.pack "error"              -> Just msg
        -- toException (ErrorCall msg)
        EApp (EVar n) inner
            | bare n == BC.pack "toException" -> stripErrorCall inner
        _ -> Nothing

    stripErrorCall e = case e of
        EApp (EVar n) msg
            | bare n == BC.pack "ErrorCall" -> Just msg
        _ -> Nothing

    bare n =
        case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
            Just idx -> BC.drop (idx + 1) n
            Nothing  -> n

-- | @raiseDivZero# :: (# #) -> b@. GHC primop invoked by source-loaded
-- numeric dispatch (e.g. @divZeroError = raise# divZeroException@ lives
-- in the wrapper). Compiler-intrinsic; no Haskell source.
raiseDivZeroB :: IO Val
raiseDivZeroB = pure $ VFun $ \_ -> do
    t <- newWHNFThunk (VStr (BC.pack "divide by zero"))
    throwIO (IhcException (BC.pack "divide by zero") t)

-- | @raiseOverflow# :: (# #) -> b@. GHC primop.
raiseOverflowB :: IO Val
raiseOverflowB = pure $ VFun $ \_ -> do
    t <- newWHNFThunk (VStr (BC.pack "arithmetic overflow"))
    throwIO (IhcException (BC.pack "arithmetic overflow") t)

-- | @raiseUnderflow# :: (# #) -> b@. GHC primop.
raiseUnderflowB :: IO Val
raiseUnderflowB = pure $ VFun $ \_ -> do
    t <- newWHNFThunk (VStr (BC.pack "arithmetic underflow"))
    throwIO (IhcException (BC.pack "arithmetic underflow") t)

-- | @catch# :: (State# RealWorld -> (# State# RealWorld, a #))
--          -> (b -> State# RealWorld -> (# State# RealWorld, a #))
--          -> State# RealWorld
--          -> (# State# RealWorld, a #)@
--
-- GHC.Prim primop, compiler-intrinsic. Source-loaded @catch@ desugars to
--
--   catch (IO io) h = IO $ catch# io handler'
--
-- so we receive the unwrapped state-transformer directly. We apply @io@ to
-- the state token; on an 'IhcException' we instead call the handler with
-- the exception value and re-thread the state. Result is an unboxed pair
-- @(# State#, a #)@ matching the primop signature.
catchHashB :: IO Val
catchHashB = pure $ VFun $ \ioT -> pure $ VFun $ \hT -> pure $ VFun $ \sT -> do
    ioV <- force legacyHooks ioT
    hV  <- force legacyHooks hT
    let runAction = do
            rRaw <- apply legacyHooks ioV sT
            runIOVal legacyHooks rRaw
    rRes <- CE.try @SomeException (CE.try @IhcException runAction)
    case rRes of
        Right (Right v) -> ensurePair v
        Right (Left exc) -> do
            rawExcVal <- ihcExceptionToVal exc
            excVal <- ensureSomeExceptionVal rawExcVal
            excT   <- newWHNFThunk excVal
            invokeHandler hV excT
        Left se -> do
            -- Non-IhcException host error — wrap & hand to handler.
            excVal <- hostExceptionToSomeExceptionVal se
            excT <- newWHNFThunk excVal
            invokeHandler hV excT
  where
    -- Ensure the result is shaped as (# State#, a #). If the IO action
    -- already returned a proper unboxed pair, pass through; otherwise wrap.
    ensurePair :: Val -> IO Val
    ensurePair v = case v of
        VCon "(#,#)" _ -> pure v
        _ -> do
            sT'  <- newWHNFThunk (VPrimObj PrimRealWorld)
            vT   <- newWHNFThunk v
            pure (VCon "(#,#)" [sT', vT])
    invokeHandler hV excT = do
        -- Handler' signature: exc -> State# -> (# State#, a #)
        r1 <- apply legacyHooks hV excT
        case r1 of
            VFun _ -> do
                sT'  <- newWHNFThunk (VPrimObj PrimRealWorld)
                rRaw <- apply legacyHooks r1 sT'
                v    <- runIOVal legacyHooks rRaw
                ensurePair v
            _ -> do
                v <- runIOVal legacyHooks r1
                ensurePair v

-- | Convert a host-thrown 'SomeException' into the Val shape that
-- source-loaded exception code expects from the @catch#@ primop.
--
-- This helper is part of the primop boundary, not a replacement for
-- source-level @catch@: ordinary exception combinators still load from
-- @GHC.Internal.IO@ / @Control.Exception@ and bottom out here.
hostExceptionToSomeExceptionVal :: SomeException -> IO Val
hostExceptionToSomeExceptionVal e = do
    descT <- newWHNFThunk =<< stringToListValIO (show e)
    handleT <- newWHNFThunk (VCon "Nothing" [])
    typeT <- newWHNFThunk (VCon "OtherError" [])
    locT <- newWHNFThunk =<< stringToListValIO ""
    errnoT <- newWHNFThunk (VCon "Nothing" [])
    fileT <- newWHNFThunk (VCon "Nothing" [])
    let ioErrVal = VCon "IOError" [handleT, typeT, locT, descT, errnoT, fileT]
    ioErrT <- newWHNFThunk ioErrVal
    pure (VCon "SomeException" [ioErrT])

ensureSomeExceptionVal :: Val -> IO Val
ensureSomeExceptionVal v = case v of
    VCon "SomeException" _ -> pure v
    _ -> do
        innerT <- newWHNFThunk v
        pure (VCon "SomeException" [innerT])

-- | @newMVar# :: State# s -> (# State# s, MVar# s a #)@
--
-- GHC.Prim primop. Source-loaded @newEmptyMVar@:
--
--   newEmptyMVar = IO $ \s -> case newMVar# s of (# s2, svar# #) -> (# s2, MVar svar# #)
--
-- Creates an empty MVar. We return an unboxed pair carrying the state and
-- the fresh 'PrimMVar'; the source pattern re-wraps it as 'MVar svar#'.
newMVarHashB :: IO Val
newMVarHashB = pure $ VFun $ \_sT -> do
    mv   <- newEmptyMVar
    sT'  <- newWHNFThunk (VPrimObj PrimRealWorld)
    mvT  <- newWHNFThunk (VPrimObj (PrimMVar mv))
    pure (VCon "(#,#)" [sT', mvT])

-- | Extract the host MVar from either a raw 'PrimMVar' (our builtin-
-- returned shape) or the source-wrapped @MVar mvar#@ VCon.
requireMVarPrim :: String -> Val -> IO (MVar Val)
requireMVarPrim fn v = case v of
    VPrimObj (PrimMVar mv) -> pure mv
    VCon "MVar" [tvT]      -> force legacyHooks tvT >>= requireMVarPrim fn
    _ -> error (fn <> ": not an MVar#: " <> showValForDebug v)

-- | @takeMVar# :: MVar# s a -> State# s -> (# State# s, a #)@
takeMVarHashB :: IO Val
takeMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "takeMVar#" mvv
    v   <- takeMVar mv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    vT  <- newWHNFThunk v
    pure (VCon "(#,#)" [sT', vT])

-- | @putMVar# :: MVar# s a -> a -> State# s -> State# s@
putMVarHashB :: IO Val
putMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "putMVar#" mvv
    av  <- force legacyHooks aT
    putMVar mv av
    pure (VPrimObj PrimRealWorld)

-- | @readMVar# :: MVar# s a -> State# s -> (# State# s, a #)@
readMVarHashB :: IO Val
readMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "readMVar#" mvv
    v   <- readMVar mv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    vT  <- newWHNFThunk v
    pure (VCon "(#,#)" [sT', vT])

-- | @tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)@
-- where the Int# is 0 if empty (a undefined) and non-zero otherwise.
tryTakeMVarHashB :: IO Val
tryTakeMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "tryTakeMVar#" mvv
    r   <- tryTakeMVar mv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    case r of
        Just v -> do
            vT  <- newWHNFThunk v
            okT <- newWHNFThunk (VInt 1)
            pure (VCon "(#,,#)" [sT', okT, vT])
        Nothing -> do
            dummyT <- newWHNFThunk (VStr (BC.pack ""))
            okT    <- newWHNFThunk (VInt 0)
            pure (VCon "(#,,#)" [sT', okT, dummyT])

-- | @tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s, Int# #)@
tryPutMVarHashB :: IO Val
tryPutMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "tryPutMVar#" mvv
    av  <- force legacyHooks aT
    ok  <- tryPutMVar mv av
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    okT <- newWHNFThunk (VInt (if ok then 1 else 0))
    pure (VCon "(#,#)" [sT', okT])

-- | @tryReadMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)@
tryReadMVarHashB :: IO Val
tryReadMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "tryReadMVar#" mvv
    r   <- tryTakeMVar mv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    case r of
        Just v -> do
            -- readMVar = takeMVar + putMVar to preserve value
            putMVar mv v
            vT  <- newWHNFThunk v
            okT <- newWHNFThunk (VInt 1)
            pure (VCon "(#,,#)" [sT', okT, vT])
        Nothing -> do
            dummyT <- newWHNFThunk (VStr (BC.pack ""))
            okT    <- newWHNFThunk (VInt 0)
            pure (VCon "(#,,#)" [sT', okT, dummyT])

-- | @isEmptyMVar# :: MVar# s a -> State# s -> (# State# s, Int# #)@
isEmptyMVarHashB :: IO Val
isEmptyMVarHashB = pure $ VFun $ \mvT -> pure $ VFun $ \_sT -> do
    mvv <- force legacyHooks mvT
    mv  <- requireMVarPrim "isEmptyMVar#" mvv
    b   <- isEmptyMVar mv
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    rT  <- newWHNFThunk (VInt (if b then 1 else 0))
    pure (VCon "(#,#)" [sT', rT])

-- | @keepAlive# :: a -> State# s -> (State# s -> b) -> b@
--
-- GHC.Prim primop. Source-loaded @withForeignPtr@:
--
--   withForeignPtr fo\@(ForeignPtr _ r) f = IO $ \s ->
--     case f (unsafeForeignPtrToPtr fo) of
--       IO action# -> keepAlive# r s action#
--
-- Ensures that the first argument (the ForeignPtrContents) is kept alive
-- while the action runs. In the interpreter we can't control GC the same
-- way; host allocation lifetime is managed via 'PrimForeignPtr' holding a
-- strong reference. We force the "keep alive" argument (to exercise any
-- pending evaluation) and then apply the continuation to the state token.
keepAliveHashB :: IO Val
keepAliveHashB = pure $ VFun $ \keepT -> pure $ VFun $ \sT -> pure $ VFun $ \kT -> do
    -- Force the "keep alive" argument so the host GC sees a live reference
    -- for the duration of the continuation. Our PrimForeignPtr / PrimPtr
    -- values hold the underlying allocation via a host ForeignPtr, so
    -- touching them here is sufficient.
    _   <- force legacyHooks keepT
    kV  <- force legacyHooks kT
    rRaw <- apply legacyHooks kV sT
    runIOVal legacyHooks rRaw

-- | @getMaskingState# :: State# RealWorld -> (# State# RealWorld, Int# #)@
--
-- GHC.Prim primop. Returns the current async-exception masking state:
--   0# = Unmasked, 1# = MaskedUninterruptible, otherwise = MaskedInterruptible.
-- The interpreter does not actually block async exceptions, so we return
-- @0#@ (Unmasked). Source-loaded @mask@ / @uninterruptibleMask@ branch on
-- this — the Unmasked branch just wraps the action; we preserve that shape.
getMaskingStateHashB :: IO Val
getMaskingStateHashB = pure $ VFun $ \_sT -> do
    sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
    iT  <- newWHNFThunk (VInt 0)
    pure (VCon "(#,#)" [sT', iT])

-- | @maskAsyncExceptions# :: (State# RealWorld -> (# State# RealWorld, a #))
--                        -> State# RealWorld
--                        -> (# State# RealWorld, a #)@
--
-- Also serves @maskUninterruptible#@ and @unmaskAsyncExceptions#@ — all
-- three are identity on the IO action at the interpreter level, since we
-- do not deliver async exceptions via masking primitives.
maskAsyncExceptionsHashB :: IO Val
maskAsyncExceptionsHashB = pure $ VFun $ \ioT -> pure $ VFun $ \sT -> do
    ioV  <- force legacyHooks ioT
    rRaw <- apply legacyHooks ioV sT
    v    <- runIOVal legacyHooks rRaw
    case v of
        VCon "(#,#)" _ -> pure v
        _ -> do
            sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
            vT  <- newWHNFThunk v
            pure (VCon "(#,#)" [sT', vT])

-- | @toExceptionWithBacktrace :: (HasCallStack, Exception e) => e -> IO SomeException@
--
-- Source defined in @GHC.Internal.Exception@:
--
--   toExceptionWithBacktrace e
--     | backtraceDesired e = do bt <- collectBacktraces
--                               return (addExceptionContext bt (toException e))
--     | otherwise          = return (toException e)
--
-- Wiring ghc-internal through the import resolver is a separate task; we
-- shim it here. The backtrace is cosmetic at the Val level (no stack
-- traces are captured by the interpreter), and 'extractExceptionMessage'
-- already unwraps 'SomeException'.
toExceptionWithBacktraceB :: IO Val
toExceptionWithBacktraceB = pure $ VFun $ \eT -> pure $ VIO $ do
    ev <- force legacyHooks eT
    case ev of
        VCon "SomeException" _ -> pure ev
        _                       -> do
            eT' <- newWHNFThunk ev
            pure (VCon "SomeException" [eT'])

-- | @toException :: Exception e => e -> SomeException@ — identity-with-wrap
-- at the Val level (we lack the Exception class dispatch; SomeException is
-- idempotent). Complements 'toExceptionWithBacktraceB' for the pure throw
-- path (@throwIO e = IO (raiseIO# (toException e))@).
toExceptionB :: IO Val
toExceptionB = pure $ VFun $ \eT -> do
    ev <- force legacyHooks eT
    case ev of
        VCon "SomeException" _ -> pure ev
        _                       -> do
            eT' <- newWHNFThunk ev
            pure (VCon "SomeException" [eT'])

-- | @fromException :: Exception e => SomeException -> Maybe e@.
-- Real GHC is type-directed.  At the Val level we return @Just@ for
-- 'SomeException' inputs; 'matchPat' supplies the limited downcast
-- behavior we can infer from demanded constructor patterns.
fromExceptionB :: IO Val
fromExceptionB = pure $ VFun $ \eT -> do
    -- 'fromException :: forall e. Exception e => SomeException -> Maybe e'
    -- is type-driven in real Haskell: it returns 'Just' only if the
    -- 'SomeException' wraps a value of type 'e'.  Without type info at
    -- runtime our previous "always Just" implementation made guards
    -- like @Just (ExceptionInsideResponseBody _) <- fromException e@
    -- match every exception, and downcast queries like
    -- @case fromException e of Just (SomeAsyncException _) -> True ;
    -- Nothing -> False@ raise 'PatternMatchFail' when @e@ is a plain
    -- IOError (@Just (IOError ...)@ doesn't match @Just
    -- (SomeAsyncException _)@ AND doesn't match @Nothing@).
    --
    ev <- force legacyHooks eT
    case ev of
        VCon "SomeException" _ -> do
            evT <- newWHNFThunk ev
            pure (VCon "Just" [evT])
        _ -> pure (VCon "Nothing" [])

-- | @unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)@
--
-- Source at @GHC.Internal.Base@: @unIO (IO a) = a@. At the Val level VIO
-- hides the state-transformer shape, so we reconstruct it.
unIOB :: IO Val
unIOB = pure $ VFun $ \ioT -> pure $ VFun $ \sT -> do
    ioV <- force legacyHooks ioT
    case ioV of
        VCon "IO" [stateFnT] -> do
            stateFn <- force legacyHooks stateFnT
            apply legacyHooks stateFn sT
        _ -> do
            v   <- runIOVal legacyHooks ioV
            sT' <- newWHNFThunk (VPrimObj PrimRealWorld)
            vT  <- newWHNFThunk v
            pure (VCon "(#,#)" [sT', vT])

ioToSTB :: IO Val
ioToSTB = pure $ VFun $ \ioT -> do
    ioV <- force legacyHooks ioT
    case ioV of
        VCon "IO" [stateFnT] -> pure (VIO (runStateTransformer stateFnT))
        _                   -> pure ioV

stToIOB :: IO Val
stToIOB = pure $ VFun $ \stT -> do
    stV <- force legacyHooks stT
    case stV of
        VCon "ST" [stateFnT] -> pure (VIO (runStateTransformer stateFnT))
        _                   -> pure stV

runStateTransformer :: Thunk -> IO Val
runStateTransformer stateFnT = do
    stateFn <- force legacyHooks stateFnT
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    raw <- apply legacyHooks stateFn stT
    res <- runIOVal legacyHooks raw
    case res of
        VCon "(#,#)" [_stateT, resultT] -> force legacyHooks resultT
        _ -> pure res

--------------------------------------------------------------------------------
-- Phase 2.9.5: Typeable / TypeRep / cast / Dynamic builtins
--------------------------------------------------------------------------------
-- Runtime representation:
--   TypeRep  = VCon "TypeRep"  [tyConThunk, argsListThunk]
--   TyCon    = VCon "TyCon"    [nameCharListThunk]
--   Dynamic  = VCon "Dynamic"  [typeRepThunk, valThunk]
--   Typeable dict = VCon "Dict_Typeable" [typeRepThunk]

typeOfB :: IO Val
typeOfB = pure $ VFun $ \dictT -> pure $ VFun $ \_valT -> do
    dictV <- force legacyHooks dictT
    extractTypeRep dictV

castB :: IO Val
castB = pure $ VFun $ \dictAT -> pure $ VFun $ \dictBT -> pure $ VFun $ \valT -> do
    dictAV <- force legacyHooks dictAT
    dictBV <- force legacyHooks dictBT
    trA    <- extractTypeRep dictAV
    trB    <- extractTypeRep dictBV
    eq     <- typeRepEq trA trB
    if eq then pure (VCon "Just" [valT])
          else pure (VCon "Nothing" [])

eqTB :: IO Val
eqTB = pure $ VFun $ \dictAT -> pure $ VFun $ \dictBT -> do
    dictAV <- force legacyHooks dictAT
    dictBV <- force legacyHooks dictBT
    trA    <- extractTypeRep dictAV
    trB    <- extractTypeRep dictBV
    eq     <- typeRepEq trA trB
    if eq
        then do { reflT <- newWHNFThunk (VCon "Refl" []); pure (VCon "Just" [reflT]) }
        else pure (VCon "Nothing" [])

mkTyCon3B :: IO Val
mkTyCon3B = pure $ VFun $ \_ -> pure $ VFun $ \_ -> pure $ VFun $ \nameT -> do
    nameV    <- force legacyHooks nameT
    nameStrT <- newWHNFThunk nameV
    pure (VCon "TyCon" [nameStrT])

mkTyConAppB :: IO Val
mkTyConAppB = pure $ VFun $ \tyConT -> pure $ VFun $ \argsT -> do
    tyConV     <- force legacyHooks tyConT
    argsV      <- force legacyHooks argsT
    tyConThunk <- newWHNFThunk tyConV
    argsThunk  <- newWHNFThunk argsV
    pure (VCon "TypeRep" [tyConThunk, argsThunk])

-- | Extract a TypeRep from a Typeable dict or raw TypeRep value.
extractTypeRep :: Val -> IO Val
extractTypeRep (VCon "Dict_Typeable" [trT]) = force legacyHooks trT
extractTypeRep v@(VCon "TrType" _)          = pure v
extractTypeRep v@(VCon "TrTyCon" _)         = pure v
extractTypeRep v@(VCon "TrApp" _)           = pure v
extractTypeRep v@(VCon "TrFun" _)           = pure v
extractTypeRep (VCon "SomeTypeRep" [trT])   = force legacyHooks trT
extractTypeRep _                            = mkTypeRep "Unknown"

-- | Build built-in Typeable instance dictionaries for well-known types.
--
-- Each dict is registered as a 'LazyBuiltin' thunk so startup doesn't pay
-- the cost of allocating a TypeRep + wrapper VCon for every primitive
-- type — a hello-world program never touches any of these.
buildBuiltinTypeableInsts :: ClassRegistry -> IO [(Name, Thunk)]
buildBuiltinTypeableInsts reg = mapM mkDict prims
  where
    prims :: [(Name, Name)]
    prims =
        [ ("Int",     "Int")
        , ("Int8",    "Int8")
        , ("Int16",   "Int16")
        , ("Int32",   "Int32")
        , ("Int64",   "Int64")
        , ("Word",    "Word")
        , ("Word8",   "Word8")
        , ("Word16",  "Word16")
        , ("Word32",  "Word32")
        , ("Word64",  "Word64")
        , ("Char",    "Char")
        , ("Bool",    "Bool")
        , ("Double",  "Double")
        , ("Float",   "Float")
        , ("Integer", "Integer")
        , ("()",      "()")
        , ("[]",      "[]")
        , ("Maybe",   "Maybe")
        , ("Either",  "Either")
        , ("(,)",     "(,)")
        , ("IO",      "IO")
        ]
    mkDict (tag, tyName) = do
        dictT <- newLazyBuiltinThunk $ do
            tr  <- mkTypeRep tyName
            trT <- newWHNFThunk tr
            pure (VCon "Dict_Typeable" [trT])
        -- Typeable instances are emitted by the compiler, not declared in
        -- Haskell source. Publish their nullary method through the same class
        -- registry used by source-defined instances so constrained source
        -- functions can capture and resolve it normally.
        tr <- mkTypeRep tyName
        registerInstance reg (BC.pack "Typeable") tag
            (HashMap.fromList
                [ (BC.pack "typeRep#", tr) ])
        pure ("typeableDict_" <> tag, dictT)

typeRepHashDispatcher :: ClassRegistry -> Val
typeRepHashDispatcher reg = self
  where
    self = VClassMethod (BC.pack "Typeable") (BC.pack "typeRep#") 0 [] $ \tags _ ->
        case tags of
            (tag:_) -> do
                m <- lookupInstanceMethod reg (BC.pack "Typeable")
                        (normalizeTyTag tag) (BC.pack "typeRep#")
                maybe (pure self) (forceMethodVal legacyHooks) m
            [] -> pure self
