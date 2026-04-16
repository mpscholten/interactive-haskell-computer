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
    , buildFieldEnv
    , showValWith
    ) where

import Control.Concurrent
    ( forkIO, killThread, myThreadId, threadDelay
    )
import Control.Concurrent.MVar
    ( MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar
    , modifyMVar_, modifyMVar, tryTakeMVar, tryPutMVar, isEmptyMVar
    , withMVar, swapMVar
    )
import Control.Concurrent.STM
    ( TVar, atomically, retry, orElse, check
    , newTVar, newTVarIO, readTVar, writeTVar, modifyTVar, modifyTVar', readTVarIO
    , STM
    )
import GHC.Conc.Sync (unsafeIOToSTM)
import qualified Control.Exception as CE
import Control.Exception
    ( throwIO, catch, try, evaluate, mask, mask_
    , bracket, bracket_, finally, onException, throwTo
    , SomeException, IOException
    , Exception(..)
    )
import Data.Bits
    ( (.&.), (.|.), xor, complement, shiftL, shiftR
    , popCount, countLeadingZeros, finiteBitSize
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word64)
import Foreign.C.String (peekCAString)
import Foreign.ForeignPtr
    ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr, touchForeignPtr
    , newForeignPtr_
    , plusForeignPtr
    )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr, minusPtr)
import qualified Foreign.Ptr as FP
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff, sizeOf)
import System.Exit (ExitCode(..), exitWith)
import System.IO
    ( BufferMode(..)
    , Handle
    , IOMode(..)
    , hClose
    , hFlush
    , hGetLine
    , hPutBuf
    , hPutStr
    , hPutStrLn
    , hSetBuffering
    , openFile
    , stderr
    , stdin
    , stdout
    )

import IHC.AST  (Name)
import IHC.Classes
    ( ClassRegistry, lookupInstance, typeTagOf
    , mkTypeRep, typeRepEq
    )
import IHC.Eval (apply, force)
import IHC.Scan (DataRegistry, FieldRegistry)
import IHC.TH (thBuiltinPairs)
import IHC.Val

mkForeignPtrVal :: ForeignPtr Word8 -> IO Val
mkForeignPtrVal fp = do
    addrT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr (unsafeForeignPtrToPtr fp))))
    gutsT <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    pure (VCon "ForeignPtr" [addrT, gutsT])

ptrValToPtr :: Val -> IO (Ptr Word8)
ptrValToPtr (VPrimObj (PrimPtr p)) = pure p
ptrValToPtr (VCon "Ptr" [pT]) = force pT >>= ptrValToPtr
ptrValToPtr other = error ("expected Ptr: " <> showValForDebug other)

foreignPtrValToForeignPtr :: Val -> IO (ForeignPtr Word8)
foreignPtrValToForeignPtr (VPrimObj (PrimForeignPtr fp)) = pure fp
foreignPtrValToForeignPtr (VCon "ForeignPtr" [addrT, gutsT]) = do
    gv <- force gutsT
    case gv of
        VPrimObj (PrimForeignPtr fp) -> pure fp
        -- Source-loaded code can construct ForeignPtr values whose guts are
        -- constructors like FinalPtr rather than our host PrimForeignPtr.
        -- Rebuild an equivalent host ForeignPtr from the raw address so the
        -- RTS-backed pointer builtins can still operate on it.
        _ -> do
            addrV <- force addrT
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
    pairs <- mapM (\(n, mkV) -> do { v <- mkV; t <- newWHNFThunk v; pure (n, t) })
                  (builtins reg)
    -- Built-in list constructors.
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
    -- Standard handles.
    stdinT  <- newWHNFThunk (VPrimObj (PrimHandle stdin))
    stdoutT <- newWHNFThunk (VPrimObj (PrimHandle stdout))
    stderrT <- newWHNFThunk (VPrimObj (PrimHandle stderr))
    let handles = [("stdin", stdinT), ("stdout", stdoutT), ("stderr", stderrT)]
    -- Builtin Maybe constructors (commonly needed without an explicit data decl).
    nothingT <- newWHNFThunk (VCon "Nothing" [])
    justT    <- newWHNFThunk (VFun $ \x -> pure (VCon "Just" [x]))
    let maybeCtors = [("Nothing", nothingT), ("Just", justT)]
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
    -- The parser builds EApp chains using these names.
    unbox2T <- newWHNFThunk (VFun $ \a -> pure $ VFun $ \b -> pure (VCon "(#,#)" [a, b]))
    unbox3T <- newWHNFThunk (VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure (VCon "(#,,#)" [a, b, c]))
    let unboxCtors = [("(#,#)", unbox2T), ("(#,,#)", unbox3T)]
    -- ExitCode constructors.
    exitSuccT <- newWHNFThunk (VCon "ExitSuccess" [])
    exitFailT <- newWHNFThunk (VFun $ \n -> pure (VCon "ExitFailure" [n]))
    let exitCtors = [("ExitSuccess", exitSuccT), ("ExitFailure", exitFailT)]
    -- Either constructors: always present regardless of whether Data.Either
    -- is source-loaded. Previously provided implicitly via GHC.* stubs; now
    -- explicitly registered so they survive whitelist minimisation (GAP-3).
    leftT  <- newWHNFThunk (VFun $ \x -> pure (VCon "Left"  [x]))
    rightT <- newWHNFThunk (VFun $ \x -> pure (VCon "Right" [x]))
    let eitherCtors = [("Left", leftT), ("Right", rightT)]
    -- IO constructor: IO wraps a (State# RealWorld -> (# State# RealWorld, a #))
    -- function into a VIO action.  GHC.Types defines `newtype IO a = IO (State# ...)`
    -- but since GHC.Types is builtin-backed (no .hs source), we register it here.
    ioCtorT <- newWHNFThunk (VFun $ \fThunk -> do
        f <- force fThunk
        pure (VIO (do
            let stok = VPrimObj PrimRealWorld
            stokT <- newWHNFThunk stok
            r <- apply f stokT
            case r of
                VCon "(#,#)" [_stT, aT] -> do
                    v <- force aT
                    pure v
                other                    -> pure other)))
    -- Ptr smart constructor: `Ptr addr#` wraps an Addr# (PrimPtr) back into PrimPtr.
    ptrCtorT <- newWHNFThunk ptrCtorV
    -- Phase 2.9.5: Proxy and Dynamic constructors.
    -- Phase 3.5 note: when a VLabel is used where a Proxy is expected,
    -- fromLabel produces VCon "Proxy" [VLabel name].
    proxyT    <- newWHNFThunk (VCon "Proxy" [])
    dynCtorV  <- dynamicCtorB
    dynCtorT  <- newWHNFThunk dynCtorV
    let phase295Ctors = [("Proxy", proxyT), ("Dynamic", dynCtorT)]
    -- Phase 2.9.5: Built-in Typeable dictionaries for primitive types.
    typeableInsts <- buildBuiltinTypeableInsts
    pure (extendEnvMany (pairs ++ listCtors ++ boolish ++ ioModes ++ handles
                         ++ maybeCtors ++ orderingCtors ++ exitCtors ++ unboxCtors
                         ++ unitCtor ++ eitherCtors ++ [("IO", ioCtorT)]
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
        v <- force addrT
        case v of
            VPrimObj (PrimPtr p) -> pure (VPrimObj (PrimPtr p))
            _                   -> pure (VCon "Ptr" [addrT])  -- fallback

builtins :: ClassRegistry -> [(Name, IO Val)]
builtins reg =
    -- Arithmetic (TODO 2.6: replace with Num class dispatch)
    [ ("+",        binOpNum (+) (+))
    , ("-",        binOpNum (-) (-))
    , ("*",        binOpNum (*) (*))
    , ("/",        binOpFloat (/))
    , ("mod",      binOpInt mod)
    , ("div",      binOpInt div)
    , ("negate",   unaryOpNum negate negate)
    , ("abs",      unaryOpNum abs abs)
    , ("signum",   unaryOpNum signum signum)
    , ("succ",     unaryOpNum (+1) (+1))
    , ("pred",     unaryOpNum (subtract 1) (subtract 1))
    , ("min",      binOpNum min min)
    , ("max",      binOpNum max max)
    , ("gcd",      binOpInt gcd)
    , ("subtract", binOpNum (\a b -> b - a) (\a b -> b - a))
    , ("sqrt",     unaryOpFloat sqrt)
    , ("floor",    floatToIntB floor)
    , ("ceiling",  floatToIntB ceiling)
    , ("round",    floatToIntB round)
    , ("truncate", floatToIntB truncate)
    , ("fromIntegral", fromIntegralB)
    , ("fromInteger",  fromIntegralB)
    , (".",        compose)
    -- Comparisons: Phase 2.3 dispatch via ClassRegistry.
    -- Builtin instances for Int, Char, Bool, [] are handled inline;
    -- user-defined instances are looked up from the registry.
    , ("==",       eqDispatch reg)
    , ("/=",       neqDispatch reg)
    , ("<",        ordDispatch reg 0)
    , ("<=",       ordDispatch reg 1)
    , (">",        ordDispatch reg 2)
    , (">=",       ordDispatch reg 3)
    , ("compare",  compareDispatch reg)
    , ("even",     evenB)
    , ("odd",      oddB)
    , ("not",      notB)
    -- Boolean
    , ("&&",       andB)
    , ("||",       orB)
    -- Strings / lists (strings are [Char] from Phase 2.2 onward)
    , ("++",       listConcat)
    , ("show",     showDispatch reg)
    , ("length",   lengthB)
    -- IO
    , ("putStrLn", putStrLnB)
    , ("putStr",   putStrB)
    , ("print",    printDispatch reg)
    , ("putChar",  putCharB)
    , ("getLine",  getLineB)
    -- Monad core: >>=  and >>  dispatch via class registry for non-IO monads
    -- (e.g. ST s a, State s, Maybe, etc.) while falling back to the plain IO
    -- implementation for VIO values.
    , (">>=",      bindDispatch reg)
    , (">>",       seqDispatch reg)
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
    , ("readFile",    readFileB)
    , ("writeFile",   writeFileB)
    , ("appendFile",  appendFileB)
    -- Control flow
    , ("seq",         seqB)
    , ("$!",          dollarBangB)
    , ("assert",      assertB)
    , ("error",       errorB)
    , ("undefined",   undefinedB)
    , ("exitWith",    exitWithB)
    , ("exitSuccess", exitSuccessB)
    -- Char / numeric conversions
    , ("ord",         ordB)
    , ("chr",         chrB)
    , ("fromIntegral", fromIntegralB)
    -- Phase 2.8: RealWorld / State primops
    , ("realWorld#",               realWorldB)
    , ("noDuplicate#",            noDuplicateB)  -- GHC primop: no-op in interpreter
    , ("runRW#",                   runRWB)
    , ("lazy",                     lazyB)
    -- Phase 2.8: unsafePerformIO family
    , ("unsafePerformIO",          unsafePerformIOB)
    , ("unsafeDupablePerformIO",   unsafePerformIOB)
    , ("accursedUnutterablePerformIO", unsafePerformIOB)
    -- Phase 2.8: boxing/unboxing constructors
    , ("I#",  iHashB)
    , ("W#",  wHashB)
    , ("W8#", w8HashB)
    , ("C#",  cHashB)
    -- Phase 2.8: Addr# primitives
    , ("nullAddr#",   nullAddrB)
    , ("plusAddr#",   plusAddrB)
    , ("minusAddr#",  minusAddrB)
    , ("addr2Int#",   addr2IntB)
    -- Phase 2.8: Ptr arithmetic
    , ("plusPtr",   plusPtrB)
    , ("minusPtr",  minusPtrB)
    , ("nullPtr",   nullPtrB)
    , ("castPtr",   castPtrB)
    -- Phase 2.8: ForeignPtr
    , ("mallocPlainForeignPtrBytes", mallocForeignPtrBytesB)
    , ("mallocForeignPtrBytes",      mallocForeignPtrBytesB)
    , ("withForeignPtr",             withForeignPtrB)
    , ("unsafeWithForeignPtr",       withForeignPtrB)
    , ("plusForeignPtr",             plusForeignPtrB)
    , ("touchForeignPtr",            touchForeignPtrB)
    , ("newForeignPtr_",             newForeignPtr_B)
    -- Phase 2.8: Storable ops on Ptr
    , ("peek",         peekB)
    , ("poke",         pokeB)
    , ("peekByteOff",  peekByteOffB)
    , ("pokeByteOff",  pokeByteOffB)
    -- Phase 2.8: MutableByteArray# family
    , ("newByteArray#",             newByteArrayB)
    , ("writeWord8Array#",          writeWord8ArrayB)
    , ("readWord8Array#",           readWord8ArrayB)
    , ("indexWord8Array#",          indexWord8ArrayB)
    , ("unsafeFreezeByteArray#",    unsafeFreezeByteArrayB)
    , ("getSizeofMutableByteArray#", getSizeofMutableByteArrayB)
    , ("sizeofByteArray#",          sizeofByteArrayB)
    , ("copyAddrToByteArray#",      copyAddrToByteArrayB)
    , ("copyByteArrayToAddr#",      copyByteArrayToAddrB)
    -- Phase 2.8: C memory ops
    , ("memcpy",     memcpyB)
    , ("memcpyFp",   memcpyFpB)
    , ("copyBytes",  copyBytesB)  -- Foreign.Marshal.Utils.copyBytes: wraps memcpy (primop-backed, no Haskell source)
    , ("memset",     memsetB)
    , ("memchr",     memchrB)
    , ("memcmp",     memcmpB)
    , ("c_strlen",   cStrlenB)
    -- Phase 2.8: buffered I/O
    , ("hPutBuf",    hPutBufB)
    -- Phase 2.8: Int/Word coercions + bit ops
    , ("int2Word#",         int2WordB)
    , ("word2Int#",         word2IntB)
    , ("or#",               orHashB)
    , ("and#",              andHashB)
    , ("xor#",              xorHashB)
    , ("not#",              notHashB)
    , ("uncheckedShiftL#",  uncheckedShiftLB)
    , ("uncheckedShiftRL#", uncheckedShiftRLB)
    , ("timesInt2#",        timesInt2B)
    , ("timesWord2#",       timesWord2B)
    -- Phase 2.8: GHC.Exts Word# comparison primops (for containers)
    , ("ltWord#",   ltWordB)
    , ("leWord#",   leWordB)
    , ("eqWord#",   eqWordB)
    , ("gtWord#",   gtWordB)
    , ("geWord#",   geWordB)
    , ("minusWord#", minusWordB)
    , ("plusWord#",  plusWordB)
    , ("timesWord#", timesWordB)
    , ("quotWord#",  quotWordB)
    , ("remWord#",   remWordB)
    , ("popCnt#",    popCntB)
    , ("indexOfTheOnlyBit#", indexOfTheOnlyBitB)
    -- Phase 2.8: Int# arithmetic primops
    , ("negateInt#",   negateIntB)
    , ("quotInt#",     quotIntB)
    , ("remInt#",      remIntB)
    , ("quotRemInt#",  quotRemIntB)
    , ("addIntC#",     addIntCB)
    , ("subIntC#",     subIntCB)
    , ("mulIntMayOflo#", mulIntMayOfloB)
    -- Phase 2.8: misc
    , ("cstringLength#",  cstringLengthB)
    , ("unpackCString#",  unpackCStringB)
    , ("unpackCStringUtf8#", unpackCStringB)
    , ("sizeOf",       sizeOfB)
    , ("alignment",    alignmentB)
    -- Phase 2.8: additional numeric ops needed by containers
    , ("fromInteger",  fromIntegralB)
    , ("toInteger",    fromIntegralB)
    , ("quot",         binOpInt quot)
    , ("rem",          binOpInt rem)
    , ("div",          binOpInt div)
    , ("divMod",       divModB)
    , ("quotRem",      quotRemB)
    , ("shiftL",       shiftLB)
    , ("shiftR",       shiftRB)
    , (".&.",          bitAndB)
    , (".|.",          bitOrB)
    , ("xor",          bitXorB)
    , ("complement",   bitComplementB)
    , ("popCount",     popCountB)
    , ("bit",          bitB)
    , ("testBit",      testBitB)
    , ("clearBit",     clearBitB)
    , ("setBit",       setBitB)
    -- Power operator and range
    , ("^",            powOpB)
    , ("^^",           powFloatOpB)
    , ("**",           powFloatOpB)
    , ("enumFromTo",   enumFromToB)
    , ("enumFromThenTo", enumFromThenToB)
    -- Phase 2.10a: concurrency primitives
    , ("forkIO",          forkIOB)
    , ("killThread",      killThreadB)
    , ("myThreadId",      myThreadIdB)
    , ("threadDelay",     threadDelayB)
    , ("getNumCapabilities", getNumCapabilitiesB)
    -- Phase 2.10a: MVar
    , ("newMVar",         newMVarB)
    , ("newEmptyMVar",    newEmptyMVarB)
    , ("takeMVar",        takeMVarB)
    , ("putMVar",         putMVarB)
    , ("readMVar",        readMVarB)
    , ("modifyMVar_",     modifyMVar_B)
    , ("modifyMVar",      modifyMVarB)
    , ("tryTakeMVar",     tryTakeMVarB)
    , ("tryPutMVar",      tryPutMVarB)
    , ("isEmptyMVar",     isEmptyMVarB)
    , ("withMVar",        withMVarB)
    , ("swapMVar",        swapMVarB)
    -- Phase 2.10a: STM
    , ("atomically",      atomicallyB)
    , ("retry",           retryB)
    , ("orElse",          orElseB)
    , ("check",           checkB)
    , ("newTVar",         newTVarB)
    , ("newTVarIO",       newTVarIOB)
    , ("readTVar",        readTVarB)
    , ("writeTVar",       writeTVarB)
    , ("modifyTVar'",     modifyTVar'B)
    , ("modifyTVar",      modifyTVar'B)
    , ("readTVarIO",      readTVarIOB)
    -- Phase 2.10a: exceptions
    , ("throwIO",         throwIOB)
    , ("throw",           throwIOB)
    -- GHC primops from GHC.Prim: compiler-intrinsic, no Haskell source.
    -- Source-loaded `error`, `throw`, `undefined`, `head []`, numeric
    -- overflow paths etc. all bottom out into these. See commit message
    -- / CLAUDE.md builtin rule: compiler-built + RTS-exclusive.
    , ("raise#",          raiseHashB)
    , ("raiseIO#",        raiseIOHashB)
    , ("raiseDivZero#",   raiseDivZeroB)
    , ("raiseOverflow#",  raiseOverflowB)
    , ("raiseUnderflow#", raiseUnderflowB)
    , ("catch",           catchB)
    , ("handle",          handleB)
    , ("try",             tryB)
    , ("evaluate",        evaluateB)
    , ("mask_",           mask_B)
    , ("mask",            maskB)
    , ("uninterruptibleMask_", mask_B)
    , ("bracket",         bracketB)
    , ("bracket_",        bracket_B)
    , ("finally",         finallyB)
    , ("onException",     onExceptionB)
    , ("throwTo",         throwToB)
    , ("displayException", displayExceptionB)
    -- Phase 2.9.5: Typeable / TypeRep / cast / Dynamic
    , ("typeRep",        typeRepB)
    , ("typeOf",         typeOfB)
    , ("cast",           castB)
    , ("eqT",            eqTB)
    , ("toDyn",          toDynB)
    , ("fromDynamic",    fromDynamicB)
    , ("fromDyn",        fromDynB)
    , ("dynTypeRep",     dynTypeRepB)
    , ("mkTyCon3",       mkTyCon3B)
    , ("mkTyConApp",     mkTyConAppB)
    , ("tyConName",      tyConNameB)
    , ("typeRepTyCon",   typeRepTyConB)
    , ("typeRepArgs",    typeRepArgsB)
    -- Phase 3.5: OverloadedLabels
    , ("fromLabel",    fromLabelB reg)
    -- Phase 3.6: MutVar# primops (backing ST monad source).
    -- GHC.Prim has no .hs source; these are wired-in by the GHC build system.
    , ("newMutVar#",            newMutVarB)
    , ("readMutVar#",           readMutVarB)
    , ("writeMutVar#",          writeMutVarB)
    , ("atomicModifyMutVar#",   atomicModifyMutVarB)
    , ("atomicModifyMutVar2#",  atomicModifyMutVar2B)
    , ("atomicModifyMutVar_#",  atomicModifyMutVarUB)
    , ("casMutVar#",            casMutVarB)
    -- Phase 2.11: TH Lift builtins.
    ] ++ thBuiltinPairs

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

-- | Polymorphic binary op for Int and Float.
-- If either argument is a Float, both are promoted.
binOpNum :: (Int64 -> Int64 -> Int64) -> (Double -> Double -> Double) -> IO Val
binOpNum intOp floatOp = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VInt x,   VInt y)   -> pure (VInt   (intOp   x y))
        (VFloat x, VFloat y) -> pure (VFloat (floatOp x y))
        (VInt x,   VFloat y) -> pure (VFloat (floatOp (fromIntegral x) y))
        (VFloat x, VInt y)   -> pure (VFloat (floatOp x (fromIntegral y)))
        _ -> error ("binOpNum: non-numeric args: "
                    <> showValForDebug av <> ", " <> showValForDebug bv)

-- | Float-only binary op (division etc.).
binOpFloat :: (Double -> Double -> Double) -> IO Val
binOpFloat op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    let toD (VFloat d) = d
        toD (VInt n)   = fromIntegral n
        toD v          = error ("binOpFloat: non-numeric: " <> showValForDebug v)
    pure (VFloat (op (toD av) (toD bv)))

unaryOpInt :: (Int64 -> Int64) -> IO Val
unaryOpInt op = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt x -> pure (VInt (op x))
        _ -> error ("unaryOp: non-Int arg: " <> showValForDebug av)

-- | Polymorphic unary op for Int and Float.
unaryOpNum :: (Int64 -> Int64) -> (Double -> Double) -> IO Val
unaryOpNum intOp floatOp = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n   -> pure (VInt   (intOp   n))
        VFloat d -> pure (VFloat (floatOp d))
        _ -> error ("unaryOpNum: non-numeric arg: " <> showValForDebug av)

-- | Float-only unary op.
unaryOpFloat :: (Double -> Double) -> IO Val
unaryOpFloat op = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VFloat d -> pure (VFloat (op d))
        VInt n   -> pure (VFloat (op (fromIntegral n)))
        _ -> error ("unaryOpFloat: non-numeric arg: " <> showValForDebug av)

-- | floor/ceiling/round/truncate — Float -> Int.
floatToIntB :: (Double -> Int64) -> IO Val
floatToIntB op = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VFloat d -> pure (VInt (op d))
        VInt n   -> pure (VInt n)
        _ -> error ("floatToInt: non-numeric arg: " <> showValForDebug av)

-- | @(.) :: (b -> c) -> (a -> b) -> a -> c@. Built as a three-arg
-- curried 'VFun'. @f@ and @g@ are held as thunks; @x@ is passed to @g@,
-- whose result is forced and handed to @f@.
compose :: IO Val
compose = pure $ VFun $ \fT -> pure $ VFun $ \gT -> pure $ VFun $ \xT -> do
    fv <- force fT
    gv <- force gT
    gx <- apply gv xT
    gxT <- newWHNFThunk gx
    apply fv gxT

-- cmpInt removed in Phase 2.3 — replaced by eqDispatch/ordDispatch

-- | Boolean-returning version of a comparison: returns VCon "True" or "False".
boolVal :: Bool -> Val
boolVal True  = VCon "True"  []
boolVal False = VCon "False" []

-- | Test for truthy value: VCon "True"/VInt non-zero is True.
isTruthy :: Val -> Bool
isTruthy (VCon "True" _)  = True
isTruthy (VCon "False" _) = False
isTruthy (VInt 0)         = False
isTruthy (VInt _)         = True
isTruthy other = error ("isTruthy: not a Bool: " <> showValForDebug other)

notB :: IO Val
notB = pure $ VFun $ \a -> do
    av <- force a
    pure (boolVal (not (isTruthy av)))

evenB :: IO Val
evenB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (boolVal (even n))
        _ -> error ("even: not an Int: " <> showValForDebug av)

oddB :: IO Val
oddB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (boolVal (odd n))
        _ -> error ("odd: not an Int: " <> showValForDebug av)

andB :: IO Val
andB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    if isTruthy av
        then do
            bv <- force b
            pure (boolVal (isTruthy bv))
        else pure (boolVal False)

orB :: IO Val
orB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    if isTruthy av
        then pure (boolVal True)
        else do
            bv <- force b
            pure (boolVal (isTruthy bv))

--------------------------------------------------------------------------------
-- Phase 2.3: type-class dispatch for Eq, Ord, Show
--
-- For Int, Char, Bool, List: handled inline.
-- For user-defined types: look up the ClassRegistry.
--------------------------------------------------------------------------------

-- | Eq dispatch: look up "==" method from the class registry.
-- Method slot 0 = (==), slot 1 = (/=).
eqDispatch :: ClassRegistry -> IO Val
eqDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    eqVals reg av bv

-- | /= dispatch.
neqDispatch :: ClassRegistry -> IO Val
neqDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    r <- eqVals reg av bv
    pure (boolVal (not (isTruthy r)))

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
    (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2)) ->
        pure (boolVal (p1 == p2))
    (VPrimObj (PrimForeignPtr fp1), VPrimObj (PrimForeignPtr fp2)) ->
        pure (boolVal (fp1 == fp2))
    (VUnit, VUnit)      -> pure (boolVal True)
    (VCon "True" _, VCon "True" _)   -> pure (boolVal True)
    (VCon "False" _, VCon "False" _) -> pure (boolVal True)
    (VCon "True" _, VCon "False" _)  -> pure (boolVal False)
    (VCon "False" _, VCon "True" _)  -> pure (boolVal False)
    (VCon n1 ts1, VCon n2 ts2)
        | n1 /= n2  -> pure (boolVal False)
        | otherwise -> do
            -- Check field-by-field.
            results <- mapM (\(t1, t2) -> do
                v1 <- force t1
                v2 <- force t2
                eqVals reg v1 v2)
                (zip ts1 ts2)
            pure (boolVal (all isTruthy results))
    _ -> do
        -- Try user-defined instance.
        let tag = typeTagOf av
        mMethods <- lookupInstance reg "Eq" tag
        case mMethods of
            Just (eqMethod : _) -> do
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply eqMethod aT
                apply r1 bT
            _ -> error ("(==): no Eq instance for type tag `"
                        <> BC.unpack tag <> "`: "
                        <> showValForDebug av)

-- | Ord dispatch. Slot in the method list:
--   0 = (<), 1 = (<=), 2 = (>), 3 = (>=), 4 = compare
-- We implement all four directly for builtin types and use
-- registry lookup for user-defined types.
ordDispatch :: ClassRegistry -> Int -> IO Val
ordDispatch reg slot = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    ordCmp reg slot av bv

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
    _ -> do
        let tag = typeTagOf av
        mMethods <- lookupInstance _reg "Ord" tag
        case mMethods of
            Just methods | length methods > slot -> do
                let method = methods !! slot
                aT <- newWHNFThunk av
                bT <- newWHNFThunk bv
                r1 <- apply method aT
                apply r1 bT
            _ ->
                -- Fall back to Eq for <= and >=
                case slot of
                    1 -> do r <- eqVals _reg av bv
                            if isTruthy r then pure (boolVal True)
                            else ordCmp _reg 0 av bv
                    3 -> do r <- eqVals _reg av bv
                            if isTruthy r then pure (boolVal True)
                            else ordCmp _reg 2 av bv
                    _ -> error ("Ord: no instance for type tag `"
                                <> BC.unpack (typeTagOf av) <> "`")
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

-- | @compare@ returns an Ordering constructor: LT, EQ, or GT.
compareDispatch :: ClassRegistry -> IO Val
compareDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VCon (orderingName (compare x y)) [])
        (VFloat x, VFloat y) ->
            pure (VCon (orderingName (compare x y)) [])
        (VInt x, VFloat y) ->
            pure (VCon (orderingName (compare (fromIntegral x :: Double) y)) [])
        (VFloat x, VInt y) ->
            pure (VCon (orderingName (compare x (fromIntegral y :: Double))) [])
        (VChar x, VChar y) ->
            pure (VCon (orderingName (compare x y)) [])
        (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2)) ->
            pure (VCon (orderingName (compare p1 p2)) [])
        (VPrimObj (PrimForeignPtr fp1), VPrimObj (PrimForeignPtr fp2)) ->
            pure (VCon (orderingName (compare fp1 fp2)) [])
        _ -> do
            lt <- ordCmp reg 0 av bv
            if isTruthy lt then pure (VCon "LT" [])
            else do
                eq <- eqVals reg av bv
                if isTruthy eq then pure (VCon "EQ" [])
                else pure (VCon "GT" [])
  where
    orderingName LT = "LT"
    orderingName EQ = "EQ"
    orderingName GT = "GT"

-- | Show dispatch: look up "show" in the Show class registry.
-- Slot 0 = show. Falls back to built-in showVal for base types.
showDispatch :: ClassRegistry -> IO Val
showDispatch reg = pure $ VFun $ \a -> do
    av <- force a
    s  <- showValWith reg av
    stringToListValIO s

-- | Show a value, consulting the ClassRegistry for user-defined Show.
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
    VCon n _ | isTupleConName n -> showVal av
    VCon n _ -> do
        let tag = n
        mMethods <- lookupInstance reg "Show" tag
        case mMethods of
            Just (showMethod : _) -> do
                aT <- newWHNFThunk av
                rv <- apply showMethod aT
                valToString rv
            _ -> showVal av
    _ -> showVal av

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
-- | Show a Double in Haskell-compatible format. Whole numbers are shown
-- with a trailing ".0", e.g. @3.0@ not @3@.
showDouble :: Double -> String
showDouble d
    | isNaN d      = "NaN"
    | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
    | otherwise    =
        let s = show d
        in if '.' `elem` s || 'e' `elem` s then s else s <> ".0"

showVal :: Val -> IO String
showVal (VLabel name) = pure ("#" <> BC.unpack name)   -- Phase 3.5
showVal (VInt n)    = pure (show n)
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
    | isUnboxedTupleConName name = do
        parts <- mapM (\t -> do v <- force t; showVal v) thunks
        pure ("(#" <> intercalate "," parts <> "#)")
    | isTupleConName name = do
        parts <- mapM (\t -> do v <- force t; showVal v) thunks
        pure ("(" <> intercalate "," parts <> ")")
    | otherwise = do
        parts <- mapM (\t -> do v <- force t; showVal v) thunks
        case parts of
            [] -> pure (BC.unpack name)
            _  -> pure (BC.unpack name <> " " <> unwords parts)
showVal (VFun _)    = pure "<function>"
showVal (VFunIP _ _) = pure "<function>"
showVal (VIO _)     = pure "<IO>"
showVal (VPrimObj (PrimIORef  _))      = pure "<IORef>"
showVal (VPrimObj (PrimHandle _))      = pure "<Handle>"
showVal (VPrimObj (PrimForeignPtr _))  = pure "<ForeignPtr>"
showVal (VPrimObj (PrimPtr _))         = pure "<Ptr>"
showVal (VPrimObj (PrimByteArray _))   = pure "<MutableByteArray>"
showVal (VPrimObj PrimRealWorld)       = pure "<RealWorld#>"
showVal (VPrimObj (PrimMVar _))        = pure "<MVar>"
showVal (VPrimObj (PrimTVar _))        = pure "<TVar>"
showVal (VPrimObj (PrimThreadId tid))  = pure ("ThreadId " <> show tid)

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

-- Phase 3.5: OverloadedLabels ------------------------------------------------

-- | @fromLabel :: VLabel name -> Val@
--
-- In GHC, @fromLabel \@"name"@ selects an @IsLabel@ instance via type
-- inference. We have no type inference, so we dispatch at runtime:
--
-- * When the label is used in any context we don't yet recognise, we
--   return a @VCon "Proxy" [VLabel name]@ — the Proxy IsLabel instance.
--   Downstream code can pattern-match on this to recover the name.
-- * Field-accessor dispatch is deferred until the OverloadedRecordDot
--   agent lands its field-lookup primitive.
fromLabelB :: ClassRegistry -> IO Val
fromLabelB _reg = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VLabel name -> pure (VCon "Proxy" [])   -- Proxy IsLabel instance
        other -> error ("fromLabel: expected a Label value, got: "
                        <> showValForDebug other)

-- showB replaced by showDispatch in Phase 2.3

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

-- printB replaced by printDispatch in Phase 2.3

printDispatch :: ClassRegistry -> IO Val
printDispatch reg = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    s  <- showValWith reg av
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
    hPutStrLn stderr ("ihc error called: " <> s)
    hFlush stderr
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

-- | Class-dispatched @>>=@.  For 'VIO' values (the common IO case) this
-- is the same as the old 'bindB'.  For ST computations (VCon "ST" _), the
-- ST monad bind is implemented directly: build a new ST computation that
-- sequences the two.  For other values the 'Monad' instance in the class
-- registry is consulted.
bindDispatch :: ClassRegistry -> IO Val
bindDispatch reg = pure $ VFun $ \ma -> pure $ VFun $ \kt -> do
    mv <- force ma
    case mv of
        VIO _ -> pure $ VIO $ do
            v  <- runIOVal mv
            kv <- force kt
            vT <- newWHNFThunk v
            r  <- apply kv vT
            runIOVal r
        -- ST monad bind:
        -- (ST m) >>= k = ST (\s -> case m s of { (# s', r #) -> case k r of { ST k2 -> k2 s' }})
        -- Note: primop state functions may return VIO actions; run them with runIOVal.
        VCon "ST" [mFuncT] -> do
            mFuncV <- force mFuncT
            ktV    <- force kt
            stFunc <- newWHNFThunk $ VFun $ \sT -> do
                resRaw <- apply mFuncV sT    -- apply :: Val -> Thunk -> IO Val
                resV   <- runIOVal resRaw    -- run VIO if needed (primop results)
                case resV of
                    VCon "(#,#)" [newST, rT] -> do
                        stkRaw <- apply ktV rT   -- k applied to r; result is an ST
                        stkV   <- runIOVal stkRaw
                        case stkV of
                            VCon "ST" [k2FuncT] -> do
                                k2FuncV  <- force k2FuncT
                                resRaw2  <- apply k2FuncV newST
                                runIOVal resRaw2
                            other -> pure other  -- k returned a non-ST (shouldn't happen)
                    other -> pure other  -- m didn't return an unboxed tuple
            pure (VCon "ST" [stFunc])
        _ -> do
            -- Look up Monad instance for this value's type tag.
            let tag = typeTagOf mv
            mMethods <- lookupInstance reg "Monad" tag
            case mMethods of
                Just methods | not (null methods) -> do
                    -- Monad methods are stored in the order they appear in the
                    -- instance declaration.  We search by name.
                    let bindMethod = last methods   -- >>=  is typically the key method
                    maT <- newWHNFThunk mv
                    r1  <- apply bindMethod maT
                    apply r1 kt
                _ ->
                    -- No instance found; fall back to IO bind (will error at
                    -- runtime if mv is not VIO, but gives a sensible message).
                    pure $ VIO $ do
                        v  <- runIOVal mv
                        kv <- force kt
                        vT <- newWHNFThunk v
                        r  <- apply kv vT
                        runIOVal r

-- | @m >> n@ = run m (discarding result), then run n.
seqDispatch :: ClassRegistry -> IO Val
seqDispatch reg = pure $ VFun $ \ma -> pure $ VFun $ \mb -> do
    mv <- force ma
    case mv of
        VIO _ -> pure $ VIO $ do
            _ <- runIOVal mv
            nv <- force mb
            runIOVal nv
        -- ST monad seq:
        -- m >> n = m >>= \_ -> n  for ST
        -- Note: primop state functions may return VIO actions; run them with runIOVal.
        VCon "ST" [mFuncT] -> do
            mFuncV <- force mFuncT
            stFunc <- newWHNFThunk $ VFun $ \sT -> do
                resRaw <- apply mFuncV sT    -- apply :: Val -> Thunk -> IO Val
                resV   <- runIOVal resRaw    -- run VIO if needed
                case resV of
                    VCon "(#,#)" [newST, _] -> do
                        nbV <- force mb
                        case nbV of
                            VCon "ST" [k2FuncT] -> do
                                k2FuncV  <- force k2FuncT
                                resRaw2  <- apply k2FuncV newST
                                runIOVal resRaw2
                            other -> pure other
                    other -> pure other
            pure (VCon "ST" [stFunc])
        _ -> do
            let tag = typeTagOf mv
            mMethods <- lookupInstance reg "Monad" tag
            case mMethods of
                Just methods | length methods >= 2 -> do
                    let seqMethod = head methods   -- >>  is typically stored first
                    maT <- newWHNFThunk mv
                    r1  <- apply seqMethod maT
                    apply r1 mb
                _ ->
                    pure $ VIO $ do
                        _ <- runIOVal mv
                        nv <- force mb
                        runIOVal nv

-- | @m >>= k@. Low-level IO-only variant (kept for internal use).
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
    v  <- force initThunk
    rf <- newIORef v
    stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
    refT <- newWHNFThunk (VPrimObj (PrimIORef rf))
    pure (VCon "(#,#)" [stT, refT])

-- | @readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)@
-- Returns the unboxed tuple directly (no VIO wrapper).
readMutVarB :: IO Val
readMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \_st -> do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            v   <- readIORef rf
            stT <- newWHNFThunk (VPrimObj PrimRealWorld)
            vT  <- newWHNFThunk v
            pure (VCon "(#,#)" [stT, vT])
        _ -> error ("readMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @writeMutVar# :: MutVar# s a -> a -> State# s -> State# s@
-- Returns the new state token directly (no VIO wrapper).
writeMutVarB :: IO Val
writeMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \valThunk ->
               pure $ VFun $ \_st -> do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            v <- force valThunk
            writeIORef rf v
            pure (VPrimObj PrimRealWorld)
        _ -> error ("writeMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, b #)@
atomicModifyMutVarB :: IO Val
atomicModifyMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                      pure $ VFun $ \_st -> pure $ VIO $ do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force fThunk
            cur  <- readIORef rf
            curT <- newWHNFThunk cur
            -- f cur returns a (a, b) pair
            res  <- apply fv curT
            resV <- runIOVal res
            case resV of
                VCon _ [newT, bT] -> do
                    new <- force newT
                    b   <- force bT
                    writeIORef rf new
                    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
                    bT' <- newWHNFThunk b
                    pure (VCon "(#,#)" [stT, bT'])
                _ -> error ("atomicModifyMutVar#: f did not return a pair: "
                            <> showValForDebug resV)
        _ -> error ("atomicModifyMutVar#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar2# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, a, (a, b) #)@
atomicModifyMutVar2B :: IO Val
atomicModifyMutVar2B = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                       pure $ VFun $ \_st -> pure $ VIO $ do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force fThunk
            cur  <- readIORef rf
            curT <- newWHNFThunk cur
            res  <- apply fv curT
            resV <- runIOVal res
            case resV of
                VCon _ [newT, bT] -> do
                    new  <- force newT
                    b    <- force bT
                    writeIORef rf new
                    stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
                    curT' <- newWHNFThunk cur
                    newT' <- newWHNFThunk new
                    bT'   <- newWHNFThunk b
                    pairT <- newWHNFThunk (VCon "(,)" [newT', bT'])
                    pure (VCon "(#,,#)" [stT, curT', pairT])
                _ -> error ("atomicModifyMutVar2#: f did not return a pair: "
                            <> showValForDebug resV)
        _ -> error ("atomicModifyMutVar2#: not a MutVar#: " <> showValForDebug mvV)

-- | @atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)@
-- Returns @(# s, old, new #)@.
atomicModifyMutVarUB :: IO Val
atomicModifyMutVarUB = pure $ VFun $ \mvThunk -> pure $ VFun $ \fThunk ->
                       pure $ VFun $ \_st -> pure $ VIO $ do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            fv   <- force fThunk
            old  <- readIORef rf
            oldT <- newWHNFThunk old
            new  <- apply fv oldT
            writeIORef rf new
            stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
            oldT' <- newWHNFThunk old
            newT  <- newWHNFThunk new
            pure (VCon "(#,,#)" [stT, oldT', newT])
        _ -> error ("atomicModifyMutVar_#: not a MutVar#: " <> showValForDebug mvV)

-- | @casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)@
-- Non-atomic CAS — always succeeds (returns 0# = success).
casMutVarB :: IO Val
casMutVarB = pure $ VFun $ \mvThunk -> pure $ VFun $ \_expectedThunk ->
             pure $ VFun $ \newThunk -> pure $ VFun $ \_st -> pure $ VIO $ do
    mvV <- force mvThunk
    case mvV of
        VPrimObj (PrimIORef rf) -> do
            new <- force newThunk
            writeIORef rf new
            -- Return (# s, 0#, new #) — 0# means success
            stT  <- newWHNFThunk (VPrimObj PrimRealWorld)
            zT   <- newWHNFThunk (VInt 0)
            newT <- newWHNFThunk new
            pure (VCon "(#,,#)" [stT, zT, newT])
        _ -> error ("casMutVar#: not a MutVar#: " <> showValForDebug mvV)

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

-- | @assert cond x = x@.  Like GHC's default (assertions ignored) behaviour.
assertB :: IO Val
assertB = pure $ VFun $ \_cond -> pure $ VFun $ \x -> force x

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

-- | 'fromIntegral' / 'fromInteger' coercion. Accepts Int or Float/Double;
-- returns the value unchanged (we have one Int type and one Float type).
fromIntegralB :: IO Val
fromIntegralB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n   -> pure (VInt n)
        VFloat d -> pure (VFloat d)
        VChar c  -> pure (VInt (fromIntegral (ord c)))
        _ -> error ("fromIntegral: not a numeric value: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: RealWorld / State primops
--------------------------------------------------------------------------------

realWorldB :: IO Val
realWorldB = pure (VPrimObj PrimRealWorld)

-- | noDuplicate# :: State# s -> State# s
-- No-op in the interpreter; in GHC RTS this prevents thunk duplication.
noDuplicateB :: IO Val
noDuplicateB = pure $ VFun $ \_ -> pure (VPrimObj PrimRealWorld)

-- | runRW# :: (State# RealWorld -> (# State# RealWorld, a #)) -> (# State# RealWorld, a #)
-- In our interpreter: apply the function to the RealWorld token, run any
-- resulting VIO action, and return the (# State# RealWorld, a #) unboxed tuple.
-- Callers (like runST, unsafePerformIO) are expected to extract the second
-- element themselves via case pattern matching.
-- NOTE: previous versions extracted the result here, but that broke source-loaded
-- callers like GHC.ST.runST that pattern-match on the returned tuple themselves.
runRWB :: IO Val
runRWB = pure $ VFun $ \ft -> do
    fv <- force ft
    rwT <- newWHNFThunk (VPrimObj PrimRealWorld)
    resRaw <- apply fv rwT
    result <- runIOVal resRaw
    pure result

lazyB :: IO Val
lazyB = pure $ VFun $ \a -> force a

--------------------------------------------------------------------------------
-- Phase 2.8: unsafePerformIO family
--------------------------------------------------------------------------------

unsafePerformIOB :: IO Val
unsafePerformIOB = pure $ VFun $ \a -> do
    av <- force a
    runIOVal av

--------------------------------------------------------------------------------
-- Phase 2.8: boxing/unboxing constructors
--------------------------------------------------------------------------------

iHashB :: IO Val
iHashB = pure $ VFun $ \a -> force a

wHashB :: IO Val
wHashB = pure $ VFun $ \a -> force a

w8HashB :: IO Val
w8HashB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt (n .&. 0xff))
        _      -> force a

cHashB :: IO Val
cHashB = pure $ VFun $ \a -> force a

--------------------------------------------------------------------------------
-- Phase 2.8: Addr# primitives
--------------------------------------------------------------------------------

nullAddrB :: IO Val
nullAddrB = pure (VPrimObj (PrimPtr nullPtr))

plusAddrB :: IO Val
plusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VInt n) ->
            pure (VPrimObj (PrimPtr (plusPtr p (fromIntegral n))))
        _ -> error ("plusAddr#: bad args: " <> showValForDebug av)

minusAddrB :: IO Val
minusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VPrimObj (PrimPtr q)) ->
            pure (VInt (fromIntegral (p `minusPtr` q)))
        _ -> error ("minusAddr#: bad args: " <> showValForDebug av)

addr2IntB :: IO Val
addr2IntB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VPrimObj (PrimPtr p) ->
            pure (VInt (fromIntegral (FP.ptrToIntPtr p)))
        _ -> error ("addr2Int#: not a Ptr: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: Ptr arithmetic
--------------------------------------------------------------------------------

plusPtrB :: IO Val
plusPtrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VInt n) ->
            pure (VPrimObj (PrimPtr (plusPtr p (fromIntegral n))))
        (VPrimObj (PrimForeignPtr _), VInt _) -> pure av
        _ -> error ("plusPtr: bad args: " <> showValForDebug av)

minusPtrB :: IO Val
minusPtrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VPrimObj (PrimPtr q)) ->
            pure (VInt (fromIntegral (p `minusPtr` q)))
        _ -> error ("minusPtr: bad args: " <> showValForDebug av)

nullPtrB :: IO Val
nullPtrB = pure (VPrimObj (PrimPtr nullPtr))

castPtrB :: IO Val
castPtrB = pure $ VFun $ \a -> force a

--------------------------------------------------------------------------------
-- Phase 2.8: ForeignPtr
--------------------------------------------------------------------------------

mallocForeignPtrBytesB :: IO Val
mallocForeignPtrBytesB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    case av of
        VInt n -> do
            fp <- mallocForeignPtrBytes (fromIntegral n)
            mkForeignPtrVal fp
        _ -> error ("mallocForeignPtrBytes: not an Int: " <> showValForDebug av)

withForeignPtrB :: IO Val
withForeignPtrB = pure $ VFun $ \fpT -> pure $ VFun $ \fT -> pure $ VIO $ do
    fpv <- force fpT; fv <- force fT
    fp <- foreignPtrValToForeignPtr fpv
    withForeignPtr fp $ \ptr -> do
        pT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr ptr)))
        rv <- apply fv pT
        runIOVal rv

plusForeignPtrB :: IO Val
plusForeignPtrB = pure $ VFun $ \fpT -> pure $ VFun $ \nT -> do
    fpv <- force fpT; nv <- force nT
    case (fpv, nv) of
        -- ForeignPtr is RTS-backed and has no pure-Haskell storage model in IHC,
        -- so this builtin must preserve the host pointer offset exactly.
        (_, VInt n) -> do
            fp <- foreignPtrValToForeignPtr fpv
            mkForeignPtrVal (plusForeignPtr fp (fromIntegral n))
        _ -> error ("plusForeignPtr: bad args: " <> showValForDebug fpv)

touchForeignPtrB :: IO Val
touchForeignPtrB = pure $ VFun $ \fpT -> pure $ VIO $ do
    fpv <- force fpT
    fp <- foreignPtrValToForeignPtr fpv
    touchForeignPtr fp
    pure VUnit

newForeignPtr_B :: IO Val
newForeignPtr_B = pure $ VFun $ \pT -> pure $ VIO $ do
    pv <- force pT
    p <- ptrValToPtr pv
    fp <- newForeignPtr_ (castPtr p)
    mkForeignPtrVal fp

--------------------------------------------------------------------------------
-- Phase 2.8: Storable ops on Ptr
--------------------------------------------------------------------------------

peekB :: IO Val
peekB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    p <- ptrValToPtr av
    w <- peek (p :: Ptr Word8)
    pure (VInt (fromIntegral w))

pokeB :: IO Val
pokeB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force a; bv <- force b
    p <- ptrValToPtr av
    case bv of
        VInt n -> do { poke (p :: Ptr Word8) (fromIntegral n); pure VUnit }
        _ -> error ("poke: value not an Int: " <> showValForDebug bv)

peekByteOffB :: IO Val
peekByteOffB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force a; bv <- force b
    p <- ptrValToPtr av
    case bv of
        VInt off -> do
            w <- peekByteOff (p :: Ptr Word8) (fromIntegral off)
            pure (VInt (fromIntegral (w :: Word8)))
        _ -> error ("peekByteOff: bad args: " <> showValForDebug av)

pokeByteOffB :: IO Val
pokeByteOffB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    av <- force a; bv <- force b; cv <- force c
    p <- ptrValToPtr av
    case bv of
        VInt off ->
            case cv of
                VInt n -> do
                    pokeByteOff (p :: Ptr Word8) (fromIntegral off) (fromIntegral n :: Word8)
                    pure VUnit
                _ -> error ("pokeByteOff: value not an Int: " <> showValForDebug cv)
        _ -> error ("pokeByteOff: bad args: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: MutableByteArray# family (backed by IORef ByteString)
--------------------------------------------------------------------------------

newByteArrayB :: IO Val
newByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force a; stv <- force stT
    let n = case av of { VInt i -> fromIntegral i; _ -> 0 }
    ref  <- newIORef (BS.replicate n 0)
    baT  <- newWHNFThunk (VPrimObj (PrimByteArray ref))
    stT' <- newWHNFThunk stv
    pure (VCon "(#,#)" [stT', baT])

writeWord8ArrayB :: IO Val
writeWord8ArrayB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force a; bv <- force b; cv <- force c; stv <- force stT
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
                    stT' <- newWHNFThunk stv
                    pure (VCon "(#,#)" [stT', stT'])
                _ -> error "writeWord8Array#: bad index/val"
        _ -> error ("writeWord8Array#: not a MutableByteArray: " <> showValForDebug av)

readWord8ArrayB :: IO Val
readWord8ArrayB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force a; bv <- force b; stv <- force stT
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
    av <- force a; bv <- force b
    case av of
        VPrimObj (PrimByteArray ref) ->
            case bv of
                VInt idx -> do
                    bs <- readIORef ref
                    pure (VInt (fromIntegral (BS.index bs (fromIntegral idx))))
                _ -> error "indexWord8Array#: bad index"
        _ -> error ("indexWord8Array#: not a MutableByteArray: " <> showValForDebug av)

unsafeFreezeByteArrayB :: IO Val
unsafeFreezeByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force a; stv <- force stT
    case av of
        VPrimObj (PrimByteArray _) -> do
            aT   <- newWHNFThunk av
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', aT])
        _ -> error ("unsafeFreezeByteArray#: not a MutableByteArray: " <> showValForDebug av)

getSizeofMutableByteArrayB :: IO Val
getSizeofMutableByteArrayB = pure $ VFun $ \a -> pure $ VFun $ \stT -> pure $ VIO $ do
    av <- force a; stv <- force stT
    case av of
        VPrimObj (PrimByteArray ref) -> do
            bs   <- readIORef ref
            nT   <- newWHNFThunk (VInt (fromIntegral (BS.length bs)))
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', nT])
        _ -> error ("getSizeofMutableByteArray#: not a MutableByteArray: " <> showValForDebug av)

sizeofByteArrayB :: IO Val
sizeofByteArrayB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VPrimObj (PrimByteArray ref) -> do
            bs <- readIORef ref
            pure (VInt (fromIntegral (BS.length bs)))
        _ -> error ("sizeofByteArray#: not a ByteArray: " <> showValForDebug av)

copyAddrToByteArrayB :: IO Val
copyAddrToByteArrayB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \stT -> pure $ VIO $ do
    srcV    <- force a; baV <- force b
    dstOffV <- force c; lenV <- force d; stv <- force stT
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
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', stT'])
        _ -> error "copyAddrToByteArray#: bad args"

copyByteArrayToAddrB :: IO Val
copyByteArrayToAddrB = pure
    $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VFun $ \d -> pure $ VFun $ \stT -> pure $ VIO $ do
    baV     <- force a; srcOffV <- force b
    dstV    <- force c; lenV    <- force d; stv <- force stT
    case (baV, srcOffV, dstV, lenV) of
        (VPrimObj (PrimByteArray ref), VInt srcOff, VPrimObj (PrimPtr dst), VInt len) -> do
            bs <- readIORef ref
            let chunk = BS.take (fromIntegral len) (BS.drop (fromIntegral srcOff) bs)
            BS.useAsCStringLen chunk $ \(src, _n) ->
                copyBytes (castPtr dst) (castPtr src :: Ptr Word8) (fromIntegral len)
            stT' <- newWHNFThunk stv
            pure (VCon "(#,#)" [stT', stT'])
        _ -> error "copyByteArrayToAddr#: bad args"

--------------------------------------------------------------------------------
-- Phase 2.8: C memory ops
--------------------------------------------------------------------------------

memcpyB :: IO Val
memcpyB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    dstV <- force a; srcV <- force b; lenV <- force c
    case (dstV, srcV, lenV) of
        (VPrimObj (PrimPtr dst), VPrimObj (PrimPtr src), VInt n) -> do
            copyBytes dst src (fromIntegral n)
            pure VUnit
        _ -> error "memcpy: bad args"

memcpyFpB :: IO Val
memcpyFpB = pure $ VFun $ \fpT -> pure $ VFun $ \pT -> pure $ VFun $ \nT -> pure $ VIO $ do
    fpv <- force fpT; pv <- force pT; nv <- force nT
    case (fpv, pv, nv) of
        (VPrimObj (PrimForeignPtr fp), VPrimObj (PrimPtr src), VInt n) ->
            withForeignPtr fp $ \dst -> do
                copyBytes (castPtr dst) src (fromIntegral n)
                pure VUnit
        _ -> error "memcpyFp: bad args"

-- | copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
-- Foreign.Marshal.Utils.copyBytes — wraps copyAddrToAddrNonOverlapping# primop.
copyBytesB :: IO Val
copyBytesB = pure $ VFun $ \destT -> pure $ VFun $ \srcT -> pure $ VFun $ \nT -> pure $ VIO $ do
    dv <- force destT; sv <- force srcT; nv <- force nT
    case (dv, sv, nv) of
        (VPrimObj (PrimPtr dest), VPrimObj (PrimPtr src), VInt n) -> do
            copyBytes dest src (fromIntegral n)
            pure VUnit
        _ -> error $ "copyBytes: bad args"

memsetB :: IO Val
memsetB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    pv <- force a; vv <- force b; nv <- force c
    case (pv, vv, nv) of
        (VPrimObj (PrimPtr p), VInt v, VInt n) -> do
            fillBytes p (fromIntegral v) (fromIntegral n)
            pure VUnit
        _ -> error "memset: bad args"

memchrB :: IO Val
memchrB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    pv <- force a; vv <- force b; nv <- force c
    case (pv, vv, nv) of
        (VPrimObj (PrimPtr p), VInt v, VInt n) -> do
            let go i
                  | i >= fromIntegral n = pure (VPrimObj (PrimPtr nullPtr))
                  | otherwise = do
                      w <- peek (plusPtr p i :: Ptr Word8)
                      if fromIntegral w == v
                          then pure (VPrimObj (PrimPtr (plusPtr p i)))
                          else go (i + 1)
            go (0 :: Int)
        _ -> error "memchr: bad args"

memcmpB :: IO Val
memcmpB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    p1v <- force a; p2v <- force b; nv <- force c
    case (p1v, p2v, nv) of
        (VPrimObj (PrimPtr p1), VPrimObj (PrimPtr p2), VInt n) -> do
            let go i
                  | i >= fromIntegral n = pure (VInt 0)
                  | otherwise = do
                      w1 <- peek (plusPtr p1 i :: Ptr Word8)
                      w2 <- peek (plusPtr p2 i :: Ptr Word8)
                      case compare w1 w2 of
                          LT -> pure (VInt (-1))
                          GT -> pure (VInt 1)
                          EQ -> go (i + 1)
            go (0 :: Int)
        _ -> error "memcmp: bad args"

cStrlenB :: IO Val
cStrlenB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force a
    case av of
        VPrimObj (PrimPtr p) -> do
            let go i = do
                  w <- peek (plusPtr p i :: Ptr Word8)
                  if w == 0 then pure (VInt (fromIntegral i)) else go (i + 1)
            go (0 :: Int)
        _ -> error ("c_strlen: not a Ptr: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: buffered I/O
--------------------------------------------------------------------------------

hPutBufB :: IO Val
hPutBufB = pure $ VFun $ \hT -> pure $ VFun $ \pT -> pure $ VFun $ \nT -> pure $ VIO $ do
    hv <- force hT; pv <- force pT; nv <- force nT
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
    av <- force a
    case av of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word64)))
        _      -> force a

word2IntB :: IO Val
word2IntB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt (fromIntegral (fromIntegral n :: Word64)))
        _      -> force a

orHashB :: IO Val
orHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .|. y))
        _ -> error ("or#: bad args: " <> showValForDebug av)

andHashB :: IO Val
andHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .&. y))
        _ -> error ("and#: bad args: " <> showValForDebug av)

xorHashB :: IO Val
xorHashB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `xor` y))
        _ -> error ("xor#: bad args: " <> showValForDebug av)

notHashB :: IO Val
notHashB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt x -> pure (VInt (complement x))
        _      -> error ("not#: bad arg: " <> showValForDebug av)

uncheckedShiftLB :: IO Val
uncheckedShiftLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftL` fromIntegral n))
        _ -> error ("uncheckedShiftL#: bad args: " <> showValForDebug av)

uncheckedShiftRLB :: IO Val
uncheckedShiftRLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) ->
            pure (VInt (fromIntegral (fromIntegral x `shiftR` fromIntegral n :: Word64)))
        _ -> error ("uncheckedShiftRL#: bad args: " <> showValForDebug av)

timesInt2B :: IO Val
timesInt2B = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let r   = x * y
                ovf = if x /= 0 && r `div` x /= y then 1 else 0 :: Int64
            carryT  <- newWHNFThunk (VInt ovf)
            resultT <- newWHNFThunk (VInt r)
            pure (VCon "(#,#)" [carryT, resultT])
        _ -> error ("timesInt2#: bad args: " <> showValForDebug av)

timesWord2B :: IO Val
timesWord2B = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let r = x * y
            hiT <- newWHNFThunk (VInt 0)
            loT <- newWHNFThunk (VInt r)
            pure (VCon "(#,#)" [hiT, loT])
        _ -> error ("timesWord2#: bad args: " <> showValForDebug av)

--------------------------------------------------------------------------------
-- Phase 2.8: GHC.Exts Word# comparison + arithmetic primops
--------------------------------------------------------------------------------

ltWordB :: IO Val
ltWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (boolVal ((fromIntegral x :: Word64) < fromIntegral y))
        _ -> error "ltWord#: bad args"

leWordB :: IO Val
leWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (boolVal ((fromIntegral x :: Word64) <= fromIntegral y))
        _ -> error "leWord#: bad args"

eqWordB :: IO Val
eqWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (boolVal (x == y))
        _ -> error "eqWord#: bad args"

gtWordB :: IO Val
gtWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (boolVal ((fromIntegral x :: Word64) > fromIntegral y))
        _ -> error "gtWord#: bad args"

geWordB :: IO Val
geWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (boolVal ((fromIntegral x :: Word64) >= fromIntegral y))
        _ -> error "geWord#: bad args"

minusWordB :: IO Val
minusWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral (fromIntegral x - fromIntegral y :: Word64)))
        _ -> error "minusWord#: bad args"

plusWordB :: IO Val
plusWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral (fromIntegral x + fromIntegral y :: Word64)))
        _ -> error "plusWord#: bad args"

timesWordB :: IO Val
timesWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral (fromIntegral x * fromIntegral y :: Word64)))
        _ -> error "timesWord#: bad args"

quotWordB :: IO Val
quotWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral ((fromIntegral x :: Word64) `quot` fromIntegral y)))
        _ -> error "quotWord#: bad args"

remWordB :: IO Val
remWordB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) ->
            pure (VInt (fromIntegral ((fromIntegral x :: Word64) `rem` fromIntegral y)))
        _ -> error "remWord#: bad args"

popCntB :: IO Val
popCntB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt (fromIntegral (popCount (fromIntegral n :: Word64))))
        _      -> error ("popCnt#: bad arg: " <> showValForDebug av)

indexOfTheOnlyBitB :: IO Val
indexOfTheOnlyBitB = pure $ VFun $ \a -> do
    av <- force a
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
    av <- force a
    case av of
        VInt n -> pure (VInt (negate n))
        _      -> error ("negateInt#: bad arg: " <> showValForDebug av)

quotIntB :: IO Val
quotIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `quot` y))
        _ -> error "quotInt#: bad args"

remIntB :: IO Val
remIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `rem` y))
        _ -> error "remInt#: bad args"

quotRemIntB :: IO Val
quotRemIntB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let (q, r) = x `quotRem` y
            qT <- newWHNFThunk (VInt q)
            rT <- newWHNFThunk (VInt r)
            pure (VCon "(#,#)" [qT, rT])
        _ -> error "quotRemInt#: bad args"

addIntCB :: IO Val
addIntCB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            rT <- newWHNFThunk (VInt (x + y))
            cT <- newWHNFThunk (VInt 0)
            pure (VCon "(#,#)" [rT, cT])
        _ -> error "addIntC#: bad args"

subIntCB :: IO Val
subIntCB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            rT <- newWHNFThunk (VInt (x - y))
            cT <- newWHNFThunk (VInt 0)
            pure (VCon "(#,#)" [rT, cT])
        _ -> error "subIntC#: bad args"

mulIntMayOfloB :: IO Val
mulIntMayOfloB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt _, VInt _) -> pure (VInt 0)
        _ -> error "mulIntMayOflo#: bad args"

--------------------------------------------------------------------------------
-- Phase 2.8: misc primops
--------------------------------------------------------------------------------

cstringLengthB :: IO Val
cstringLengthB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VPrimObj (PrimPtr p) -> do
            let go i = do
                  w <- peek (plusPtr p i :: Ptr Word8)
                  if w == 0 then pure (VInt (fromIntegral i)) else go (i + 1)
            go (0 :: Int)
        VStr s -> pure (VInt (fromIntegral (BC.length s)))
        _ -> error ("cstringLength#: bad arg: " <> showValForDebug av)

unpackCStringB :: IO Val
unpackCStringB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VPrimObj (PrimPtr p) -> do
            s <- peekCAString (castPtr p)
            stringToListValIO s
        VStr s -> stringToListValIO (BC.unpack s)
        _ -> error ("unpackCString#: bad arg: " <> showValForDebug av)

sizeOfB :: IO Val
sizeOfB = pure $ VFun $ \a -> do
    _av <- force a
    pure (VInt (fromIntegral (sizeOf (undefined :: Word8))))

alignmentB :: IO Val
alignmentB = pure $ VFun $ \a -> do
    _av <- force a
    pure (VInt 1)

--------------------------------------------------------------------------------
-- Phase 2.8: additional numeric / bit ops
--------------------------------------------------------------------------------

divModB :: IO Val
divModB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let (d, m) = x `divMod` y
            dT <- newWHNFThunk (VInt d); mT <- newWHNFThunk (VInt m)
            pure (VCon "(,)" [dT, mT])
        _ -> error "divMod: bad args"

quotRemB :: IO Val
quotRemB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let (q, r) = x `quotRem` y
            qT <- newWHNFThunk (VInt q); rT <- newWHNFThunk (VInt r)
            pure (VCon "(,)" [qT, rT])
        _ -> error "quotRem: bad args"

shiftLB :: IO Val
shiftLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftL` fromIntegral n))
        _ -> error "shiftL: bad args"

shiftRB :: IO Val
shiftRB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftR` fromIntegral n))
        _ -> error "shiftR: bad args"

bitAndB :: IO Val
bitAndB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .&. y))
        _ -> error "(.&.): bad args"

bitOrB :: IO Val
bitOrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x .|. y))
        _ -> error "(.|.): bad args"

bitXorB :: IO Val
bitXorB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (x `xor` y))
        _ -> error "xor: bad args"

bitComplementB :: IO Val
bitComplementB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt x -> pure (VInt (complement x))
        _ -> error "complement: bad arg"

popCountB :: IO Val
popCountB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt (fromIntegral (popCount (fromIntegral n :: Word64))))
        _ -> error "popCount: bad arg"

bitB :: IO Val
bitB = pure $ VFun $ \a -> do
    av <- force a
    case av of
        VInt n -> pure (VInt (1 `shiftL` fromIntegral n))
        _ -> error "bit: bad arg"

testBitB :: IO Val
testBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (boolVal ((x `shiftR` fromIntegral n) .&. 1 /= 0))
        _ -> error "testBit: bad args"

clearBitB :: IO Val
clearBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x .&. complement (1 `shiftL` fromIntegral n)))
        _ -> error "clearBit: bad args"

setBitB :: IO Val
setBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a; bv <- force b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x .|. (1 `shiftL` fromIntegral n)))
        _ -> error "setBit: bad args"

--------------------------------------------------------------------------------
-- Power operator
--------------------------------------------------------------------------------

-- | @(^) :: Num a => a -> Int -> a@ — right-associative, precedence 8.
-- Int ^ Int → Int via repeated multiplication (handles 0^0 = 1).
-- Double ^ Int → Double via Haskell's (^^).
powOpB :: IO Val
powOpB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    case (av, bv) of
        (VInt x, VInt n)
            | n < 0    -> error ("(^): negative exponent: " <> show n)
            | otherwise -> pure (VInt (intPow x n))
        (VFloat x, VInt n) -> pure (VFloat (x ^^ n))
        (VInt x, VFloat _) -> error "(^): exponent must be Int"
        _ -> error ("(^): non-numeric args: "
                    <> showValForDebug av <> " ^ " <> showValForDebug bv)
  where
    intPow :: Int64 -> Int64 -> Int64
    intPow _ 0 = 1
    intPow x n | odd n    = x * intPow x (n - 1)
               | otherwise = let h = intPow x (n `div` 2) in h * h

-- | @(^^) :: Fractional a => a -> Int -> a@  and  @(**) :: Floating a => a -> a -> a@.
powFloatOpB :: IO Val
powFloatOpB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force a
    bv <- force b
    let toD (VFloat d) = d
        toD (VInt n)   = fromIntegral n
        toD v          = error ("(^^)/(** ): non-numeric: " <> showValForDebug v)
    pure (VFloat (toD av ** toD bv))

--------------------------------------------------------------------------------
-- Arithmetic sequences: enumFromTo, enumFromThenTo
--------------------------------------------------------------------------------

-- | @enumFromTo lo hi@ builds @[lo..hi]@. Works for Int and Char.
enumFromToB :: IO Val
enumFromToB = pure $ VFun $ \loT -> pure $ VFun $ \hiT -> do
    lov <- force loT
    hiv <- force hiT
    case (lov, hiv) of
        (VInt lo, VInt hi) -> buildIntList lo hi
        (VChar lo, VChar hi) ->
            buildIntList (fromIntegral (fromEnum lo)) (fromIntegral (fromEnum hi))
            >>= charifyList
        _ -> error ("enumFromTo: non-Int/Char args: "
                    <> showValForDebug lov <> " " <> showValForDebug hiv)
  where
    buildIntList :: Int64 -> Int64 -> IO Val
    buildIntList lo hi
        | lo > hi   = pure (VCon "[]" [])
        | otherwise = do
            hT   <- newWHNFThunk (VInt lo)
            rest <- buildIntList (lo + 1) hi
            tT   <- newWHNFThunk rest
            pure (VCon ":" [hT, tT])
    charifyList :: Val -> IO Val
    charifyList (VCon "[]" _) = pure (VCon "[]" [])
    charifyList (VCon ":" [hT, tT]) = do
        hv <- force hT
        case hv of
            VInt n -> do
                cT   <- newWHNFThunk (VChar (toEnum (fromIntegral n)))
                rest <- force tT >>= charifyList
                rT   <- newWHNFThunk rest
                pure (VCon ":" [cT, rT])
            _ -> error "charifyList: expected VInt"
    charifyList v = error ("charifyList: bad list: " <> showValForDebug v)

-- | @enumFromThenTo lo step hi@ builds @[lo, step..hi]@. Works for Int and Char.
enumFromThenToB :: IO Val
enumFromThenToB = pure $ VFun $ \loT -> pure $ VFun $ \stepT -> pure $ VFun $ \hiT -> do
    lov  <- force loT
    stepv <- force stepT
    hiv  <- force hiT
    case (lov, stepv, hiv) of
        (VInt lo, VInt step, VInt hi) -> buildStepList lo step hi
        _ -> error ("enumFromThenTo: non-Int args")
  where
    buildStepList :: Int64 -> Int64 -> Int64 -> IO Val
    buildStepList lo step hi
        | step > 0 && lo > hi = pure (VCon "[]" [])
        | step < 0 && lo < hi = pure (VCon "[]" [])
        | step == 0            = pure (VCon "[]" [])  -- avoid infinite loop
        | otherwise = do
            hT   <- newWHNFThunk (VInt lo)
            rest <- buildStepList (lo + step) step hi
            tT   <- newWHNFThunk rest
            pure (VCon ":" [hT, tT])

--------------------------------------------------------------------------------
-- Simple file IO: readFile, writeFile, appendFile
--------------------------------------------------------------------------------

-- | @readFile path@ — read the entire file as a String ([Char]).
readFileB :: IO Val
readFileB = pure $ VFun $ \a -> pure $ VIO $ do
    pv   <- force a
    path <- valToString pv
    contents <- readFile path
    stringToListValIO contents

-- | @writeFile path contents@ — write a String to the file (truncating).
writeFileB :: IO Val
writeFileB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    pv   <- force a
    path <- valToString pv
    cv   <- force b
    s    <- valToString cv
    writeFile path s
    pure VUnit

-- | @appendFile path contents@ — append a String to the file.
appendFileB :: IO Val
appendFileB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    pv   <- force a
    path <- valToString pv
    cv   <- force b
    s    <- valToString cv
    appendFile path s
    pure VUnit

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

-- | Build an environment binding each record-field name to an accessor
-- function.  For a field @f@ that lives at index @i@ in constructor @Con@,
-- the accessor is equivalent to:
--
-- > f (Con _ _ ... x _ ...) = x  -- where x is at position i
--
-- If a field name appears in multiple constructors (unusual but legal) we
-- generate a single function that tries each one in order and falls back
-- to an @error@ on mismatch.
--
-- The accessor is a plain @VFun@ so it participates in lazy evaluation.
buildFieldEnv :: FieldRegistry -> IO Env
buildFieldEnv reg = do
    pairs <- mapM mkAccessor (Map.toList reg)
    pure (Map.fromList pairs)
  where
    mkAccessor (fieldName, clauses) = do
        t <- newWHNFThunk (VFun (access fieldName clauses))
        pure (fieldName, t)

    -- Build a function that, given a VCon, extracts the right field.
    access fieldName clauses argThunk = do
        v <- force argThunk
        case v of
            VCon conName args ->
                case lookup conName clauses of
                    Just idx | idx < length args ->
                        force (args !! idx)
                    Just idx ->
                        throwIO (userError
                            ("record accessor `" <> BC.unpack fieldName
                             <> "`: constructor `" <> BC.unpack conName
                             <> "` has only " <> show (length args)
                             <> " fields, index " <> show idx
                             <> " out of range"))
                    Nothing ->
                        throwIO (userError
                            ("record accessor `" <> BC.unpack fieldName
                             <> "`: constructor `" <> BC.unpack conName
                             <> "` has no such field"))
            _ ->
                throwIO (userError
                    ("record accessor `" <> BC.unpack fieldName
                     <> "` applied to non-constructor value"))

--------------------------------------------------------------------------------
-- Phase 2.10a: concurrency - thread primitives
--------------------------------------------------------------------------------

-- | @forkIO action@ - fork a new thread running the IO action.
forkIOB :: IO Val
forkIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    tid <- forkIO $ do
        _ <- runIOVal av
        pure ()
    pure (VPrimObj (PrimThreadId tid))

-- | @killThread tid@ - asynchronously raise 'ThreadKilled' in the thread.
killThreadB :: IO Val
killThreadB = pure $ VFun $ \tidT -> pure $ VIO $ do
    tidV <- force tidT
    case tidV of
        VPrimObj (PrimThreadId tid) -> do
            killThread tid
            pure VUnit
        _ -> error ("killThread: not a ThreadId: " <> showValForDebug tidV)

-- | @myThreadId@ - return the current thread's id.
myThreadIdB :: IO Val
myThreadIdB = pure $ VIO $ do
    tid <- myThreadId
    pure (VPrimObj (PrimThreadId tid))

-- | @threadDelay microseconds@ - sleep.
threadDelayB :: IO Val
threadDelayB = pure $ VFun $ \nT -> pure $ VIO $ do
    nv <- force nT
    case nv of
        VInt n -> do { threadDelay (fromIntegral n); pure VUnit }
        _ -> error ("threadDelay: not an Int: " <> showValForDebug nv)

-- | @getNumCapabilities@ - return 1 (simplified).
getNumCapabilitiesB :: IO Val
getNumCapabilitiesB = pure $ VIO $ pure (VInt 1)

--------------------------------------------------------------------------------
-- Phase 2.10a: MVar primitives
--------------------------------------------------------------------------------

requireMVar :: String -> Val -> IO (MVar Val)
requireMVar fn v = case v of
    VPrimObj (PrimMVar mv) -> pure mv
    _ -> error (fn <> ": not an MVar: " <> showValForDebug v)

newMVarB :: IO Val
newMVarB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    mv <- newMVar av
    pure (VPrimObj (PrimMVar mv))

newEmptyMVarB :: IO Val
newEmptyMVarB = pure $ VIO $ do
    mv <- newEmptyMVar
    pure (VPrimObj (PrimMVar mv))

takeMVarB :: IO Val
takeMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "takeMVar" mvv
    takeMVar mv

putMVarB :: IO Val
putMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "putMVar" mvv
    av  <- force aT
    putMVar mv av
    pure VUnit

readMVarB :: IO Val
readMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "readMVar" mvv
    readMVar mv

modifyMVar_B :: IO Val
modifyMVar_B = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "modifyMVar_" mvv
    fv  <- force fT
    modifyMVar_ mv $ \cur -> do
        curT <- newWHNFThunk cur
        rv   <- apply fv curT
        runIOVal rv
    pure VUnit

modifyMVarB :: IO Val
modifyMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "modifyMVar" mvv
    fv  <- force fT
    modifyMVar mv $ \cur -> do
        curT  <- newWHNFThunk cur
        rv    <- apply fv curT
        pairV <- runIOVal rv
        case pairV of
            VCon _ [newT, extraT] -> do
                newV   <- force newT
                extraV <- force extraT
                pure (newV, extraV)
            _ -> error ("modifyMVar: f did not return a pair: "
                        <> showValForDebug pairV)

tryTakeMVarB :: IO Val
tryTakeMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "tryTakeMVar" mvv
    r   <- tryTakeMVar mv
    case r of
        Nothing -> pure (VCon "Nothing" [])
        Just v  -> do { t <- newWHNFThunk v; pure (VCon "Just" [t]) }

tryPutMVarB :: IO Val
tryPutMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "tryPutMVar" mvv
    av  <- force aT
    ok  <- tryPutMVar mv av
    pure (boolVal ok)

isEmptyMVarB :: IO Val
isEmptyMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "isEmptyMVar" mvv
    b   <- isEmptyMVar mv
    pure (boolVal b)

withMVarB :: IO Val
withMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "withMVar" mvv
    fv  <- force fT
    withMVar mv $ \cur -> do
        curT <- newWHNFThunk cur
        rv   <- apply fv curT
        runIOVal rv

swapMVarB :: IO Val
swapMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force mvT
    mv  <- requireMVar "swapMVar" mvv
    av  <- force aT
    swapMVar mv av

--------------------------------------------------------------------------------
-- Phase 2.10a: STM primitives
--------------------------------------------------------------------------------

atomicallyB :: IO Val
atomicallyB = pure $ VFun $ \stmT -> pure $ VIO $ do
    stmV <- force stmT
    runIOVal stmV

retryB :: IO Val
retryB = pure $ VIO $ atomically retry

orElseB :: IO Val
orElseB = pure $ VFun $ \aT -> pure $ VFun $ \bT -> pure $ VIO $ do
    av <- force aT
    bv <- force bT
    -- Approximate: try av first, fall back to bv on exception
    CE.catch (runIOVal av) (\(_ :: CE.SomeException) -> runIOVal bv)

checkB :: IO Val
checkB = pure $ VFun $ \bT -> pure $ VIO $ do
    bv <- force bT
    atomically (check (isTruthy bv))
    pure VUnit

newTVarB :: IO Val
newTVarB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    tv <- newTVarIO av
    pure (VPrimObj (PrimTVar tv))

newTVarIOB :: IO Val
newTVarIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    tv <- newTVarIO av
    pure (VPrimObj (PrimTVar tv))

readTVarB :: IO Val
readTVarB = pure $ VFun $ \tvT -> pure $ VIO $ do
    tvv <- force tvT
    case tvv of
        VPrimObj (PrimTVar tv) -> atomically (readTVar tv)
        _ -> error ("readTVar: not a TVar: " <> showValForDebug tvv)

writeTVarB :: IO Val
writeTVarB = pure $ VFun $ \tvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    tvv <- force tvT
    av  <- force aT
    case tvv of
        VPrimObj (PrimTVar tv) -> do
            atomically (writeTVar tv av)
            pure VUnit
        _ -> error ("writeTVar: not a TVar: " <> showValForDebug tvv)

modifyTVar'B :: IO Val
modifyTVar'B = pure $ VFun $ \tvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    tvv <- force tvT
    fv  <- force fT
    case tvv of
        VPrimObj (PrimTVar tv) -> do
            cur  <- atomically (readTVar tv)
            curT <- newWHNFThunk cur
            new  <- apply fv curT
            atomically (writeTVar tv new)
            pure VUnit
        _ -> error ("modifyTVar': not a TVar: " <> showValForDebug tvv)

readTVarIOB :: IO Val
readTVarIOB = pure $ VFun $ \tvT -> pure $ VIO $ do
    tvv <- force tvT
    case tvv of
        VPrimObj (PrimTVar tv) -> readTVarIO tv
        _ -> error ("readTVarIO: not a TVar: " <> showValForDebug tvv)

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
        inner <- force innerT
        extractExceptionMessage inner
    VCon "ErrorCall" [msgT] ->
        tryValToString msgT (BC.pack "ErrorCall")
    VCon "ErrorCallWithLocation" (msgT : _) ->
        tryValToString msgT (BC.pack "ErrorCallWithLocation")
    VCon n _ -> pure n
    _        -> pure (BC.pack (showValForDebug val))
  where
    tryValToString :: Thunk -> ByteString -> IO ByteString
    tryValToString t fallback = do
        r <- CE.try @SomeException (force t >>= valToString)
        pure $ case r of
            Right s -> BC.pack s
            Left  _ -> fallback

-- | Extract the 'Val' from an 'IhcException'.
ihcExceptionToVal :: IhcException -> IO Val
ihcExceptionToVal (IhcException _ t) = force t

throwIOB :: IO Val
throwIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av  <- force aT
    exc <- valToIhcException av
    throwIO exc

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
forceToException :: Thunk -> IO IhcException
forceToException t = do
    r <- CE.try @SomeException (force t)
    case r of
        Right v  -> valToIhcException v
        Left  se -> do
            let msg = BC.pack (show se)
            t' <- newWHNFThunk (VStr msg)
            pure (IhcException msg t')

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

catchB :: IO Val
catchB = pure $ VFun $ \aT -> pure $ VFun $ \hT -> pure $ VIO $ do
    av <- force aT
    hv <- force hT
    catch
        (catch
            (runIOVal av)
            (\(exc :: IhcException) -> do
                excVal <- ihcExceptionToVal exc
                excT   <- newWHNFThunk excVal
                rv     <- apply hv excT
                runIOVal rv))
        (\(exc :: SomeException) -> do
            let msg = BC.pack (show exc)
            excT <- newWHNFThunk (VStr msg)
            rv   <- apply hv excT
            runIOVal rv)

handleB :: IO Val
handleB = pure $ VFun $ \hT -> pure $ VFun $ \aT -> pure $ VIO $ do
    hv <- force hT
    av <- force aT
    catch
        (catch
            (runIOVal av)
            (\(exc :: IhcException) -> do
                excVal <- ihcExceptionToVal exc
                excT   <- newWHNFThunk excVal
                rv     <- apply hv excT
                runIOVal rv))
        (\(exc :: SomeException) -> do
            let msg = BC.pack (show exc)
            excT <- newWHNFThunk (VStr msg)
            rv   <- apply hv excT
            runIOVal rv)

tryB :: IO Val
tryB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    r  <- CE.try @IhcException (runIOVal av)
    case r of
        Right v -> do
            vT <- newWHNFThunk v
            pure (VCon "Right" [vT])
        Left exc -> do
            excVal <- ihcExceptionToVal exc
            excT   <- newWHNFThunk excVal
            pure (VCon "Left" [excT])

evaluateB :: IO Val
evaluateB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    _  <- evaluate av
    pure av

mask_B :: IO Val
mask_B = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force aT
    mask_ (runIOVal av)

maskB :: IO Val
maskB = pure $ VFun $ \fT -> pure $ VIO $ do
    fv <- force fT
    mask $ \restore -> do
        let restoreVal = VFun $ \aT -> pure $ VIO $ do
                av <- force aT
                restore (runIOVal av)
        restoreT <- newWHNFThunk restoreVal
        rv <- apply fv restoreT
        runIOVal rv

bracketB :: IO Val
bracketB = pure $ VFun $ \acqT -> pure $ VFun $ \relT -> pure $ VFun $ \useT -> pure $ VIO $ do
    acqV <- force acqT
    relV <- force relT
    useV <- force useT
    bracket
        (runIOVal acqV)
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply relV resT
            _    <- runIOVal rv
            pure ())
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply useV resT
            runIOVal rv)

bracket_B :: IO Val
bracket_B = pure $ VFun $ \befT -> pure $ VFun $ \aftT -> pure $ VFun $ \thingT -> pure $ VIO $ do
    befV   <- force befT
    aftV   <- force aftT
    thingV <- force thingT
    bracket_
        (runIOVal befV >> pure ())
        (runIOVal aftV >> pure ())
        (runIOVal thingV)

finallyB :: IO Val
finallyB = pure $ VFun $ \aT -> pure $ VFun $ \cleanT -> pure $ VIO $ do
    av     <- force aT
    cleanV <- force cleanT
    finally
        (runIOVal av)
        (runIOVal cleanV >> pure ())

onExceptionB :: IO Val
onExceptionB = pure $ VFun $ \aT -> pure $ VFun $ \cleanT -> pure $ VIO $ do
    av     <- force aT
    cleanV <- force cleanT
    onException
        (runIOVal av)
        (runIOVal cleanV >> pure ())

throwToB :: IO Val
throwToB = pure $ VFun $ \tidT -> pure $ VFun $ \excT -> pure $ VIO $ do
    tidV <- force tidT
    excV <- force excT
    case tidV of
        VPrimObj (PrimThreadId tid) -> do
            exc <- valToIhcException excV
            throwTo tid exc
            pure VUnit
        _ -> error ("throwTo: not a ThreadId: " <> showValForDebug tidV)

displayExceptionB :: IO Val
displayExceptionB = pure $ VFun $ \eT -> do
    ev <- force eT
    let s = case ev of
                VStr msg  -> BC.unpack msg
                VCon n _  -> BC.unpack n
                _         -> showValForDebug ev
    stringToListValIO s

--------------------------------------------------------------------------------
-- Phase 2.9.5: Typeable / TypeRep / cast / Dynamic builtins
--------------------------------------------------------------------------------
-- Runtime representation:
--   TypeRep  = VCon "TypeRep"  [tyConThunk, argsListThunk]
--   TyCon    = VCon "TyCon"    [nameCharListThunk]
--   Dynamic  = VCon "Dynamic"  [typeRepThunk, valThunk]
--   Typeable dict = VCon "Dict_Typeable" [typeRepThunk]

typeRepB :: IO Val
typeRepB = pure $ VFun $ \dictT -> pure $ VFun $ \_proxyT -> do
    dictV <- force dictT
    extractTypeRep dictV

typeOfB :: IO Val
typeOfB = pure $ VFun $ \dictT -> pure $ VFun $ \_valT -> do
    dictV <- force dictT
    extractTypeRep dictV

castB :: IO Val
castB = pure $ VFun $ \dictAT -> pure $ VFun $ \dictBT -> pure $ VFun $ \valT -> do
    dictAV <- force dictAT
    dictBV <- force dictBT
    trA    <- extractTypeRep dictAV
    trB    <- extractTypeRep dictBV
    eq     <- typeRepEq trA trB
    if eq then pure (VCon "Just" [valT])
          else pure (VCon "Nothing" [])

eqTB :: IO Val
eqTB = pure $ VFun $ \dictAT -> pure $ VFun $ \dictBT -> do
    dictAV <- force dictAT
    dictBV <- force dictBT
    trA    <- extractTypeRep dictAV
    trB    <- extractTypeRep dictBV
    eq     <- typeRepEq trA trB
    if eq
        then do { reflT <- newWHNFThunk (VCon "Refl" []); pure (VCon "Just" [reflT]) }
        else pure (VCon "Nothing" [])

toDynB :: IO Val
toDynB = pure $ VFun $ \dictT -> pure $ VFun $ \valT -> do
    dictV <- force dictT
    tr    <- extractTypeRep dictV
    trT   <- newWHNFThunk tr
    pure (VCon "Dynamic" [trT, valT])

fromDynamicB :: IO Val
fromDynamicB = pure $ VFun $ \dictBT -> pure $ VFun $ \dynT -> do
    dictBV <- force dictBT
    dynV   <- force dynT
    trB    <- extractTypeRep dictBV
    case dynV of
        VCon "Dynamic" [trAT, storedT] -> do
            trA <- force trAT
            eq  <- typeRepEq trA trB
            pure (if eq then VCon "Just" [storedT] else VCon "Nothing" [])
        _ -> pure (VCon "Nothing" [])

fromDynB :: IO Val
fromDynB = pure $ VFun $ \dictT -> pure $ VFun $ \dynT -> pure $ VFun $ \defT -> do
    dictV <- force dictT
    dynV  <- force dynT
    trB   <- extractTypeRep dictV
    case dynV of
        VCon "Dynamic" [trAT, storedT] -> do
            trA <- force trAT
            eq  <- typeRepEq trA trB
            if eq then force storedT else force defT
        _ -> force defT

dynTypeRepB :: IO Val
dynTypeRepB = pure $ VFun $ \dynT -> do
    dynV <- force dynT
    case dynV of
        VCon "Dynamic" [trT, _] -> force trT
        _ -> mkTypeRep "Unknown"

mkTyCon3B :: IO Val
mkTyCon3B = pure $ VFun $ \_ -> pure $ VFun $ \_ -> pure $ VFun $ \nameT -> do
    nameV    <- force nameT
    nameStrT <- newWHNFThunk nameV
    pure (VCon "TyCon" [nameStrT])

mkTyConAppB :: IO Val
mkTyConAppB = pure $ VFun $ \tyConT -> pure $ VFun $ \argsT -> do
    tyConV     <- force tyConT
    argsV      <- force argsT
    tyConThunk <- newWHNFThunk tyConV
    argsThunk  <- newWHNFThunk argsV
    pure (VCon "TypeRep" [tyConThunk, argsThunk])

tyConNameB :: IO Val
tyConNameB = pure $ VFun $ \tyConT -> do
    tyConV <- force tyConT
    case tyConV of
        VCon "TyCon" [nameT] -> force nameT
        _ -> pure (VCon "[]" [])

typeRepTyConB :: IO Val
typeRepTyConB = pure $ VFun $ \trT -> do
    trV <- force trT
    case trV of
        VCon "TypeRep" [tcT, _] -> force tcT
        _ -> pure (VCon "TyCon" [])

typeRepArgsB :: IO Val
typeRepArgsB = pure $ VFun $ \trT -> do
    trV <- force trT
    case trV of
        VCon "TypeRep" [_, argsT] -> force argsT
        _ -> pure (VCon "[]" [])

-- | Extract a TypeRep from a Typeable dict or raw TypeRep value.
extractTypeRep :: Val -> IO Val
extractTypeRep (VCon "Dict_Typeable" [trT]) = force trT
extractTypeRep v@(VCon "TypeRep" _)         = pure v
extractTypeRep _                            = mkTypeRep "Unknown"

-- | Dynamic constructor as a curried function value.
dynamicCtorB :: IO Val
dynamicCtorB = pure $ VFun $ \trT -> pure $ VFun $ \valT -> do
    trV    <- force trT
    trThunk <- newWHNFThunk trV
    pure (VCon "Dynamic" [trThunk, valT])

-- | Build built-in Typeable instance dictionaries for well-known types.
buildBuiltinTypeableInsts :: IO [(Name, Thunk)]
buildBuiltinTypeableInsts = mapM mkDict prims
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
        tr    <- mkTypeRep tyName
        trT   <- newWHNFThunk tr
        dictT <- newWHNFThunk (VCon "Dict_Typeable" [trT])
        pure ("typeableDict_" <> tag, dictT)
