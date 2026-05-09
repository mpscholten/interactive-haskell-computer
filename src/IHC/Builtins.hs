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
    ) where

import Control.Concurrent
    ( forkIO, killThread, myThreadId, threadDelay
    , threadWaitRead, threadWaitWrite
    )
import Control.Concurrent.MVar
    ( MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar
    , modifyMVar_, modifyMVar, tryTakeMVar, tryPutMVar, isEmptyMVar
    , withMVar, swapMVar
    )
import Control.Concurrent.STM
    ( TVar, atomically, retry, check
    , newTVarIO, readTVar, writeTVar, readTVarIO
    )
import qualified Control.Exception as CE
import Control.Exception
    ( throwIO, catch, try, evaluate, mask, mask_
    , bracket, bracket_, bracketOnError, finally, onException, throwTo
    , SomeException
    )
import Foreign.C.Error (Errno(..), getErrno, eAGAIN, eWOULDBLOCK, eINTR)
import Foreign.C.Types (CInt(..), CSize(..))
import Data.Bits
    ( (.&.), (.|.), xor, complement, shiftL, shiftR
    , popCount, countLeadingZeros, finiteBitSize
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32)
import Foreign.C.String (peekCAString, withCString)
import Foreign.ForeignPtr
    ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr, touchForeignPtr
    , newForeignPtr_
    , plusForeignPtr
    )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, mallocBytes, free)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, IntPtr, castPtr, plusPtr, nullPtr, minusPtr, intPtrToPtr, ptrToIntPtr)
import qualified Foreign.Ptr as FP
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff, peekElemOff, pokeElemOff, sizeOf)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
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
import qualified System.Posix.IO as PosixIO
import System.Posix.Types (Fd)

import Control.Monad (when)
import IHC.AST  (Name, Expr(..))
import IHC.Classes
    ( ClassRegistry, lookupInstanceMethod, registerInstance, typeTagOf
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

{-# NOINLINE foreignPtrWord8RangesRef #-}
foreignPtrWord8RangesRef :: IORef [(IntPtr, IntPtr)]
foreignPtrWord8RangesRef = unsafePerformIO (newIORef [])

markWord8Ptr :: Ptr Word8 -> IO ()
markWord8Ptr p = markWord8PtrRange p 1

markWord8PtrRange :: Ptr Word8 -> Int -> IO ()
markWord8PtrRange p len =
    let start = ptrToIntPtr (castPtr p)
        end   = start + fromIntegral (max 1 len)
    in modifyIORef' foreignPtrWord8RangesRef ((start, end) :)

markForeignPtrWord8 :: ForeignPtr Word8 -> IO ()
markForeignPtrWord8 fp =
    markWord8Ptr (castPtr (unsafeForeignPtrToPtr fp))

isMarkedWord8Ptr :: Ptr Word8 -> IO Bool
isMarkedWord8Ptr p =
    let addr = ptrToIntPtr (castPtr p)
    in any (\(start, end) -> addr >= start && addr < end)
        <$> readIORef foreignPtrWord8RangesRef

ptrValToPtr :: Val -> IO (Ptr Word8)
ptrValToPtr (VPrimObj (PrimPtr p)) = pure p
ptrValToPtr (VCon "Ptr" [pT]) = force legacyHooks pT >>= ptrValToPtr
ptrValToPtr other = error ("expected Ptr: " <> showValForDebug other)

foreignPtrValToForeignPtr :: Val -> IO (ForeignPtr Word8)
foreignPtrValToForeignPtr (VPrimObj (PrimForeignPtr fp)) = pure fp
foreignPtrValToForeignPtr (VCon "ForeignPtr" [addrT, gutsT]) = do
    gv <- force legacyHooks gutsT
    case gv of
        VPrimObj (PrimForeignPtr fp) -> pure fp
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
    -- Standard handles.
    stdinT  <- newWHNFThunk (VPrimObj (PrimHandle stdin))
    stdoutT <- newWHNFThunk (VPrimObj (PrimHandle stdout))
    stderrT <- newWHNFThunk (VPrimObj (PrimHandle stderr))
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
    let unboxCtors = [("(#,#)", unbox2T), ("(#,,#)", unbox3T)]
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
    dynCtorT  <- newLazyBuiltinThunk dynamicCtorB
    let phase295Ctors = [("Proxy", proxyT), ("Dynamic", dynCtorT)]
    -- Phase 2.9.5: Built-in Typeable dictionaries for primitive types.
    -- Lazy-init: each dict costs two IORef allocs + a VCon, and most
    -- programs only touch typeableDict_Int / _Char / _Bool.
    typeableInsts <- buildBuiltinTypeableInsts
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
    [ ("min",      minDispatch reg)
    , ("max",      maxDispatch reg)
    -- 'gcd' graduated to source-loaded.  Body lives at
    --   ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Real.hs:928-930
    --     gcd x y = gcd' (abs x) (abs y)
    --       where gcd' a 0 = a
    --             gcd' a b = gcd' b (a `rem` b)
    -- abs already graduated in Phase E; rem is registered below.
    -- The Phase F buildOwnerLocalEnv guard handles class-method
    -- resolution inside the source-loaded body.
    , ("sqrt",     unaryOpFloat sqrt)
    , ("floor",    floatToIntB floor)
    , ("ceiling",  floatToIntB ceiling)
    , ("round",    floatToIntB round)
    , ("truncate", floatToIntB truncate)
    , ("fromIntegral", fromIntegralB)
    , ("fromInteger",  fromIntegralB)
    , ("maxBound",     maxBoundB)
    , ("minBound",     minBoundB)
    -- Comparisons: Phase 2.3 dispatch via ClassRegistry.
    -- Builtin instances for Int, Char, Bool, [] are handled inline;
    -- user-defined instances are looked up from the registry.
    , ("==",       eqDispatch reg)
    , ("/=",       neqDispatch reg)
    , ("<",        ordDispatch reg 0)
    , ("<=",       ordDispatch reg 1)
    , (">",        ordDispatch reg 2)
    , (">=",       ordDispatch reg 3)
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
    -- Strings / lists (strings are [Char] from Phase 2.2 onward)
    , ("show",     showDispatch reg)
    -- Data.ByteString shims kept as documented carve-outs because
    -- source-loading bytestring exposes interpreter gaps:
    --   unpack    — internal recursive function "non-exhaustive patterns"
    --   append    — `append = mappend`, Monoid dispatch yields a
    --               <<ihc-method-placeholder>> rather than the BS instance
    --   concat    — `concat = mconcat`, same Monoid dispatch gap
    --   singleton — uses `allBytes` 256-byte static buffer; hits
    --               `expected Ptr: <:>` on the Ptr cons
    --   replicate — silent wrong output ("/\NUL\NUL" for `replicate 3 65`)
    --   index     — body calls `length ps`; the polymorphic `length`
    --               routes to Foldable.length, which has no BS instance
    -- The other 7 entries (empty, null, length, pack, take, drop, head)
    -- source-load cleanly and are dropped in this change.
    , ("Data.ByteString.unpack",    bsUnpackB)
    , ("Data.ByteString.append",    bsAppendB)
    , ("Data.ByteString.concat",    bsConcatB)
    , ("Data.ByteString.singleton", bsSingletonB)
    , ("Data.ByteString.replicate", bsReplicateB)
    , ("Data.ByteString.index",     bsIndexB)
    -- Unique generation is an RTS/global-state service. Vault uses these as
    -- ordered map keys, so represent them as Unique Integer-style constructors
    -- backed by a host counter.
    , ("newUnique", newUniqueB)
    , ("hashUnique", hashUniqueB)
    , ("Data.Unique.newUnique", newUniqueB)
    , ("Data.Unique.hashUnique", hashUniqueB)
    , ("Data.Unique.Really.newUnique", newUniqueB)
    , ("Data.Unique.Really.hashUnique", hashUniqueB)
    -- Data.ByteString.Char8.putStrLn: kept as a host shim because
    -- source-loaded `hPutStrLn` calls `length ps` against a ByteString,
    -- but the interpreter's `Prelude hiding (Foldable(..))` handling
    -- doesn't yet remove `Prelude.length` from scope, so the call
    -- routes to the polymorphic `length` and fails with
    -- "length: not a list: <BS...>". Remove this shim once the
    -- import-hiding dispatch is fixed at the Scheduler level.
    , ("Data.ByteString.Char8.putStrLn",  bs8PutStrLnB)
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
    -- 'putStrLn' is source-loaded (slice 1).  'show' remains a shim
    -- ('showDispatch reg' below) because it's the entry point of the
    -- Show class registry — graduating it is slice 5+ territory.
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
    -- It was never shimmed in the first place; documented here for
    -- symmetry with 'getLine'.
    -- Monad core: >>=  and >>  dispatch via class registry for non-IO monads
    -- (e.g. ST s a, State s, Maybe, etc.) while falling back to the plain IO
    -- implementation for VIO values.
    , (">>=",      bindDispatch reg)
    , ("GHC.Internal.Base.>>=", bindDispatch reg)
    , ("Prelude.>>=", bindDispatch reg)
    , (">>",       seqDispatch reg)
    , ("GHC.Internal.Base.>>", seqDispatch reg)
    , ("Prelude.>>", seqDispatch reg)
    , ("return",   returnB)
    , ("GHC.Internal.Base.return", returnB)
    , ("Prelude.return", returnB)
    , ("pure",     returnB)
    , ("GHC.Internal.Base.pure", returnB)
    , ("Prelude.pure", returnB)
    , ("fmap",     fmapDispatch reg)
    , ("GHC.Internal.Base.fmap", fmapDispatch reg)
    , ("Prelude.fmap", fmapDispatch reg)
    , ("<*>",      apDispatch reg)
    , ("GHC.Internal.Base.<*>", apDispatch reg)
    , ("Prelude.<*>", apDispatch reg)
    -- 'Semigroup.(<>)' — argument-directed dispatch keyed on the LHS
    -- tag.  Source-loaded code (warp's HTTP response builders, blaze's
    -- html builder, etc.) and user code (e.g. instance defining
    -- '(<>)' in prefix form) reach for it as a bare 'EVar', so the
    -- env must bind it directly rather than relying on whole-program
    -- elaboration.
    , ("<>",       sappendDispatch reg)
    , ("GHC.Internal.Base.<>", sappendDispatch reg)
    , ("Prelude.<>", sappendDispatch reg)
    , ("Data.Semigroup.<>", sappendDispatch reg)
    --
    -- 'join' deliberately omitted: it has source at
    -- ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Base.hs:1292-1293
    --     join :: Monad m => m (m a) -> m a
    --     join x = x >>= id
    -- The source dispatches via the Monad class — '>>=' is still
    -- shimmed ('bindDispatch reg' below) so the IO/Maybe/Either
    -- variants reach their existing dispatcher; user-defined Monad
    -- instances flow through the registry the same way.
    --
    -- 'void' deliberately omitted: it has source at
    -- ~/.cache/ihc/sources/base-4.19.0.0/Data/Functor.hs:210-211
    --     void :: Functor f => f a -> f ()
    --     void x = () <$ x
    -- Per CLAUDE.md "Builtin modules: minimum surface only". Source
    -- dispatches via the Functor class ('<$' default body
    -- '(<$) = fmap . const' from GHC.Internal.Base) which routes
    -- through 'fmapDispatch' (still shimmed). The empty-data-decl
    -- scanner fix at commit a1db2a6 was a prerequisite — without it
    -- 'fmapDispatch' read the IO ctor's tag as "PrimMVar".
    -- @Control.Arrow.first@ / @second@ - for the @(->)@ arrow.  Warp's
    -- @runSettingsConnectionMaker@ uses
    -- @first ((,TCP) \<$\>)@ to lift @(,Transport)@ into the IO action and
    -- map it over the @(connectionMaker, sockAddr)@ tuple.  Our class
    -- dispatcher mis-classifies the call: arg-direction lands on the
    -- tuple (since the function is non-dispatchable) and looks up
    -- @Arrow (,) first@, which doesn't exist (Arrow is for arrows, not
    -- tuples).  Host directly under the (->)-instance semantics:
    -- @first f (a, b) = (f a, b)@.
    , ("first",   firstFnB)
    , ("Control.Arrow.first", firstFnB)
    , ("second",  secondFnB)
    , ("Control.Arrow.second", secondFnB)
    -- IORef
    , ("newIORef",    newIORefB)
    , ("GHC.IORef.newIORef", newIORefB)
    , ("GHC.Internal.IORef.newIORef", newIORefB)
    , ("GHC.Internal.Data.IORef.newIORef", newIORefB)
    , ("Data.IORef.newIORef", newIORefB)
    , ("readIORef",   readIORefB)
    , ("GHC.IORef.readIORef", readIORefB)
    , ("GHC.Internal.IORef.readIORef", readIORefB)
    , ("GHC.Internal.Data.IORef.readIORef", readIORefB)
    , ("Data.IORef.readIORef", readIORefB)
    , ("writeIORef",  writeIORefB)
    , ("GHC.IORef.writeIORef", writeIORefB)
    , ("GHC.Internal.IORef.writeIORef", writeIORefB)
    , ("GHC.Internal.Data.IORef.writeIORef", writeIORefB)
    , ("Data.IORef.writeIORef", writeIORefB)
    , ("modifyIORef", modifyIORefB)
    , ("GHC.IORef.modifyIORef", modifyIORefB)
    , ("GHC.Internal.IORef.modifyIORef", modifyIORefB)
    , ("GHC.Internal.Data.IORef.modifyIORef", modifyIORefB)
    , ("Data.IORef.modifyIORef", modifyIORefB)
    , ("modifyIORef'",modifyIORefB)             -- same, no laziness diff here
    , ("GHC.IORef.modifyIORef'", modifyIORefB)
    , ("GHC.Internal.IORef.modifyIORef'", modifyIORefB)
    , ("GHC.Internal.Data.IORef.modifyIORef'", modifyIORefB)
    , ("Data.IORef.modifyIORef'", modifyIORefB)
    , ("atomicModifyIORef'", atomicModifyIORefB)
    , ("GHC.IORef.atomicModifyIORef'", atomicModifyIORefB)
    , ("GHC.Internal.IORef.atomicModifyIORef'", atomicModifyIORefB)
    , ("GHC.Internal.Data.IORef.atomicModifyIORef'", atomicModifyIORefB)
    , ("Data.IORef.atomicModifyIORef'", atomicModifyIORefB)
    -- mkWeakIORef is source-defined only as a thin wrapper over mkWeak#,
    -- so the observable operation is RTS-exclusive.  We keep the Weak inert:
    -- network's mkSocket discards it after registering the close finalizer.
    , ("mkWeakIORef", mkWeakIORefB)
    , ("GHC.IORef.mkWeakIORef", mkWeakIORefB)
    , ("GHC.Internal.Data.IORef.mkWeakIORef", mkWeakIORefB)
    , ("Data.IORef.mkWeakIORef", mkWeakIORefB)
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
    , ("assert",      assertB)
    , ("error",       errorB)
    , ("undefined",   undefinedB)
    -- B.1: debug-only superclass-relation probe.  Source-loaded code
    -- can call @__ihc_class_supers \"MyOrd\"@ to inspect the global
    -- superclass map; useful for testing that the scanner captured
    -- the @class C a => D a@ relation. Single argument is a [Char]
    -- list (a String); result is a [[Char]] list (a [String]).
    , ("__ihc_class_supers", classSupersProbeB)
    , ("exitWith",    exitWithB)
    , ("exitSuccess", exitSuccessB)
    -- Char / numeric conversions
    , ("ord",         ordB)
    , ("chr",         chrB)
    , ("ord#",        ordB)
    , ("chr#",        chrB)
    -- isTrue# :: Int# -> Bool.  Projects the 1#/0# unboxed-Int encoding
    -- of a comparison primop result (@==#@, @<#@, etc.) into a regular
    -- Bool.  Our Int# is VInt, so it's a plain @/= 0@.
    , ("isTrue#",     isTrueHashB)
    , ("fromIntegral", fromIntegralB)
    -- Phase 2.8: RealWorld / State primops
    , ("realWorld#",               realWorldB)
    , ("noDuplicate#",            noDuplicateB)  -- GHC primop: no-op in interpreter
    , ("touch#",                  touchHashB)     -- GHC primop: keep-alive touch, no-op at Val level
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
    -- GHC.Prim-only raw-address Int access.  Source-loaded
    -- GHC.Internal.Storable defines writeIntOffPtr/readIntOffPtr in terms of
    -- these primops; there is no .hs implementation to interpret below them.
    , ("readIntOffAddr#",  readIntOffAddrHashB)
    , ("writeIntOffAddr#", writeIntOffAddrHashB)
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
    , ("Foreign.ForeignPtr.withForeignPtr", withForeignPtrB)
    , ("Foreign.ForeignPtr.unsafeWithForeignPtr", withForeignPtrB)
    , ("Foreign.ForeignPtr.Imp.withForeignPtr", withForeignPtrB)
    , ("Foreign.ForeignPtr.Safe.withForeignPtr", withForeignPtrB)
    , ("GHC.ForeignPtr.withForeignPtr", withForeignPtrB)
    , ("GHC.Internal.Foreign.ForeignPtr.withForeignPtr", withForeignPtrB)
    , ("GHC.Internal.Foreign.ForeignPtr.unsafeWithForeignPtr", withForeignPtrB)
    , ("GHC.Internal.Foreign.ForeignPtr.Imp.withForeignPtr", withForeignPtrB)
    , ("plusForeignPtr",             plusForeignPtrB)
    , ("minusForeignPtr",            minusForeignPtrB)
    , ("GHC.ForeignPtr.minusForeignPtr", minusForeignPtrB)
    , ("touchForeignPtr",            touchForeignPtrB)
    , ("newForeignPtr_",             newForeignPtr_B)
    , ("newForeignPtr",              newForeignPtrB)
    , ("addForeignPtrFinalizer",     addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.newForeignPtr", newForeignPtrB)
    , ("Foreign.ForeignPtr.Imp.newForeignPtr", newForeignPtrB)
    , ("Foreign.ForeignPtr.Safe.newForeignPtr", newForeignPtrB)
    , ("GHC.ForeignPtr.newForeignPtr", newForeignPtrB)
    , ("GHC.Internal.Foreign.ForeignPtr.newForeignPtr", newForeignPtrB)
    , ("GHC.Internal.Foreign.ForeignPtr.Imp.newForeignPtr", newForeignPtrB)
    , ("Foreign.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.Imp.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("Foreign.ForeignPtr.Safe.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.Internal.Foreign.ForeignPtr.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    , ("GHC.Internal.Foreign.ForeignPtr.Imp.addForeignPtrFinalizer", addForeignPtrFinalizerB)
    -- Phase 2.8: Storable ops on Ptr
    , ("peek",         peekB)
    , ("Foreign.peek", peekB)
    , ("Foreign.Storable.peek", peekB)
    , ("GHC.Internal.Foreign.Storable.peek", peekB)
    , ("Network.Socket.Imports.peek", peekB)
    , ("poke",         pokeB)
    , ("Foreign.poke", pokeB)
    , ("Foreign.Storable.poke", pokeB)
    , ("GHC.Internal.Foreign.Storable.poke", pokeB)
    , ("Network.Socket.Imports.poke", pokeB)
    , ("peekByteOff",  peekByteOffB)
    , ("Foreign.peekByteOff", peekByteOffB)
    , ("Foreign.Storable.peekByteOff", peekByteOffB)
    , ("GHC.Internal.Foreign.Storable.peekByteOff", peekByteOffB)
    , ("Network.Socket.Imports.peekByteOff", peekByteOffB)
    , ("pokeByteOff",  pokeByteOffB)
    , ("Foreign.pokeByteOff", pokeByteOffB)
    , ("Foreign.Storable.pokeByteOff", pokeByteOffB)
    , ("GHC.Internal.Foreign.Storable.pokeByteOff", pokeByteOffB)
    , ("Network.Socket.Imports.pokeByteOff", pokeByteOffB)
    -- Phase 2.8: MutableByteArray# family
    , ("newByteArray#",             newByteArrayB)
    , ("newPinnedByteArray#",       newPinnedByteArrayB)
    , ("newAlignedPinnedByteArray#", newAlignedPinnedByteArrayB)
    , ("writeWord8Array#",          writeWord8ArrayB)
    , ("readWord8Array#",           readWord8ArrayB)
    , ("indexWord8Array#",          indexWord8ArrayB)
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
    , ("memcpy",     memcpyB)
    , ("memcpyFp",   memcpyFpB)
    , ("copyBytes",  copyBytesB)  -- Foreign.Marshal.Utils.copyBytes: wraps memcpy (primop-backed, no Haskell source)
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
    , ("uncheckedIShiftRA#", uncheckedIShiftRAB)
    -- Bitwise Int# primops: aliased to the boxed bit ops since
    -- IHC represents Int# as VInt (Int64-backed).  Required by
    -- the Int64-encoding helpers in 'GHC.Num.Integer' that
    -- 'integerDecodeDouble#' rides through.
    , ("andI#",             bitAndB)
    , ("orI#",              bitOrB)
    , ("xorI#",             bitXorB)
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
    -- @decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)@.
    -- Bottom-of-stack primop for 'decodeFloat' on Double:
    -- 'GHC.Num.Integer.integerDecodeDouble#' wraps it
    -- (ghc-bignum-1.3/src/GHC/Num/Integer.hs:1046).  Required by
    -- the source-loaded RealFrac Double / properFractionFloat
    -- chain, which 'floor'\/'ceiling'\/'round'\/'truncate' ride.
    , ("decodeDouble_Int64#", decodeDoubleInt64HashB)
    -- Phase 2.8: misc
    , ("cstringLength#",  cstringLengthB)
    , ("unpackCString#",  unpackCStringB)
    , ("unpackCStringUtf8#", unpackCStringB)
    -- Foreign.C.String shortcuts — source bodies reach for RTS locale
    -- state via getForeignEncoding; we bypass that and go straight to
    -- ASCII-byte marshalling (matches GHC's default for libc FFI).
    , ("withCString",     withCStringB)
    , ("withCStringLen",  withCStringLenB)
    , ("withCStringLen0", withCStringLenB)
    , ("with",            withB)
    , ("Foreign.Marshal.Utils.with", withB)
    , ("GHC.Internal.Foreign.Marshal.Utils.with", withB)
    , ("peekCString",     peekCStringB)
    , ("peekCAString",    peekCStringB)
    , ("newCString",      newCStringB)
    , ("newCAString",     newCStringB)
    , ("sizeOf",       sizeOfB)
    , ("Foreign.Storable.sizeOf", sizeOfB)
    , ("GHC.Internal.Foreign.Storable.sizeOf", sizeOfB)
    , ("Network.Socket.Imports.sizeOf", sizeOfB)
    , ("alignment",    alignmentB)
    , ("Foreign.Storable.alignment", alignmentB)
    , ("GHC.Internal.Foreign.Storable.alignment", alignmentB)
    , ("Network.Socket.Imports.alignment", alignmentB)
    -- Network.Socket.socket creates an OS file descriptor and wraps it in
    -- network's Socket constructor.  The fd allocation is inherently an
    -- OS/RTS boundary, so IHC hosts this syscall while preserving the source
    -- Socket value shape used by the rest of the interpreted network package.
    , ("socket", socketCreateB)
    , ("Network.Socket.socket", socketCreateB)
    , ("Network.Socket.Syscall.socket", socketCreateB)
    -- setsockopt is an OS syscall.  Keep the Haskell option constants and
    -- Socket shape, but perform the fd-level call in the host.
    , ("setSocketOption", socketSetOptionB)
    , ("Network.Socket.setSocketOption", socketSetOptionB)
    , ("Network.Socket.Options.setSocketOption", socketSetOptionB)
    -- 'Network.Socket.Options' uses pattern synonyms for the common
    -- 'SocketOption' constants (e.g. @pattern NoDelay = SockOpt 6 1@).
    -- Our parser doesn't yet expand pattern synonyms; warp uses
    -- @setSocketOption s NoDelay 1@ (and 'KeepAlive', 'ReuseAddr') so
    -- we host them as plain VCon "SockOpt" values keyed by the
    -- platform constants.  Values from
    -- 'Network.Socket.Options' on macOS / Linux post-hsc.
    , ("NoDelay",    sockOptB 6 1)              -- IPPROTO_TCP, TCP_NODELAY
    , ("Network.Socket.NoDelay",            sockOptB 6 1)
    , ("Network.Socket.Options.NoDelay",    sockOptB 6 1)
    , ("ReuseAddr",  sockOptB 65535 4)          -- SOL_SOCKET, SO_REUSEADDR
    , ("Network.Socket.ReuseAddr",          sockOptB 65535 4)
    , ("Network.Socket.Options.ReuseAddr",  sockOptB 65535 4)
    , ("KeepAlive",  sockOptB 65535 8)          -- SOL_SOCKET, SO_KEEPALIVE
    , ("Network.Socket.KeepAlive",          sockOptB 65535 8)
    , ("Network.Socket.Options.KeepAlive",  sockOptB 65535 8)
    , ("Broadcast",  sockOptB 65535 32)         -- SOL_SOCKET, SO_BROADCAST
    , ("Network.Socket.Broadcast",          sockOptB 65535 32)
    , ("Network.Socket.Options.Broadcast",  sockOptB 65535 32)
    , ("ReusePort",  sockOptB 65535 512)        -- SOL_SOCKET, SO_REUSEPORT
    , ("Network.Socket.ReusePort",          sockOptB 65535 512)
    , ("Network.Socket.Options.ReusePort",  sockOptB 65535 512)
    -- listen(2) is another fd-level syscall in Network.Socket.Syscall.
    , ("listen", socketListenB)
    , ("Network.Socket.listen", socketListenB)
    , ("Network.Socket.Syscall.listen", socketListenB)
    -- accept(2) blocks for the next connection and returns a network Socket
    -- plus SockAddr.  This is an OS boundary; the Haskell connection logic
    -- above it remains source-interpreted.
    , ("accept", socketAcceptB)
    , ("Network.Socket.accept", socketAcceptB)
    , ("Network.Socket.SockAddr.accept", socketAcceptB)
    , ("Network.Socket.Syscall.accept", socketAcceptB)
    -- getsockname(2), used by Warp to populate connMySockAddr.
    , ("getSocketName", socketGetNameB)
    , ("Network.Socket.getSocketName", socketGetNameB)
    , ("Network.Socket.SockAddr.getSocketName", socketGetNameB)
    , ("Network.Socket.Name.getSocketName", socketGetNameB)
    -- Raw heap buffer allocation used by Warp's response buffers.
    , ("mallocBytes", mallocBytesB)
    , ("Foreign.Marshal.Alloc.mallocBytes", mallocBytesB)
    , ("GHC.Internal.Foreign.Marshal.Alloc.mallocBytes", mallocBytesB)
    , ("free", freeB)
    , ("Foreign.Marshal.Alloc.free", freeB)
    , ("GHC.Internal.Foreign.Marshal.Alloc.free", freeB)
    , ("bind", socketBindB)
    , ("Network.Socket.bind", socketBindB)
    , ("Network.Socket.Types.bind", socketBindB)
    , ("Network.Socket.SockAddr.bind", socketBindB)
    , ("Network.Socket.Syscall.bind", socketBindB)
    -- Network.Socket.bind is the actual OS bind(2) boundary for AF_INET/AF_INET6
    -- sockets. We keep SockAddr data constructors source-interpreted, but perform
    -- the sockaddr marshalling + bind syscall in the host.
    -- Network.Socket.close / close' ultimately coordinate fd shutdown through
    -- closeFdWith and the host RTS/OS socket object. We keep network's Socket
    -- value shape source-compatible, but perform the actual invalidation +
    -- close(2) in the host.
    , ("close", socketCloseB False)
    , ("close'", socketCloseB True)
    , ("Network.Socket.close", socketCloseB False)
    , ("Network.Socket.close'", socketCloseB True)
    , ("Network.Socket.Types.close", socketCloseB False)
    , ("Network.Socket.Types.close'", socketCloseB True)
    -- Network.Socket.withFdSocket reads the live fd out of the mutable Socket
    -- cell and runs an IO callback. The Socket is host-backed already, so we
    -- bridge this directly rather than re-entering the interpreted wrapper.
    , ("withFdSocket", withFdSocketB)
    , ("Network.Socket.withFdSocket", withFdSocketB)
    , ("Network.Socket.Types.withFdSocket", withFdSocketB)
    -- Network.Socket's Socket value is already represented by IHC as a
    -- host-backed fd-bearing constructor. fdSocket/unsafeFdSocket expose that
    -- fd as IO CInt in network >= 3, so this is an RTS/OS bridge over the
    -- existing host socket object rather than a Hackage-library shim.
    , ("fdSocket", socketFdB)
    , ("unsafeFdSocket", socketFdB)
    , ("Network.Socket.fdSocket", socketFdB)
    , ("Network.Socket.unsafeFdSocket", socketFdB)
    , ("Network.Socket.Types.fdSocket", socketFdB)
    , ("Network.Socket.Types.unsafeFdSocket", socketFdB)
    -- sendBuf/recvBuf are the fd-level OS send(2)/recv(2) boundaries used by
    -- Network.Socket.ByteString. Keep the ByteString API source-interpreted,
    -- but perform the actual socket syscall in the host.
    , ("sendBuf", socketSendBufB)
    , ("recvBuf", socketRecvBufB)
    , ("Network.Socket.Buffer.sendBuf", socketSendBufB)
    , ("Network.Socket.Buffer.recvBuf", socketRecvBufB)
    , ("Network.Socket.sendBuf", socketSendBufB)
    , ("Network.Socket.recvBuf", socketRecvBufB)
    -- Phase C.3 (builtins-removal): the @Settings@ field accessors
    -- (settingsPort/Host/Timeout/FdCacheDuration/FileInfoCacheDuration)
    -- used to live here as positional shims that indexed into a host-
    -- constructed VCon.  They were removed once defaultSettings became
    -- source-loaded via Scheduler.preludeDirectOwner: the loaded module
    -- registers all Settings fields in lmFieldReg, and tryFieldSlot
    -- synthesises the accessors automatically.  Helpers warpSettings*B
    -- and settingsFieldB went with them.
    -- Network.Socket AddrInfo record-field accessors.  The host backing
    -- builds AddrInfo as @VCon "AddrInfo" [flags, family, socktype,
    -- protocol, addr, canonName]@ via 'peekAddrInfoVal'; warp's
    -- 'Network.Wai.Handler.Warp.Run' calls @NS.addrFamily@ etc. on the
    -- result, so each accessor needs a builtin that pulls the
    -- corresponding field index.
    , ("addrFlags",      addrInfoFieldB "addrFlags" 0)
    , ("Network.Socket.addrFlags", addrInfoFieldB "addrFlags" 0)
    , ("Network.Socket.Info.addrFlags", addrInfoFieldB "addrFlags" 0)
    , ("addrFamily",     addrInfoFieldB "addrFamily" 1)
    , ("Network.Socket.addrFamily", addrInfoFieldB "addrFamily" 1)
    , ("Network.Socket.Info.addrFamily", addrInfoFieldB "addrFamily" 1)
    , ("addrSocketType", addrInfoFieldB "addrSocketType" 2)
    , ("Network.Socket.addrSocketType", addrInfoFieldB "addrSocketType" 2)
    , ("Network.Socket.Info.addrSocketType", addrInfoFieldB "addrSocketType" 2)
    , ("addrProtocol",   addrInfoFieldB "addrProtocol" 3)
    , ("Network.Socket.addrProtocol", addrInfoFieldB "addrProtocol" 3)
    , ("Network.Socket.Info.addrProtocol", addrInfoFieldB "addrProtocol" 3)
    , ("addrAddress",    addrInfoFieldB "addrAddress" 4)
    , ("Network.Socket.addrAddress", addrInfoFieldB "addrAddress" 4)
    , ("Network.Socket.Info.addrAddress", addrInfoFieldB "addrAddress" 4)
    , ("addrCanonName",  addrInfoFieldB "addrCanonName" 5)
    , ("Network.Socket.addrCanonName", addrInfoFieldB "addrCanonName" 5)
    , ("Network.Socket.Info.addrCanonName", addrInfoFieldB "addrCanonName" 5)
    -- Network.Socket.getAddrInfo: host @getaddrinfo(3)@.  warp's listen
    -- pipeline calls it through streaming-commons' bindPortGenEx.  We
    -- ignore hints (NULL) for now — the OS gives back a TCP-stream-
    -- compatible list anyway, which is all warp examines.
    , ("getAddrInfo",     getAddrInfoB)
    , ("Network.Socket.getAddrInfo", getAddrInfoB)
    , ("Network.Socket.Info.getAddrInfo", getAddrInfoB)
    -- Phase 2.8: additional numeric ops needed by containers
    , ("fromInteger",  fromIntegralB)
    , ("toInteger",    fromIntegralB)
    , ("quot",         binOpInt quot)
    , ("rem",          binOpInt rem)
    -- 'div' graduated with the rest of the TODO 2.6 block: it now
    -- routes through the Integral Int instance (a `divInt` b) and
    -- bottoms on divInt# registered below.
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
    -- Power operator
    , ("^",            powOpB)
    , ("^^",           powFloatOpB)
    , ("**",           powFloatOpB)
    -- Phase 2.10a: concurrency primitives.  forkIO is backed by GHC's
    -- RTS thread primitive (`fork#` in GHC.Internal.Conc.Sync), so the
    -- module-qualified names forward to the same host operation as the bare
    -- builtin instead of interpreting the low-level RTS wrapper source.
    , ("forkIO",          forkIOB)
    , ("Control.Concurrent.forkIO", forkIOB)
    , ("GHC.Conc.Sync.forkIO", forkIOB)
    , ("GHC.Internal.Conc.Sync.forkIO", forkIOB)
    -- Compiler-intrinsic 'fork#' primop. ghc-prim has no .hs source;
    -- forkIO and warp's defaultFork bottom out into this.
    , ("fork#",           forkHashB)
    , ("GHC.Prim.fork#",  forkHashB)
    , ("killThread",      killThreadB)
    , ("myThreadId",      myThreadIdB)
    , ("myThreadId#",     myThreadIdHashB)
    , ("fromThreadId",    fromThreadIdB)
    , ("GHC.Conc.Sync.fromThreadId", fromThreadIdB)
    , ("GHC.Internal.Conc.Sync.fromThreadId", fromThreadIdB)
    , ("labelThread",     labelThreadB)
    , ("GHC.Conc.Sync.labelThread", labelThreadB)
    , ("GHC.Internal.Conc.Sync.labelThread", labelThreadB)
    , ("labelThreadByteArray#", labelThreadByteArrayHashB)
    , ("GHC.Conc.Sync.labelThreadByteArray#", labelThreadByteArrayHashB)
    , ("GHC.Internal.Conc.Sync.labelThreadByteArray#", labelThreadByteArrayHashB)
    , ("threadDelay",     threadDelayB)
    , ("getNumCapabilities", getNumCapabilitiesB)
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
    -- IHC does not run GHC's RTS event manager.  Source modules such as
    -- auto-update branch on this probe; returning Nothing selects their
    -- ordinary threadDelay/forkIO implementation instead of the event-manager
    -- backend.
    , ("getSystemEventManager", getSystemEventManagerB)
    , ("GHC.Event.getSystemEventManager", getSystemEventManagerB)
    , ("GHC.Event.Thread.getSystemEventManager", getSystemEventManagerB)
    , ("GHC.Internal.Event.getSystemEventManager", getSystemEventManagerB)
    , ("GHC.Internal.Event.Thread.getSystemEventManager", getSystemEventManagerB)
    -- IHC also does not run GHC's RTS timer manager.  Warp/time-manager use
    -- these operations only to register idle timeout callbacks; expose a
    -- no-op timer manager so request handling can proceed without evaluating
    -- the RTS-only TimerManager implementation.
    , ("getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Event.getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Event.Thread.getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Internal.Event.getSystemTimerManager", getSystemTimerManagerB)
    , ("GHC.Internal.Event.Thread.getSystemTimerManager", getSystemTimerManagerB)
    , ("registerTimeout", registerTimeoutB)
    , ("GHC.Event.registerTimeout", registerTimeoutB)
    , ("GHC.Event.TimerManager.registerTimeout", registerTimeoutB)
    , ("GHC.Internal.Event.registerTimeout", registerTimeoutB)
    , ("GHC.Internal.Event.TimerManager.registerTimeout", registerTimeoutB)
    , ("unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Event.unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Event.TimerManager.unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Internal.Event.unregisterTimeout", unregisterTimeoutB)
    , ("GHC.Internal.Event.TimerManager.unregisterTimeout", unregisterTimeoutB)
    , ("updateTimeout", updateTimeoutB)
    , ("GHC.Event.updateTimeout", updateTimeoutB)
    , ("GHC.Event.TimerManager.updateTimeout", updateTimeoutB)
    , ("GHC.Internal.Event.updateTimeout", updateTimeoutB)
    , ("GHC.Internal.Event.TimerManager.updateTimeout", updateTimeoutB)
    , ("withHandle", timeManagerWithHandleB)
    , ("withHandleKillThread", timeManagerWithHandleB)
    , ("System.TimeManager.withHandle", timeManagerWithHandleB)
    , ("System.TimeManager.withHandleKillThread", timeManagerWithHandleB)
    -- @System.TimeManager.initialize@ / @stopManager@ are RTS-tied helpers
    -- (their upstream definitions just box / unbox a 'Manager Int' since the
    -- timeout work itself is delegated to the GHC RTS timer manager).  IHC
    -- skips that backend entirely, so the bare-name and qualified bindings
    -- both resolve to no-op host shims that keep warp's @withII@ happy.
    , ("initialize", timeManagerInitializeB)
    , ("System.TimeManager.initialize", timeManagerInitializeB)
    , ("stopManager", timeManagerStopManagerB)
    , ("System.TimeManager.stopManager", timeManagerStopManagerB)
    -- System.Posix.IO comes from the boot `unix` package, whose source is
    -- not present in ~/.cache/ihc/sources for this run.  setFdOption mutates
    -- an OS file descriptor flag; expose the host operation so Warp can mark
    -- accepted/listening sockets CloseOnExec during startup.
    , ("setFdOption", setFdOptionB)
    , ("System.Posix.IO.setFdOption", setFdOptionB)
    -- Phase 2.10a: MVar
    --
    -- Source-loaded MVar operations (GHC.Internal.MVar.takeMVar etc.)
    -- pattern-match @(MVar mvar#)@ against the value, which fails
    -- for our 'VPrimObj (PrimMVar _)' shape — the pattern-match
    -- failure then bubbles up through the user's catch chain and
    -- silently kills the worker.  Bind every FQN that source code
    -- might resolve to so our host primitives win.
    , ("newMVar",         newMVarB)
    , ("Control.Concurrent.MVar.newMVar", newMVarB)
    , ("GHC.MVar.newMVar", newMVarB)
    , ("GHC.Internal.MVar.newMVar", newMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.newMVar", newMVarB)
    , ("newEmptyMVar",    newEmptyMVarB)
    , ("Control.Concurrent.MVar.newEmptyMVar", newEmptyMVarB)
    , ("GHC.MVar.newEmptyMVar", newEmptyMVarB)
    , ("GHC.Internal.MVar.newEmptyMVar", newEmptyMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.newEmptyMVar", newEmptyMVarB)
    , ("takeMVar",        takeMVarB)
    , ("Control.Concurrent.MVar.takeMVar", takeMVarB)
    , ("GHC.MVar.takeMVar", takeMVarB)
    , ("GHC.Internal.MVar.takeMVar", takeMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.takeMVar", takeMVarB)
    , ("putMVar",         putMVarB)
    , ("Control.Concurrent.MVar.putMVar", putMVarB)
    , ("GHC.MVar.putMVar", putMVarB)
    , ("GHC.Internal.MVar.putMVar", putMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.putMVar", putMVarB)
    , ("readMVar",        readMVarB)
    , ("Control.Concurrent.MVar.readMVar", readMVarB)
    , ("GHC.MVar.readMVar", readMVarB)
    , ("GHC.Internal.MVar.readMVar", readMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.readMVar", readMVarB)
    , ("modifyMVar_",     modifyMVar_B)
    , ("Control.Concurrent.MVar.modifyMVar_", modifyMVar_B)
    , ("modifyMVar",      modifyMVarB)
    , ("Control.Concurrent.MVar.modifyMVar", modifyMVarB)
    , ("tryTakeMVar",     tryTakeMVarB)
    , ("Control.Concurrent.MVar.tryTakeMVar", tryTakeMVarB)
    , ("GHC.MVar.tryTakeMVar", tryTakeMVarB)
    , ("GHC.Internal.MVar.tryTakeMVar", tryTakeMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.tryTakeMVar", tryTakeMVarB)
    , ("tryPutMVar",      tryPutMVarB)
    , ("Control.Concurrent.MVar.tryPutMVar", tryPutMVarB)
    , ("GHC.MVar.tryPutMVar", tryPutMVarB)
    , ("GHC.Internal.MVar.tryPutMVar", tryPutMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.tryPutMVar", tryPutMVarB)
    , ("isEmptyMVar",     isEmptyMVarB)
    , ("Control.Concurrent.MVar.isEmptyMVar", isEmptyMVarB)
    , ("GHC.MVar.isEmptyMVar", isEmptyMVarB)
    , ("GHC.Internal.MVar.isEmptyMVar", isEmptyMVarB)
    , ("GHC.Internal.Control.Concurrent.MVar.isEmptyMVar", isEmptyMVarB)
    , ("withMVar",        withMVarB)
    , ("Control.Concurrent.MVar.withMVar", withMVarB)
    , ("swapMVar",        swapMVarB)
    , ("Control.Concurrent.MVar.swapMVar", swapMVarB)
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
    -- With Val-level dynamic typing we cannot implement the type match;
    -- we always return Just, so the user handler sees the raw exception Val.
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
    , ("catch",           catchB)
    , ("GHC.IO.catch",    catchB)
    , ("GHC.Internal.IO.catch", catchB)
    , ("Control.Exception.catch", catchB)
    , ("GHC.Internal.Control.Exception.catch", catchB)
    , ("handle",          handleB)
    , ("Control.Exception.handle", handleB)
    , ("GHC.Internal.Control.Exception.handle", handleB)
    , ("try",             tryB)
    , ("Control.Exception.try", tryB)
    , ("Control.Exception.Base.try", tryB)
    , ("GHC.Internal.Control.Exception.try", tryB)
    , ("GHC.Internal.Control.Exception.Base.try", tryB)
    , ("evaluate",        evaluateB)
    , ("Control.Exception.evaluate", evaluateB)
    , ("GHC.Internal.Control.Exception.evaluate", evaluateB)
    , ("mask_",           mask_B)
    , ("GHC.IO.mask_",    mask_B)
    , ("GHC.Internal.IO.mask_", mask_B)
    , ("Control.Exception.mask_", mask_B)
    , ("GHC.Internal.Control.Exception.mask_", mask_B)
    , ("mask",            maskB)
    , ("GHC.IO.mask",     maskB)
    , ("GHC.Internal.IO.mask", maskB)
    , ("Control.Exception.mask", maskB)
    , ("GHC.Internal.Control.Exception.mask", maskB)
    , ("uninterruptibleMask_", mask_B)
    , ("uninterruptibleMask", maskB)
    , ("GHC.IO.uninterruptibleMask_", mask_B)
    , ("GHC.IO.uninterruptibleMask", maskB)
    , ("GHC.Internal.IO.uninterruptibleMask_", mask_B)
    , ("GHC.Internal.IO.uninterruptibleMask", maskB)
    , ("Control.Exception.uninterruptibleMask_", mask_B)
    , ("Control.Exception.uninterruptibleMask", maskB)
    , ("GHC.Internal.Control.Exception.uninterruptibleMask_", mask_B)
    , ("GHC.Internal.Control.Exception.uninterruptibleMask", maskB)
    , ("block",           mask_B)
    , ("GHC.IO.block",    mask_B)
    , ("GHC.Internal.IO.block", mask_B)
    , ("unblock",         mask_B)
    , ("GHC.IO.unblock",  mask_B)
    , ("GHC.Internal.IO.unblock", mask_B)
    , ("unsafeUnmask",    mask_B)
    , ("GHC.IO.unsafeUnmask", mask_B)
    , ("GHC.Internal.IO.unsafeUnmask", mask_B)
    , ("allowInterrupt", allowInterruptB)
    , ("Control.Exception.allowInterrupt", allowInterruptB)
    , ("GHC.Internal.Control.Exception.allowInterrupt", allowInterruptB)
    , ("interruptible", interruptibleB)
    , ("Control.Exception.interruptible", interruptibleB)
    , ("GHC.Internal.Control.Exception.interruptible", interruptibleB)
    , ("GHC.IO.interruptible", interruptibleB)
    , ("GHC.Internal.IO.interruptible", interruptibleB)
    , ("bracket",         bracketB)
    , ("Control.Exception.bracket", bracketB)
    , ("GHC.Internal.Control.Exception.bracket", bracketB)
    , ("bracketOnError",  bracketOnErrorB)
    , ("Control.Exception.bracketOnError", bracketOnErrorB)
    , ("GHC.Internal.Control.Exception.bracketOnError", bracketOnErrorB)
    , ("bracket_",        bracket_B)
    , ("Control.Exception.bracket_", bracket_B)
    , ("GHC.Internal.Control.Exception.bracket_", bracket_B)
    , ("finally",         finallyB)
    , ("Control.Exception.finally", finallyB)
    , ("GHC.Internal.Control.Exception.finally", finallyB)
    , ("onException",     onExceptionB)
    , ("Control.Exception.onException", onExceptionB)
    , ("GHC.Internal.Control.Exception.onException", onExceptionB)
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
    -- Weak# primops.  Weak pointers are RTS objects; source modules only
    -- wrap these operations in ordinary newtypes.
    , ("mkWeak#",               mkWeakHashB)
    , ("mkWeakNoFinalizer#",    mkWeakNoFinalizerHashB)
    -- Phase 2.11: TH Lift builtins.
    ] ++ thBuiltinPairs

--------------------------------------------------------------------------------
-- Builders
--------------------------------------------------------------------------------

binOpInt :: (Int64 -> Int64 -> Int64) -> IO Val
binOpInt op = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> pure (VInt (op x y))
        _ -> error ("binOp: non-Int args: "
                    <> showValForDebug av <> ", " <> showValForDebug bv)

-- | Float-only unary op.
unaryOpFloat :: (Double -> Double) -> IO Val
unaryOpFloat op = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VFloat (op d))
        VInt n   -> pure (VFloat (op (fromIntegral n)))
        _ -> error ("unaryOpFloat: non-numeric arg: " <> showValForDebug av)

-- cmpInt removed in Phase 2.3 — replaced by eqDispatch/ordDispatch

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

-- | Extract a host IORef from a 'VPrimObj' or fail with a
-- context-tagged error. Used by readIORef / writeIORef /
-- modifyIORef / atomicModifyIORef'.
extractIORef :: String -> Val -> IO (IORef Thunk)
extractIORef _   (VPrimObj (PrimIORef rf)) = pure rf
extractIORef ctx v = error (ctx <> ": not an IORef: " <> showValForDebug v)

-- | Coerce a 'Val' (either 'VInt' or 'VChar') to a host 'Word8'.
-- Used by ByteString primops that accept either a numeric literal
-- or a character literal.
valToWord8 :: String -> Val -> IO Word8
valToWord8 _   (VInt i)  = pure (fromIntegral i)
valToWord8 _   (VChar c) = pure (fromIntegral (fromEnum c))
valToWord8 ctx v         = error (ctx <> ": not a Word8: " <> showValForDebug v)

-- | Test for truthy value: VCon "True"/VInt non-zero is True.
isTruthy :: Val -> Bool
isTruthy (VCon "True" _)  = True
isTruthy (VCon "False" _) = False
isTruthy (VInt 0)         = False
isTruthy (VInt _)         = True
isTruthy other = error ("isTruthy: not a Bool: " <> showValForDebug other)

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
    av <- force legacyHooks a
    bv <- force legacyHooks b
    eqVals reg av bv

-- | /= dispatch.
neqDispatch :: ClassRegistry -> IO Val
neqDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
    r <- eqVals reg av bv
    pure (boolVal (not (isTruthy r)))

-- | Core equality test on WHNF values.
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
    isPlaceholder (VCon n []) = n == BC.pack "<ihc-method-placeholder>"
    isPlaceholder _           = False

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
    -- Data.ByteString shim (see isBuiltinBackedModule): compare by
    -- content via the host Eq ByteString, not by ForeignPtr identity.
    -- Default VCon field-by-field would compare fp1 == fp2 which is
    -- always False for freshly-allocated buffers with the same bytes.
    (VCon "BS" _, VCon "BS" _) -> do
        ba <- bsValToBS av
        bb <- bsValToBS bv
        pure (boolVal (ba == bb))
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
    _ -> do
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

-- | Ord dispatch. Slot in the method list:
--   0 = (<), 1 = (<=), 2 = (>), 3 = (>=), 4 = compare
-- We implement all four directly for builtin types and use
-- registry lookup for user-defined types.
ordDispatch :: ClassRegistry -> Int -> IO Val
ordDispatch reg slot = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
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
    -- Data.ByteString shim (see eqVals): compare by content via the
    -- host Ord, not structural VCon-field compare (which compares
    -- ForeignPtr addresses and gives meaningless results).
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
    -- Slot 4 is unused at this level (compareDispatch calls slot 0 then
    -- consults Eq for EQ), but we handle it defensively.
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
-- 'structuralOrdering' is invoked through a long chain of Ord-dispatch
-- helpers (ordCmp, valOrdering, compareDispatch, …) and threading an
-- extra registry through every one of them for a purely-derived
-- fallback would touch far too many call sites. The ref is written
-- once per module load and read many times per comparison; races
-- aren't a concern because the scheduler only rebuilds the env
-- single-threaded.
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
clearForeignPtrWord8Ranges = writeIORef foreignPtrWord8RangesRef []

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
-- 'Ordering'. Used by 'structuralOrdering' and 'compareDispatch' so
-- every code path shares the same ordering logic.
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

-- | @min x y@ dispatched through 'valOrdering': returns @x@ when
-- @compare x y /= GT@, otherwise @y@. Works for every type 'ordCmp'
-- supports (numeric, primitive, and any VCon via the structural
-- fallback), not just numbers.
minDispatch :: ClassRegistry -> IO Val
minDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
    o  <- valOrdering reg av bv
    case o of
        GT -> pure bv
        _  -> pure av

-- | @max x y@ dispatched through 'valOrdering'. Counterpart to
-- 'minDispatch'.
maxDispatch :: ClassRegistry -> IO Val
maxDispatch reg = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a
    bv <- force legacyHooks b
    o  <- valOrdering reg av bv
    case o of
        LT -> pure bv
        _  -> pure av

-- | Show dispatch: look up "show" in the Show class registry.
-- Slot 0 = show. Falls back to built-in showVal for base types.
showDispatch :: ClassRegistry -> IO Val
showDispatch reg = pure $ VFun $ \a -> do
    av <- force legacyHooks a
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
    VCon "Proxy" _ -> pure "Proxy"   -- DataKinds payload is invisible in show
    VCon "BS" _ -> do
        -- Render a ByteString using Data.ByteString.Char8's show-style
        -- output: `"..."` with printable ASCII passed through and
        -- non-printables escaped. Matches the GHC stock instance.
        bs <- bsValToBS av
        pure (show bs)
    VCon n _ | isTupleConName n -> showVal av
    VCon n _ -> do
        let tag = n
        mShowMethod <- lookupInstanceMethod reg "Show" tag "show" >>= forceInstanceMethod
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
showVal (VClassMethod _ _ _ _) = pure "<function>"
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
                        VClassMethod _ _ _ _ -> do { aT <- newWHNFThunk av; apply legacyHooks fromLabelM aT }
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
-- (`print x = putStrLn (show x)`).  The Show-class entry point lives
-- in 'showDispatch reg' below; demand discovery on 'print x' resolves
-- 'show' to that dispatcher, which walks the ClassRegistry as before.

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

errorB :: IO Val
errorB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    s  <- valToString av
    hPutStrLn stderr ("ihc error called: " <> s)
    hFlush stderr
    error ("ihc: " <> s)

undefinedB :: IO Val
undefinedB = pure (VIO (error "Prelude.undefined"))

--------------------------------------------------------------------------------
-- Monad core. Every builtin here is a plain global binding so Phase
-- 2.3 class-dispatch can later overlay it with dictionary-threaded
-- versions. The IO monad appears either as host-backed 'VIO' or as the
-- source-loaded @IO@ newtype, depending on how far evaluation has gone.
--------------------------------------------------------------------------------

-- | @return x = VIO (pure x)@. The @x@ thunk is not forced until the
-- receiver runs the action (preserving Haskell laziness).
returnB :: IO Val
returnB = pure $ VFun $ \a -> pure (VIO (force legacyHooks a))

-- | Class-dispatched @>>=@.  For 'VIO' values (the common IO case) this
-- is the same as the old 'bindB'.  For ST computations (VCon "ST" _), the
-- ST monad bind is implemented directly: build a new ST computation that
-- sequences the two.  For other values the 'Monad' instance in the class
-- registry is consulted.
bindDispatch :: ClassRegistry -> IO Val
bindDispatch reg = pure $ VFun $ \ma -> pure $ VFun $ \kt -> do
    mv <- force legacyHooks ma
    case mv of
        VIO _        -> ioBind mv kt
        VCon "IO" [_] -> ioBind mv kt
        -- ST monad bind:
        -- (ST m) >>= k = ST (\s -> case m s of { (# s', r #) -> case k r of { ST k2 -> k2 s' }})
        -- Note: primop state functions may return VIO actions; run them with runIOVal.
        -- If k r returns VIO (from `return`/`pure` via the IO-backed builtin),
        -- we run it eagerly and wrap the result in a new (# s', a #) tuple --
        -- the ST ≈ IO bridge (CLAUDE.md: ST is compiler-intrinsic, IO machinery reused).
        VCon "ST" [mFuncT] -> do
            mFuncV <- force legacyHooks mFuncT
            ktV    <- force legacyHooks kt
            stFunc <- newWHNFThunk $ VFun $ \sT -> do
                resRaw <- apply legacyHooks mFuncV sT    -- apply :: Val -> Thunk -> IO Val
                resV   <- runIOVal legacyHooks resRaw    -- run VIO if needed (primop results)
                case stResultComponents resV of
                    Just (newST, rT) -> do
                        stkRaw <- apply legacyHooks ktV rT   -- k applied to r; may be ST or VIO
                        -- DO NOT runIOVal here unconditionally: that would strip a
                        -- `VIO (pure x)` (from `return`/`pure`) down to a bare x and
                        -- lose the state-threading shape. Branch on stkRaw first.
                        case stkRaw of
                            VCon "ST" [k2FuncT] -> do
                                k2FuncV  <- force legacyHooks k2FuncT
                                resRaw2  <- apply legacyHooks k2FuncV newST
                                runIOVal legacyHooks resRaw2
                            -- VIO from `return`/`pure` in ST: run it, wrap as (# s, a #)
                            VIO io -> do
                                r  <- io
                                rT <- newWHNFThunk r
                                pure (VCon "(#,#)" [newST, rT])
                            other -> do
                                -- Run any outstanding VIO layers; wrap as (# s, a #).
                                -- This keeps `stkRaw = VIO (VIO ...)` and other
                                -- deeply-wrapped results compatible with the ST shape.
                                v  <- runIOVal legacyHooks other
                                case v of
                                    VCon "ST" [k2FuncT] -> do
                                        k2FuncV <- force legacyHooks k2FuncT
                                        resRaw2 <- apply legacyHooks k2FuncV newST
                                        runIOVal legacyHooks resRaw2
                                    _ -> do
                                        vT <- newWHNFThunk v
                                        pure (VCon "(#,#)" [newST, vT])
                    Nothing -> pure resV  -- m didn't return a state/result pair
            pure (VCon "ST" [stFunc])
        _ -> do
            -- Look up Monad instance for this value's type tag.
            let tag = typeTagOf mv
            mBindMethod <- lookupInstanceMethod reg "Monad" tag ">>=" >>= forceInstanceMethod
            case mBindMethod of
                Just bindMethod -> do
                    maT <- newWHNFThunk mv
                    r1  <- apply legacyHooks bindMethod maT
                    apply legacyHooks r1 kt
                _ ->
                    -- No instance found; fall back to IO bind (will error at
                    -- runtime if mv is not VIO, but gives a sensible message).
                    pure $ VIO $ do
                        v  <- runIOVal legacyHooks mv
                        kv <- force legacyHooks kt
                        vT <- newWHNFThunk v
                        r  <- apply legacyHooks kv vT
                        runIOVal legacyHooks r
  where
    ioBind mv kt = pure $ VIO $ do
        v  <- runIOVal legacyHooks mv
        kv <- force legacyHooks kt
        vT <- newWHNFThunk v
        r  <- apply legacyHooks kv vT
        runIOVal legacyHooks r

-- | @m >> n@ = run m (discarding result), then run n.
seqDispatch :: ClassRegistry -> IO Val
seqDispatch reg = pure $ VFun $ \ma -> pure $ VFun $ \mb -> do
    mv <- force legacyHooks ma
    case mv of
        VIO _         -> ioSeq mv mb
        VCon "IO" [_] -> ioSeq mv mb
        -- ST monad seq:
        -- m >> n = m >>= \_ -> n  for ST
        -- Note: primop state functions may return VIO actions; run them with runIOVal.
        -- If n is VIO (from `return`/`pure`), we run it and wrap as (# s', a #).
        VCon "ST" [mFuncT] -> do
            mFuncV <- force legacyHooks mFuncT
            stFunc <- newWHNFThunk $ VFun $ \sT -> do
                resRaw <- apply legacyHooks mFuncV sT    -- apply :: Val -> Thunk -> IO Val
                resV   <- runIOVal legacyHooks resRaw    -- run VIO if needed
                case stResultComponents resV of
                    Just (newST, _) -> do
                        nbV <- force legacyHooks mb
                        case nbV of
                            VCon "ST" [k2FuncT] -> do
                                k2FuncV  <- force legacyHooks k2FuncT
                                resRaw2  <- apply legacyHooks k2FuncV newST
                                runIOVal legacyHooks resRaw2
                            -- VIO from `return`/`pure`: run and wrap as (# s, a #)
                            VIO io -> do
                                r  <- io
                                rT <- newWHNFThunk r
                                pure (VCon "(#,#)" [newST, rT])
                            other -> do
                                v <- runIOVal legacyHooks other
                                case v of
                                    VCon "ST" [k2FuncT] -> do
                                        k2FuncV <- force legacyHooks k2FuncT
                                        resRaw2 <- apply legacyHooks k2FuncV newST
                                        runIOVal legacyHooks resRaw2
                                    _ -> do
                                        vT <- newWHNFThunk v
                                        pure (VCon "(#,#)" [newST, vT])
                    Nothing -> pure resV
            pure (VCon "ST" [stFunc])
        _ -> do
            let tag = typeTagOf mv
            mSeqMethod <- lookupInstanceMethod reg "Monad" tag ">>" >>= forceInstanceMethod
            case mSeqMethod of
                Just seqMethod -> do
                    maT <- newWHNFThunk mv
                    r1  <- apply legacyHooks seqMethod maT
                    apply legacyHooks r1 mb
                _ ->
                    pure $ VIO $ do
                        _ <- runIOVal legacyHooks mv
                        nv <- force legacyHooks mb
                        runIOVal legacyHooks nv
  where
    ioSeq mv mb = pure $ VIO $ do
        _ <- runIOVal legacyHooks mv
        nv <- force legacyHooks mb
        runIOVal legacyHooks nv

-- Strict ST state functions produce unboxed tuples in state-first order,
-- while lazy ST produces boxed pairs in value-first order. Constructor names
-- are not type-qualified in the interpreter, so ST bind must bridge both.
stResultComponents :: Val -> Maybe (Thunk, Thunk)
stResultComponents (VCon "(#,#)" [stateT, valueT]) = Just (stateT, valueT)
stResultComponents (VCon "(,)" [valueT, stateT])   = Just (stateT, valueT)
stResultComponents _                               = Nothing

-- | Dispatching @fmap@. Forces the container argument and looks up a
-- @(Functor, typeTagOf mv)@ entry in the 'ClassRegistry'. If one is
-- registered (either hand-written or synthesised from a @deriving
-- Functor@ clause), that instance's @fmap@ is applied. Otherwise we
-- fall back to the @VIO@-only behaviour of 'fmapB' — so existing IO
-- uses keep working, and a @fmap@ on a container type that truly has
-- no registered instance still produces a runtime error from the IO
-- path rather than silently misbehaving.
fmapDispatch :: ClassRegistry -> IO Val
fmapDispatch reg = pure $ VFun $ \ft -> pure $ VFun $ \mt -> do
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

-- | Dispatching @<>@ — 'Semigroup' append.  Forces the LHS, picks an
-- instance based on its type tag, and applies @<>@ to both args.
-- Handles the host-string case ('VStr') with direct concatenation
-- since IHC sometimes carries strings as 'VStr' rather than as a
-- @VCon ":"@ chain (see 'charListToByteStringVal').
sappendDispatch :: ClassRegistry -> IO Val
sappendDispatch reg = pure $ VFun $ \xT -> pure $ VFun $ \yT -> do
    xv <- force legacyHooks xT
    case xv of
        -- Direct fast path for cons-list strings: avoid round-tripping
        -- through Semigroup [] which is registered but cycles back here.
        VCon ":" _ -> do
            yv <- force legacyHooks yT
            consAppend xv yv
        VCon "[]" [] -> force legacyHooks yT
        VStr _ -> do
            yv <- force legacyHooks yT
            consAppend xv yv
        _ -> do
            let tag = typeTagOf xv
            mMethod <- lookupInstanceMethod reg
                          (BC.pack "Semigroup") tag (BC.pack "<>")
                          >>= forceInstanceMethod
            case mMethod of
                Just method -> do
                    xT' <- newWHNFThunk xv
                    r1 <- apply legacyHooks method xT'
                    apply legacyHooks r1 yT
                Nothing -> error
                    ( "<>: no Semigroup instance registered for type `"
                   <> BC.unpack tag <> "`" )
  where
    -- Cons-list / VStr concatenation.  Forces both as cons-lists.
    consAppend (VCon ":" [hT, tT]) ys = do
        tv <- force legacyHooks tT
        rest <- consAppend tv ys
        restT <- newWHNFThunk rest
        pure (VCon ":" [hT, restT])
    consAppend (VCon "[]" []) ys = pure ys
    consAppend (VStr s) ys = do
        -- Promote VStr to a cons-list so we can append onto it.
        consList <- stringToListValIO (BC.unpack s)
        consAppend consList ys
    consAppend other _ =
        error ("<>: unexpected list shape: " <> showValForDebug other)


-- | Dispatching @<*>@. Forces the first argument and looks up an
-- @(Applicative, typeTagOf fv)@ entry in the 'ClassRegistry'. If the
-- first argument's tag has no Applicative instance (e.g. @Nothing@'s
-- "Nothing" tag may not be registered while only "Just" is), we also try
-- the second argument's tag — for the same Applicative both sides share
-- the type but distinct constructors. Falls back to a VIO-only inline
-- implementation so existing IO uses keep working.
apDispatch :: ClassRegistry -> IO Val
apDispatch reg = pure $ VFun $ \ft -> pure $ VFun $ \mt -> do
    fv <- force legacyHooks ft
    let tryTag tag = lookupInstanceMethod reg (BC.pack "Applicative") tag (BC.pack "<*>")
                       >>= forceInstanceMethod
    mApMethod <- tryTag (typeTagOf fv)
    mApMethod2 <- case mApMethod of
        Just _  -> pure mApMethod
        Nothing -> do
            mv <- force legacyHooks mt
            tryTag (typeTagOf mv)
    case mApMethod2 of
        Just apMethod -> do
            fT <- newWHNFThunk fv
            r1 <- apply legacyHooks apMethod fT
            apply legacyHooks r1 mt
        Nothing -> case fv of
            VIO _ -> pure $ VIO $ do
                f1 <- runIOVal legacyHooks fv
                mv <- force legacyHooks mt
                v  <- runIOVal legacyHooks mv
                vT <- newWHNFThunk v
                apply legacyHooks f1 vT
            _ -> error
                ( "<*>: no Applicative instance registered for type `"
                  <> BC.unpack (typeTagOf fv) <> "`" )

-- | @join mm = do { m <- mm; m }@.
-- 'joinB' was removed in slice 5b — 'join' is now interpreted from
-- ~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Base.hs:1292-1293
-- ('join x = x >>= id'). See the comment next to the (now-deleted)
-- "join" entry in 'builtins' above.

-- 'voidB' was removed in slice 5a — 'void' is now interpreted from
-- ~/.cache/ihc/sources/base-4.19.0.0/Data/Functor.hs:210-211. See the
-- comment next to the (now-deleted) "void" entry in 'builtins' above.

-- | @first f (a, b) = (f a, b)@ — the @Arrow (->)@ instance method.
-- Warp uses @first ((,TCP) <$>)@ in 'runSettingsConnectionMaker'.
firstFnB :: IO Val
firstFnB = pure $ VFun $ \fT -> pure $ VFun $ \tupT -> do
    fv  <- force legacyHooks fT
    tupV <- force legacyHooks tupT
    case tupV of
        VCon "(,)" [aT, bT] -> do
            r  <- apply legacyHooks fv aT
            rT <- newWHNFThunk r
            pure (VCon "(,)" [rT, bT])
        _ -> error ("first: not a tuple: " <> showValForDebug tupV)

-- | @second g (a, b) = (a, g b)@ — counterpart to 'firstFnB'.
secondFnB :: IO Val
secondFnB = pure $ VFun $ \gT -> pure $ VFun $ \tupT -> do
    gv  <- force legacyHooks gT
    tupV <- force legacyHooks tupT
    case tupV of
        VCon "(,)" [aT, bT] -> do
            r  <- apply legacyHooks gv bT
            rT <- newWHNFThunk r
            pure (VCon "(,)" [aT, rT])
        _ -> error ("second: not a tuple: " <> showValForDebug tupV)

-- runIOVal lives in 'IHC.Eval' (and now also covers STM, which used to
-- be a separate copy here).  We import it from there.

--------------------------------------------------------------------------------
-- IORef primops. Each returns 'VIO' — construction, read, and write
-- are all IO actions.
--------------------------------------------------------------------------------

newIORefB :: IO Val
newIORefB = pure $ VFun $ \a -> pure $ VIO $ do
    -- Store the THUNK directly so error-throwing initialisers
    -- (e.g. warp's @newIORef $ error "keepAliveRef not filled"@) are
    -- only evaluated on read.
    rf <- newIORef a
    pure (VPrimObj (PrimIORef rf))

readIORefB :: IO Val
readIORefB = pure $ VFun $ \a -> pure $ VIO $ do
    rf <- force legacyHooks a >>= extractIORef "readIORef"
    readIORef rf >>= force legacyHooks

writeIORefB :: IO Val
writeIORefB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    rf <- force legacyHooks a >>= extractIORef "writeIORef"
    -- Install the thunk directly; readers force on demand.
    writeIORef rf b
    pure VUnit

-- | @modifyIORef ref f@. We force f then apply it to a thunk holding
-- the current ref contents. Works for both the lazy and strict forms
-- (Phase 2.4 does not differentiate beyond that).
modifyIORefB :: IO Val
modifyIORefB = pure $ VFun $ \a -> pure $ VFun $ \f -> pure $ VIO $ do
    rf <- force legacyHooks a >>= extractIORef "modifyIORef"
    fv <- force legacyHooks f
    curT <- readIORef rf
    new <- apply legacyHooks fv curT
    newT <- newWHNFThunk new
    writeIORef rf newT
    pure VUnit

-- | @atomicModifyIORef' ref f@ for ihc's host-backed IORef.  This mirrors
-- the existing IORef builtins and is atomic enough for ihc's single-threaded
-- evaluator: read the current value, apply @f@ to get @(new, result)@, store
-- @new@, and return @result@.
atomicModifyIORefB :: IO Val
atomicModifyIORefB = pure $ VFun $ \a -> pure $ VFun $ \f -> pure $ VIO $ do
    rf <- force legacyHooks a >>= extractIORef "atomicModifyIORef'"
    fv <- force legacyHooks f
    curT <- readIORef rf
    pair <- apply legacyHooks fv curT >>= runIOVal legacyHooks
    case pair of
        VCon _ [newT, resultT] -> do
            result <- force legacyHooks resultT
            writeIORef rf newT
            pure result
        _ -> error ("atomicModifyIORef': function did not return a pair: "
                    <> showValForDebug pair)

mkWeakIORefB :: IO Val
mkWeakIORefB = pure $ VFun $ \refT -> pure $ VFun $ \_finalizerT -> pure $ VIO $ do
    refV <- force legacyHooks refT
    weakPayload <- newWHNFThunk refV
    pure (VCon "Weak" [weakPayload])

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

mkWeakHashB :: IO Val
mkWeakHashB = pure $ VFun $ \_keyT -> pure $ VFun $ \valT -> pure $ VFun $ \_finalizerT -> pure $ VFun $ \_stT -> do
    valV <- force legacyHooks valT
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    weakT <- newWHNFThunk valV
    pure (VCon "(#,#)" [stT, weakT])

mkWeakNoFinalizerHashB :: IO Val
mkWeakNoFinalizerHashB = pure $ VFun $ \_keyT -> pure $ VFun $ \valT -> pure $ VFun $ \_stT -> do
    valV <- force legacyHooks valT
    stT <- newWHNFThunk (VPrimObj PrimRealWorld)
    weakT <- newWHNFThunk valV
    pure (VCon "(#,#)" [stT, weakT])

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
    pv  <- force legacyHooks a
    path <- valToString pv
    mv  <- force legacyHooks b
    let mode = ioModeFromVal mv
    h <- openFile path mode
    pure (VPrimObj (PrimHandle h))

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

--------------------------------------------------------------------------------
-- Control flow.
--------------------------------------------------------------------------------

-- | @seq a b@: force @a@ to WHNF, then return @b@.
seqB :: IO Val
seqB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    _ <- force legacyHooks a
    force legacyHooks b

-- | @assert cond x = x@.  Like GHC's default (assertions ignored) behaviour.
assertB :: IO Val
assertB = pure $ VFun $ \_cond -> pure $ VFun $ \x -> force legacyHooks x

-- | @exitWith code@: throws 'ExitCode'. Wrapped in VIO so it's delayed.
exitWithB :: IO Val
exitWithB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force legacyHooks a
    case av of
        VCon "ExitSuccess" _ -> throwIO ExitSuccess
        VCon "ExitFailure" [nT] -> do
            nv <- force legacyHooks nT
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

-- | 'fromIntegral' / 'fromInteger' coercion. Accepts Int or Float/Double;
-- returns the value unchanged (we have one Int type and one Float type).
-- | 'maxBound' / 'minBound' — class methods of Bounded.  Nullary, so our
-- arg-directed dispatcher can't pick an instance without a type hint.
-- Default to Int bounds, which is what most real-world code wants
-- (`maxBound :: Int` shows up in text's `length` and many others).
-- Code that explicitly asks for `maxBound :: Word8` via @TypeApplications@
-- is handled separately by the @VClassMethod + @T@ path.
maxBoundB :: IO Val
maxBoundB = pure (VInt maxBound)

minBoundB :: IO Val
minBoundB = pure (VInt minBound)

fromIntegralB :: IO Val
fromIntegralB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n   -> pure (VInt n)
        VFloat d -> pure (VFloat d)
        VChar c  -> pure (VInt (fromIntegral (ord c)))
        -- Newtype numeric wrappers we handle by name so we don't eat
        -- every single-field constructor (ST, Identity, Maybe-Just, …).
        VCon c [t]
          | c `elem` numericNewtypeCons -> do
              inner <- force legacyHooks t
              case inner of
                  VInt n   -> pure (VInt n)
                  VFloat d -> pure (VFloat d)
                  _ -> error ("fromIntegral: not a numeric value: " <> showValForDebug av)
        _ -> error ("fromIntegral: not a numeric value: " <> showValForDebug av)
  where
    numericNewtypeCons =
        [ "CSize", "CInt", "CLong", "CULong", "CUInt", "CChar", "CUChar"
        , "CShort", "CUShort", "CLLong", "CULLong"
        , "CSsize", "CSSize", "CIntPtr", "CUIntPtr", "CPtrdiff"
        , "Int8", "Int16", "Int32", "Int64"
        , "Word", "Word8", "Word16", "Word32", "Word64"
        , "CFloat", "CDouble"
        -- 'Integer' has a multi-ctor representation in @ghc-bignum@:
        --   data Integer = IS !Int# | IP !ByteArray# | IN !ByteArray#
        -- The 'IS' constructor (small Integer fitting in an Int) flows
        -- here when source-loaded numeric code constructs an Integer
        -- and warp/wai then runs it through 'fromIntegral'.  Treat it
        -- as the Int it wraps.
        , "IS"
        ]

--------------------------------------------------------------------------------
-- Phase 2.8: RealWorld / State primops
--------------------------------------------------------------------------------

realWorldB :: IO Val
realWorldB = pure (VPrimObj PrimRealWorld)

-- | noDuplicate# :: State# s -> State# s
-- No-op in the interpreter; in GHC RTS this prevents thunk duplication.
noDuplicateB :: IO Val
noDuplicateB = pure $ VFun $ \_ -> pure (VPrimObj PrimRealWorld)

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

lazyB :: IO Val
lazyB = pure $ VFun $ \a -> force legacyHooks a

--------------------------------------------------------------------------------
-- Phase 2.8: unsafePerformIO family
--------------------------------------------------------------------------------

unsafePerformIOB :: IO Val
unsafePerformIOB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    runIOVal legacyHooks av

--------------------------------------------------------------------------------
-- Phase 2.8: boxing/unboxing constructors
--------------------------------------------------------------------------------

iHashB :: IO Val
iHashB = pure $ VFun $ \a -> force legacyHooks a

wHashB :: IO Val
wHashB = pure $ VFun $ \a -> force legacyHooks a

w8HashB :: IO Val
w8HashB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (n .&. 0xff))
        _      -> force legacyHooks a

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

plusAddrB :: IO Val
plusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VInt n) ->
            pure (VPrimObj (PrimPtr (plusPtr p (fromIntegral n))))
        _ -> error ("plusAddr#: bad args: " <> showValForDebug av)

minusAddrB :: IO Val
minusAddrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VPrimObj (PrimPtr q)) ->
            pure (VInt (fromIntegral (p `minusPtr` q)))
        _ -> error ("minusAddr#: bad args: " <> showValForDebug av)

addr2IntB :: IO Val
addr2IntB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VPrimObj (PrimPtr p) ->
            pure (VInt (fromIntegral (FP.ptrToIntPtr p)))
        _ -> error ("addr2Int#: not a Ptr: " <> showValForDebug av)

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

--------------------------------------------------------------------------------
-- Phase 2.8: Ptr arithmetic
--------------------------------------------------------------------------------

plusPtrB :: IO Val
plusPtrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VInt n) ->
            pure (VPrimObj (PrimPtr (plusPtr p (fromIntegral n))))
        -- A ForeignPtr flowing through plusPtr must advance by @n@
        -- bytes just like a raw Ptr would; dropping @n@ silently
        -- breaks callers that derive subsequent reads off the
        -- offset pointer (memchr/memcmp/peek).
        (VPrimObj (PrimForeignPtr fp), VInt n) ->
            pure (VPrimObj (PrimForeignPtr (plusForeignPtr fp (fromIntegral n))))
        _ -> error ("plusPtr: bad args: " <> showValForDebug av)

minusPtrB :: IO Val
minusPtrB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VPrimObj (PrimPtr p), VPrimObj (PrimPtr q)) ->
            pure (VInt (fromIntegral (p `minusPtr` q)))
        _ -> error ("minusPtr: bad args: " <> showValForDebug av)

nullPtrB :: IO Val
nullPtrB = pure (VPrimObj (PrimPtr nullPtr))

castPtrB :: IO Val
castPtrB = pure $ VFun $ \a -> force legacyHooks a

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
-- Data.ByteString shims
--
-- Temporary short-circuits for Data.ByteString operations. Data.ByteString.hs
-- source-loads correctly but takes ~9 minutes to complete because discovery
-- of GHC.Internal.Show's transitive closure cascades through thousands of
-- bindings. See isBuiltinBackedModule comment. Remove once the perf fix
-- lands on Scheduler discovery.
--
-- A ByteString is represented at runtime as @VCon "BS" [ForeignPtr, length]@,
-- matching Data.ByteString.Internal.Type.ByteString's real constructor.
--------------------------------------------------------------------------------

-- | Build a fresh bytestring value with the given ForeignPtr and length.
mkBsVal :: ForeignPtr Word8 -> Int -> IO Val
mkBsVal fp len = do
    markWord8PtrRange (castPtr (unsafeForeignPtrToPtr fp)) len
    fpT  <- newWHNFThunk (VPrimObj (PrimForeignPtr fp))
    lenT <- newWHNFThunk (VInt (fromIntegral len))
    pure (VCon "BS" [fpT, lenT])

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

newUniqueB :: IO Val
newUniqueB = pure $ VIO $ do
    n <- atomicModifyIORef' uniqueCounterRef $ \x ->
        let x' = x + 1 in (x', x')
    nT <- newWHNFThunk (VInt n)
    pure (VCon "Unique" [nT])

hashUniqueB :: IO Val
hashUniqueB = pure $ VFun $ \uT -> do
    uv <- force legacyHooks uT
    case uv of
        VCon "Unique" [nT] -> force legacyHooks nT
        VInt n             -> pure (VInt n)
        other              -> error ("hashUnique: not Unique: " <> showValForDebug other)

bsUnpackB :: IO Val
bsUnpackB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    (fp, len) <- bsValPayload av
    ws <- withForeignPtr fp $ \ptr ->
        mapM (peekElemOff (castPtr ptr :: Ptr Word8)) [0 .. len - 1]
    wordsToConsList ws

-- | Extract the underlying BS ByteString from a 'VCon "BS"' payload.
bsValToBS :: Val -> IO BS.ByteString
bsValToBS v = do
    (fp, len) <- bsValPayload v
    withForeignPtr fp $ \ptr ->
        BS.packCStringLen (castPtr ptr, len)

-- | Build a 'VCon "BS"' from a host ByteString by copying into a fresh ForeignPtr.
bsFromBS :: BS.ByteString -> IO Val
bsFromBS bs = do
    let len = BS.length bs
    fp <- mallocForeignPtrBytes len
    withForeignPtr fp $ \dst -> BS.useAsCStringLen bs $ \(src, l) ->
        copyBytes (castPtr dst) (castPtr src) l
    mkBsVal fp len

bsAppendB :: IO Val
bsAppendB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    ba <- bsValToBS av; bb <- bsValToBS bv
    bsFromBS (BS.append ba bb)

bsConcatB :: IO Val
bsConcatB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    xs <- valToBsList av
    bsFromBS (BS.concat xs)
  where
    valToBsList (VCon "[]" _)       = pure []
    valToBsList (VCon ":" [hT, tT]) = do
        hv <- force legacyHooks hT
        h  <- bsValToBS hv
        tv <- force legacyHooks tT
        (h :) <$> valToBsList tv
    valToBsList other = error ("BS.concat: expected list of ByteString, got " <> showValForDebug other)

bsSingletonB :: IO Val
bsSingletonB = pure $ VFun $ \wT -> do
    w <- force legacyHooks wT >>= valToWord8 "BS.singleton"
    bsFromBS (BS.singleton w)

bsReplicateB :: IO Val
bsReplicateB = pure $ VFun $ \nT -> pure $ VFun $ \wT -> do
    nv <- force legacyHooks nT
    n <- case nv of
        VInt i -> pure (fromIntegral i :: Int)
        _      -> error ("BS.replicate: first arg not an Int: " <> showValForDebug nv)
    w <- force legacyHooks wT >>= valToWord8 "BS.replicate: second arg"
    bsFromBS (BS.replicate n w)

bsIndexB :: IO Val
bsIndexB = pure $ VFun $ \aT -> pure $ VFun $ \iT -> do
    av <- force legacyHooks aT; iv <- force legacyHooks iT
    i <- case iv of
        VInt n -> pure (fromIntegral n :: Int)
        _      -> error ("BS.index: not an Int: " <> showValForDebug iv)
    (fp, len) <- bsValPayload av
    if i < 0 || i >= len
        then error ("BS.index: out of bounds: " <> show i <> " vs length " <> show len)
        else do
            w <- withForeignPtr fp $ \ptr -> peekElemOff (castPtr ptr :: Ptr Word8) i
            pure (VInt (fromIntegral w))

-- | Data.ByteString.Char8.putStrLn — kept as a host shim because
-- source-loaded `Char8.hPutStrLn` calls Prelude `length` on a ByteString
-- (the `Prelude hiding (Foldable(..))` import isn't honored yet by the
-- interpreter's name resolution, so `length` doesn't get redirected to
-- `Data.ByteString.length`). Remove once that's fixed at the Scheduler.
bs8PutStrLnB :: IO Val
bs8PutStrLnB = pure $ VFun $ \a -> pure $ VIO $ do
    av <- force legacyHooks a
    bs <- bsValToBS av
    BC.putStrLn bs
    hFlush stdout
    pure VUnit

wordsToConsList :: [Word8] -> IO Val
wordsToConsList []     = pure (VCon "[]" [])
wordsToConsList (w:ws) = do
    hT <- newWHNFThunk (VInt (fromIntegral w))
    tV <- wordsToConsList ws
    tT <- newWHNFThunk tV
    pure (VCon ":" [hT, tT])

withForeignPtrB :: IO Val
withForeignPtrB = pure $ VFun $ \fpT -> pure $ VFun $ \fT -> pure $ VIO $ do
    fpv <- force legacyHooks fpT; fv <- force legacyHooks fT
    fp <- foreignPtrValToForeignPtr fpv
    markForeignPtrWord8 fp
    withForeignPtr fp $ \ptr -> do
        markWord8Ptr (castPtr ptr)
        pT <- newWHNFThunk (VPrimObj (PrimPtr (castPtr ptr)))
        rv <- apply legacyHooks fv pT
        runIOVal legacyHooks rv

plusForeignPtrB :: IO Val
plusForeignPtrB = pure $ VFun $ \fpT -> pure $ VFun $ \nT -> do
    fpv <- force legacyHooks fpT; nv <- force legacyHooks nT
    case (fpv, nv) of
        -- ForeignPtr is RTS-backed and has no pure-Haskell storage model in IHC,
        -- so this builtin must preserve the host pointer offset exactly.
        (_, VInt n) -> do
            fp <- foreignPtrValToForeignPtr fpv
            mkForeignPtrVal (plusForeignPtr fp (fromIntegral n))
        _ -> error ("plusForeignPtr: bad args: " <> showValForDebug fpv)

-- | @minusForeignPtr :: ForeignPtr a -> ForeignPtr b -> Int@.  bytestring's
-- own definition pattern-matches on the @ForeignPtr@ data constructor to
-- pull out raw addresses, but in IHC @ForeignPtr@ is RTS-backed
-- ('VPrimObj (PrimForeignPtr _)') with no exposed constructor, so the
-- pattern match silently fails and the function returns @()@ instead of
-- the byte difference.  Register a host shim that does the host-side
-- subtraction directly.
minusForeignPtrB :: IO Val
minusForeignPtrB = pure $ VFun $ \aT -> pure $ VFun $ \bT -> do
    av <- force legacyHooks aT; bv <- force legacyHooks bT
    fpA <- foreignPtrValToForeignPtr av
    fpB <- foreignPtrValToForeignPtr bv
    let pA = unsafeForeignPtrToPtr fpA
        pB = unsafeForeignPtrToPtr fpB
    pure (VInt (fromIntegral (pA `minusPtr` pB)))

touchForeignPtrB :: IO Val
touchForeignPtrB = pure $ VFun $ \fpT -> pure $ VIO $ do
    fpv <- force legacyHooks fpT
    fp <- foreignPtrValToForeignPtr fpv
    touchForeignPtr fp
    pure VUnit

newForeignPtr_B :: IO Val
newForeignPtr_B = pure $ VFun $ \pT -> pure $ VIO $ do
    pv <- force legacyHooks pT
    p <- ptrValToPtr pv
    fp <- newForeignPtr_ (castPtr p)
    mkForeignPtrVal fp

newForeignPtrB :: IO Val
newForeignPtrB = pure $ VFun $ \_finalizerT -> pure $ VFun $ \pT -> pure $ VIO $ do
    pv <- force legacyHooks pT
    p <- ptrValToPtr pv
    fp <- newForeignPtr_ (castPtr p)
    mkForeignPtrVal fp

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

looksLikeAddrInfo :: Word32 -> Word32 -> Word32 -> Word32 -> Bool
looksLikeAddrInfo flags family socktype protocol =
    flags <= 0x1fff
    && family `elem` [0, 1, 2, 30]
    && socktype <= 10
    && protocol <= 255

peekAddrInfoVal :: Ptr Word8 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Val
peekAddrInfoVal p flags family socktype protocol = do
    canonPtrWord <- peekByteOff (castPtr p :: Ptr Word64) 24
    addrPtrWord <- peekByteOff (castPtr p :: Ptr Word64) 32
    flagsT <- newWHNFThunk =<< addrInfoFlagsVal flags
    familyT <- newWHNFThunk =<< oneFieldCon "Family" family
    socktypeT <- newWHNFThunk =<< oneFieldCon "SocketType" socktype
    protocolT <- newWHNFThunk (VInt (fromIntegral protocol))
    addrT <- newWHNFThunk =<< peekSockAddrVal (wordPtrToPtr addrPtrWord)
    canonT <- newWHNFThunk =<< maybeCStringVal canonPtrWord
    pure (VCon "AddrInfo" [flagsT, familyT, socktypeT, protocolT, addrT, canonT])

-- | Read a single @struct addrinfo@ at @p@ and return a 'Val'.  Used by
-- 'getAddrInfoB' when walking the linked list returned by
-- @getaddrinfo(3)@.
peekFullAddrInfoVal :: Ptr Word8 -> IO Val
peekFullAddrInfoVal p = do
    flags    <- peekByteOff (castPtr p :: Ptr Word32) 0
    family   <- peekByteOff (castPtr p :: Ptr Word32) 4
    socktype <- peekByteOff (castPtr p :: Ptr Word32) 8
    protocol <- peekByteOff (castPtr p :: Ptr Word32) 12
    peekAddrInfoVal p flags family socktype protocol

-- | @getAddrInfo :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName
-- -> IO [AddrInfo]@.  Calls into the host's @getaddrinfo(3)@.  Parses
-- the @hints@ argument and passes through the OS-relevant fields
-- (addrFlags, addrFamily, addrSocketType, addrProtocol).  Walks the
-- linked list via @ai_next@ at offset 40 (Darwin/Linux x86_64+arm64
-- layout) and materialises each entry as a 'VCon "AddrInfo"' before
-- @freeaddrinfo@-ing the chain.
--
-- Without parsing hints, warp's @bindPortGenEx@ would receive a
-- mixed UDP+TCP result list, and the first @setSocketOption sock
-- NoDelay 1@ on the UDP entry would throw EINVAL.  The exception
-- is caught by @tryAddrs@ and the next addr (TCP) used, but the
-- per-attempt churn is wasted work.
getAddrInfoB :: IO Val
getAddrInfoB = pure $ VFun $ \hintsT -> pure $ VFun $ \hostT -> pure $ VFun $ \serviceT -> pure $ VIO $ do
    hintsV <- force legacyHooks hintsT
    hostV <- force legacyHooks hostT
    serviceV <- force legacyHooks serviceT
    withMaybeCString hostV $ \hostP ->
        withMaybeCString serviceV $ \serviceP ->
            withHintsPtr hintsV $ \hintsP ->
                alloca $ \(resPP :: Ptr (Ptr Word8)) -> do
                    rc <- c_getaddrinfo_host hostP serviceP hintsP resPP
                    if rc /= 0
                        then ioError (userError ("getaddrinfo: returned " ++ show rc))
                        else do
                            firstP <- peek resPP
                            if firstP == nullPtr
                                then pure (VCon "[]" [])
                                else do
                                    lst <- walkAddrInfo firstP
                                    c_freeaddrinfo_host firstP
                                    pure lst
  where
    walkAddrInfo :: Ptr Word8 -> IO Val
    walkAddrInfo p = do
        v <- peekFullAddrInfoVal p
        nextWord <- peekByteOff (castPtr p :: Ptr Word64) 40
        let nextP = wordPtrToPtr nextWord
        rest <- if nextP == nullPtr
                    then pure (VCon "[]" [])
                    else walkAddrInfo nextP
        hd <- newWHNFThunk v
        tl <- newWHNFThunk rest
        pure (VCon ":" [hd, tl])

    withMaybeCString :: Val -> (Ptr Word8 -> IO a) -> IO a
    withMaybeCString v action = case v of
        VCon "Nothing" []   -> action nullPtr
        VCon "Just" [innerT] -> do
            inner <- force legacyHooks innerT
            s <- valToString inner
            withCString s (action . castPtr)
        other -> error ("getAddrInfo: not Maybe String: " <> showValForDebug other)

    -- Build a host @struct addrinfo@ from a @Maybe AddrInfo@ Val and
    -- pass its pointer to the action.  Without this, @c_getaddrinfo@
    -- gets a NULL hints pointer and returns ALL socket types — warp
    -- requests Stream-only, but we'd hand back UDP entries too, then
    -- @setSocketOption sock TCP_NODELAY@ on the UDP socket fails with
    -- EINVAL.  The first 4 fields (flags, family, socktype, protocol)
    -- are 4-byte ints; the rest can be zero-filled because
    -- getaddrinfo only reads the first four when given hints.
    withHintsPtr :: Val -> (Ptr Word8 -> IO a) -> IO a
    withHintsPtr v action = case v of
        VCon "Nothing" [] -> action nullPtr
        VCon "Just" [innerT] -> do
            inner <- force legacyHooks innerT
            (flags, family, socktype, protocol) <- extractHintsFields inner
            allocaBytes 48 $ \p -> do
                fillBytes p 0 48
                pokeByteOff (castPtr p :: Ptr Word32) 0  flags
                pokeByteOff (castPtr p :: Ptr Word32) 4  family
                pokeByteOff (castPtr p :: Ptr Word32) 8  socktype
                pokeByteOff (castPtr p :: Ptr Word32) 12 protocol
                action p
        _ -> action nullPtr

    -- Extract (flags, family, socktype, protocol) from an AddrInfo
    -- record value.  Pattern: VCon "AddrInfo" [flagsT, familyT,
    -- socktypeT, protocolT, addressT, canonNameT].  The flags field is
    -- a list of constructors like @[AI_PASSIVE]@ that we OR-fold; the
    -- family / socktype / protocol fields are single-field VCons
    -- wrapping @CInt@ codes.
    extractHintsFields :: Val -> IO (Word32, Word32, Word32, Word32)
    extractHintsFields val = case val of
        VCon "AddrInfo" (flagsT : familyT : socktypeT : protocolT : _) -> do
            flagsV    <- force legacyHooks flagsT
            familyV   <- force legacyHooks familyT
            socktypeV <- force legacyHooks socktypeT
            protocolV <- force legacyHooks protocolT
            flags    <- foldFlagBits flagsV
            family   <- conIntField "addrFamily" familyV
            socktype <- conIntField "addrSocketType" socktypeV
            protocol <- conIntField "addrProtocol" protocolV
            pure (flags, family, socktype, protocol)
        _ -> pure (0, 0, 0, 0)

    -- @[AI_FOO, AI_BAR]@ → bitwise-or of named flag values.
    foldFlagBits :: Val -> IO Word32
    foldFlagBits = go 0
      where
        go !acc v = case v of
            VCon "[]" []        -> pure acc
            VCon ":" [hT, tT]   -> do
                hV <- force legacyHooks hT
                tV <- force legacyHooks tT
                let bit = case hV of
                        VCon "AI_ADDRCONFIG" _ -> 1024
                        VCon "AI_ALL" _        -> 256
                        VCon "AI_CANONNAME" _  -> 2
                        VCon "AI_NUMERICHOST" _ -> 4
                        VCon "AI_NUMERICSERV" _ -> 4096
                        VCon "AI_PASSIVE" _    -> 1
                        VCon "AI_V4MAPPED" _   -> 2048
                        _                       -> 0
                go (acc .|. bit) tV
            _ -> pure acc

    conIntField :: String -> Val -> IO Word32
    conIntField name v = case v of
        VCon _ [innerT] -> do
            inner <- force legacyHooks innerT
            case inner of
                VInt n -> pure (fromIntegral n)
                _      -> error (name <> ": inner not VInt: " <> showValForDebug inner)
        VInt n          -> pure (fromIntegral n)
        _ -> error (name <> ": not a single-field VCon or VInt: " <> showValForDebug v)

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

peekSockAddrVal :: Ptr Word8 -> IO Val
peekSockAddrVal p
    | p == nullPtr = do
        portT <- newWHNFThunk (VInt 0)
        addrT <- newWHNFThunk (VInt 0)
        pure (VCon "SockAddrInet" [portT, addrT])
    | otherwise = do
        family <- peekByteOff p 1 :: IO Word8
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
            30 -> do
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

socketBindB :: IO Val
socketBindB = pure $ VFun $ \sockT -> pure $ VFun $ \addrT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    addrV <- force legacyHooks addrT
    fd <- socketFdFromVal sockV
    (sz, pokeAddr) <- sockAddrPoke addrV
    allocaBytes sz $ \p -> do
        fillBytes p 0 sz
        pokeAddr (castPtr p)
        rc <- c_bind_host (fromIntegral fd) (castPtr p) (fromIntegral sz)
        if rc == -1
            then do
                Errno e <- getErrno
                ioError (userError ("Network.Socket.bind: errno=" <> show e))
            else pure VUnit

socketCreateB :: IO Val
socketCreateB = pure $ VFun $ \familyT -> pure $ VFun $ \stypeT -> pure $ VFun $ \protocolT -> pure $ VIO $ do
    family <- familyField familyT
    stype <- socketTypeField stypeT
    protocol <- intField "socket.protocol" protocolT
    fd <- c_socket_host (fromIntegral family) (fromIntegral stype) (fromIntegral protocol)
    if fd == -1
        then ioError (userError "Network.Socket.socket")
        else do
            fdT <- newWHNFThunk (VInt (fromIntegral fd))
            ref <- newIORef fdT
            refT <- newWHNFThunk (VPrimObj (PrimIORef ref))
            pure (VCon "Socket" [refT, fdT])

socketSetOptionB :: IO Val
socketSetOptionB = pure $ VFun $ \sockT -> pure $ VFun $ \optT -> pure $ VFun $ \valueT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    (level, opt) <- socketOptionField optT
    value <- intField "setSocketOption.value" valueT
    allocaBytes (sizeOf (undefined :: CInt)) $ \p -> do
        poke (castPtr p :: Ptr CInt) (fromIntegral value)
        rc <- c_setsockopt_host (fromIntegral fd) (fromIntegral level) (fromIntegral opt) (castPtr p) (fromIntegral (sizeOf (undefined :: CInt)))
        if rc == -1
            then ioError (userError "Network.Socket.setSocketOption")
            else pure VUnit

socketListenB :: IO Val
socketListenB = pure $ VFun $ \sockT -> pure $ VFun $ \backlogT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    backlog <- intField "listen.backlog" backlogT
    rc <- c_listen_host (fromIntegral fd) (fromIntegral backlog)
    if rc == -1
        then ioError (userError "Network.Socket.listen")
        else pure VUnit

socketAcceptB :: IO Val
socketAcceptB = pure $ VFun $ \sockT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    allocaBytes 128 $ \addrP ->
      allocaBytes (sizeOf (undefined :: CInt)) $ \lenP -> do
        let acceptLoop = do
                fillBytes addrP 0 128
                poke (castPtr lenP :: Ptr CInt) 128
                newFd <- c_accept_host (fromIntegral fd)
                                       (castPtr addrP)
                                       (castPtr lenP)
                if newFd /= -1
                    then pure newFd
                    else do
                        -- @Network.Socket@ puts listening sockets in
                        -- non-blocking mode (see @Syscall.hs@ in
                        -- @network@), so a real accept must retry on
                        -- EAGAIN / EWOULDBLOCK via the IO manager and
                        -- restart on EINTR — otherwise warp's accept
                        -- loop bails on the first poll-with-no-pending.
                        Errno e <- getErrno
                        if Errno e == eAGAIN
                                || Errno e == eWOULDBLOCK
                            then do
                                threadWaitRead (fromIntegral fd)
                                acceptLoop
                            else if Errno e == eINTR
                                then acceptLoop
                                else ioError
                                    (userError
                                       ("Network.Socket.accept: errno="
                                        <> show e))
        newFd <- acceptLoop
        fdValT <- newWHNFThunk (VInt (fromIntegral newFd))
        ref <- newIORef fdValT
        refT <- newWHNFThunk (VPrimObj (PrimIORef ref))
        sockOutT <- newWHNFThunk (VCon "Socket" [refT, fdValT])
        addrV <- peekSockAddrVal (castPtr addrP)
        addrT <- newWHNFThunk addrV
        pure (VCon "(,)" [sockOutT, addrT])

socketGetNameB :: IO Val
socketGetNameB = pure $ VFun $ \sockT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    allocaBytes 128 $ \addrP ->
      allocaBytes (sizeOf (undefined :: CInt)) $ \lenP -> do
        fillBytes addrP 0 128
        poke (castPtr lenP :: Ptr CInt) 128
        rc <- c_getsockname_host (fromIntegral fd) (castPtr addrP) (castPtr lenP)
        if rc == -1
            then ioError (userError "Network.Socket.getSocketName")
            else peekSockAddrVal (castPtr addrP)

socketFdFromVal :: Val -> IO Int64
socketFdFromVal v = do
    fdV <- socketCurrentFdVal v
    case fdV of
        VInt fd -> pure fd
        other   -> error ("Socket fd is not an Int: " <> showValForDebug other)

socketCurrentFdVal :: Val -> IO Val
socketCurrentFdVal (VCon "Socket" [refT, _fdT]) = do
    refV <- force legacyHooks refT
    case refV of
        VPrimObj (PrimIORef rf) -> readIORef rf >>= force legacyHooks
        other -> error ("Socket ref is not an IORef: " <> showValForDebug other)
socketCurrentFdVal other = error ("bind: not a Socket: " <> showValForDebug other)

socketFdB :: IO Val
socketFdB = pure $ VFun $ \sockT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    pure (VInt fd)

socketCloseB :: Bool -> (IO Val)
socketCloseB throwOnError = pure $ VFun $ \sockT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    case sockV of
        VCon "Socket" [refT, _fdT] -> do
            refV <- force legacyHooks refT
            case refV of
                VPrimObj (PrimIORef rf) -> do
                    sentinelT <- newWHNFThunk (VInt (-1))
                    oldT <- atomicModifyIORef' rf $ \cur -> (sentinelT, cur)
                    oldFdV <- force legacyHooks oldT
                    case oldFdV of
                        VInt oldFd
                            | oldFd == -1 -> pure VUnit
                            | otherwise -> do
                                rc <- c_close_host (fromIntegral oldFd)
                                if rc == -1 && throwOnError
                                    then ioError (userError "Network.Socket.close'")
                                    else pure VUnit
                        other -> error ("Socket fd cell is not an Int: " <> showValForDebug other)
                other -> error ("Socket ref is not an IORef: " <> showValForDebug other)
        other -> error ("close: not a Socket: " <> showValForDebug other)

withFdSocketB :: IO Val
withFdSocketB = pure $ VFun $ \sockT -> pure $ VFun $ \fnT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    fd <- socketFdFromVal sockV
    fnV <- force legacyHooks fnT
    fdT <- newWHNFThunk (VInt fd)
    r <- apply legacyHooks fnV fdT
    runIOVal legacyHooks r

socketSendBufB :: IO Val
socketSendBufB = pure $ VFun $ \sockT -> pure $ VFun $ \ptrT -> pure $ VFun $ \lenT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    ptrV <- force legacyHooks ptrT
    lenV <- force legacyHooks lenT
    fd <- socketFdFromVal sockV
    ptr <- ptrValToPtr ptrV
    case lenV of
        VInt n | n >= 0 -> do
            r <- c_send_host (fromIntegral fd) (castPtr ptr) (fromIntegral n) 0
            if r < 0
                then ioError (userError "Network.Socket.sendBuf")
                else pure (VInt (fromIntegral r))
        _ -> error ("sendBuf: not a non-negative Int: " <> showValForDebug lenV)

socketRecvBufB :: IO Val
socketRecvBufB = pure $ VFun $ \sockT -> pure $ VFun $ \ptrT -> pure $ VFun $ \lenT -> pure $ VIO $ do
    sockV <- force legacyHooks sockT
    ptrV <- force legacyHooks ptrT
    lenV <- force legacyHooks lenT
    fd <- socketFdFromVal sockV
    ptr <- ptrValToPtr ptrV
    case lenV of
        VInt n | n > 0 -> do
            r <- c_recv_host (fromIntegral fd) (castPtr ptr) (fromIntegral n) 0
            if r < 0
                then ioError (userError "Network.Socket.recvBuf")
                else pure (VInt (fromIntegral r))
        _ -> error ("recvBuf: not a positive Int: " <> showValForDebug lenV)

mallocBytesB :: IO Val
mallocBytesB = pure $ VFun $ \nT -> pure $ VIO $ do
    n <- intField "mallocBytes.size" nT
    p <- mallocBytes (max 1 (fromIntegral n))
    pure (VPrimObj (PrimPtr (castPtr p)))

freeB :: IO Val
freeB = pure $ VFun $ \ptrT -> pure $ VIO $ do
    ptrV <- force legacyHooks ptrT
    p <- ptrValToPtr ptrV
    free p
    pure VUnit

-- | Generic field accessor for the host-built @VCon "AddrInfo" [flags,
-- family, socktype, protocol, addr, canonName]@ value.  Used to back
-- @addrFamily@ / @addrAddress@ / etc. when warp's source code accesses
-- record fields on a host-constructed AddrInfo.
addrInfoFieldB :: String -> Int -> IO Val
addrInfoFieldB label idx = pure $ VFun $ \aiT -> do
    aiV <- force legacyHooks aiT
    case aiV of
        VCon "AddrInfo" fields
            | idx < length fields -> force legacyHooks (fields !! idx)
        other -> error (label <> ": not AddrInfo: " <> showValForDebug other)

sockAddrPoke :: Val -> IO (Int, Ptr Word8 -> IO ())
sockAddrPoke (VCon "SockAddrInet" [portT, addrT]) = do
    port <- intField "SockAddrInet.port" portT
    addr <- intField "SockAddrInet.addr" addrT
    -- HostAddress is a Word32 already in network byte order (its host-LE
    -- bytes ARE the network bytes).  PortNumber is host order, so htons
    -- is required for sin_port; sin_addr is poked raw.
    pure (16, \p -> do
        pokeByteOff p 0 (16 :: Word8)
        pokeByteOff p 1 (2 :: Word8)
        pokeByteOff p 2 (htons16 (fromIntegral port) :: Word16)
        pokeByteOff p 4 (fromIntegral addr :: Word32))
sockAddrPoke (VCon "SockAddrInet6" [portT, flowT, addrT, scopeT]) = do
    port <- intField "SockAddrInet6.port" portT
    flow <- intField "SockAddrInet6.flow" flowT
    scope <- intField "SockAddrInet6.scope" scopeT
    (a0, a1, a2, a3) <- hostAddress6Fields addrT
    -- HostAddress6 = (Word32, Word32, Word32, Word32) in HOST byte order,
    -- so each chunk needs htonl on poke.  ScopeID is raw (host concept).
    pure (28, \p -> do
        pokeByteOff p 0 (28 :: Word8)
        pokeByteOff p 1 (30 :: Word8)
        pokeByteOff p 2 (htons16 (fromIntegral port) :: Word16)
        pokeByteOff p 4 (htonl32 (fromIntegral flow) :: Word32)
        pokeByteOff p 8 (htonl32 (fromIntegral a0) :: Word32)
        pokeByteOff p 12 (htonl32 (fromIntegral a1) :: Word32)
        pokeByteOff p 16 (htonl32 (fromIntegral a2) :: Word32)
        pokeByteOff p 20 (htonl32 (fromIntegral a3) :: Word32)
        pokeByteOff p 24 (fromIntegral scope :: Word32))
sockAddrPoke other = error ("bind: unsupported SockAddr: " <> showValForDebug other)

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

familyField :: Thunk -> IO Int64
familyField t = do
    v <- force legacyHooks t
    case v of
        VCon "Family" [nT] -> intField "socket.family" nT
        VInt n             -> pure n
        other              -> error ("socket.family: not a Family: " <> showValForDebug other)

socketTypeField :: Thunk -> IO Int64
socketTypeField t = do
    v <- force legacyHooks t
    case v of
        VCon "SocketType" [nT] -> intField "socket.type" nT
        VInt n                 -> pure n
        other                  -> error ("socket.type: not a SocketType: " <> showValForDebug other)

socketOptionField :: Thunk -> IO (Int64, Int64)
socketOptionField t = do
    v <- force legacyHooks t
    case v of
        VCon "SockOpt" [levelT, optT] -> do
            level <- intField "socket.option.level" levelT
            opt <- intField "socket.option.name" optT
            pure (level, opt)
        other -> error ("setSocketOption: not a SocketOption: " <> showValForDebug other)

-- | Helper for the @Network.Socket.Options@ pattern-synonym constants
-- (e.g. @NoDelay = SockOpt 6 1@).  Returns the underlying
-- 'SockOpt' VCon directly so 'socketOptionField' can decode it.
sockOptB :: Int64 -> Int64 -> IO Val
sockOptB level opt = do
    levelT <- newWHNFThunk (VInt level)
    optT   <- newWHNFThunk (VInt opt)
    pure (VCon "SockOpt" [levelT, optT])

foreign import ccall unsafe "socket"
    c_socket_host :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "setsockopt"
    c_setsockopt_host :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "listen"
    c_listen_host :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "accept"
    c_accept_host :: CInt -> Ptr Word8 -> Ptr CInt -> IO CInt

foreign import ccall unsafe "getsockname"
    c_getsockname_host :: CInt -> Ptr Word8 -> Ptr CInt -> IO CInt

foreign import ccall unsafe "bind"
    c_bind_host :: CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall unsafe "close"
    c_close_host :: CInt -> IO CInt

foreign import ccall unsafe "send"
    c_send_host :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "recv"
    c_recv_host :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "getaddrinfo"
    c_getaddrinfo_host :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr (Ptr Word8) -> IO CInt

foreign import ccall unsafe "freeaddrinfo"
    c_freeaddrinfo_host :: Ptr Word8 -> IO ()

pokeB :: IO Val
pokeB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    av <- force legacyHooks a; bv <- force legacyHooks b
    p <- ptrValToPtr av
    case bv of
        VInt n -> do { poke (p :: Ptr Word8) (fromIntegral n); pure VUnit }
        _ -> error ("poke: value not an Int: " <> showValForDebug bv)

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
        VInt off ->
            case cv of
                VInt n -> do
                    pokeByteOff (p :: Ptr Word8) (fromIntegral off) (fromIntegral n :: Word8)
                    pure VUnit
                _ | off == 24 || off == 32 || off == 40 -> do
                    ptr <- ptrValToPtr cv
                    pokeByteOff (castPtr p :: Ptr Word64) (fromIntegral off)
                        (fromIntegral (ptrToIntPtr (castPtr ptr)) :: Word64)
                    pure VUnit
                _ -> error ("pokeByteOff: value not an Int: " <> showValForDebug cv)
        _ -> error ("pokeByteOff: bad args: " <> showValForDebug av)

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
-- Phase 2.8: C memory ops
--------------------------------------------------------------------------------

memcpyB :: IO Val
memcpyB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VFun $ \c -> pure $ VIO $ do
    dstV <- force legacyHooks a; srcV <- force legacyHooks b; lenV <- force legacyHooks c
    case (dstV, srcV, lenV) of
        (VPrimObj (PrimPtr dst), VPrimObj (PrimPtr src), VInt n) -> do
            copyBytes dst src (fromIntegral n)
            pure VUnit
        _ -> error "memcpy: bad args"

memcpyFpB :: IO Val
memcpyFpB = pure $ VFun $ \fpT -> pure $ VFun $ \pT -> pure $ VFun $ \nT -> pure $ VIO $ do
    fpv <- force legacyHooks fpT; pv <- force legacyHooks pT; nv <- force legacyHooks nT
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
    dv <- force legacyHooks destT; sv <- force legacyHooks srcT; nv <- force legacyHooks nT
    case (dv, sv, nv) of
        (VPrimObj (PrimPtr dest), VPrimObj (PrimPtr src), VInt n) -> do
            copyBytes dest src (fromIntegral n)
            pure VUnit
        _ -> error $ "copyBytes: bad args"

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

timesInt2B :: IO Val
timesInt2B = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
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
    av <- force legacyHooks a; bv <- force legacyHooks b
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

ltWordB, leWordB, eqWordB, gtWordB, geWordB :: IO Val
ltWordB = makeWordCmpOp "ltWord#" (<)
leWordB = makeWordCmpOp "leWord#" (<=)
eqWordB = makeWordCmpOp "eqWord#" (==)
gtWordB = makeWordCmpOp "gtWord#" (>)
geWordB = makeWordCmpOp "geWord#" (>=)

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
-- | floor/ceiling/round/truncate — Float -> Int.  Tracked
-- carve-out: the source-loaded RealFrac Double / properFractionFloat
-- chain reaches Num Integer class-method dispatch which still
-- bottoms out at @<<ihc-method-placeholder>>@ — even with the
-- IS\/IP\/IN matchPat bridge below in place.  Lifting the rest
-- requires class-dispatch work for Num Integer \/ Integral
-- Integer instances; a separate workstream.
floatToIntB :: (Double -> Int64) -> IO Val
floatToIntB op = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VFloat d -> pure (VInt (op d))
        VInt n   -> pure (VInt n)
        _ -> error ("floatToInt: non-numeric arg: " <> showValForDebug av)


asInt64 :: Val -> Maybe Int64
asInt64 (VInt n) = Just n
asInt64 (VInteger n)
  | n >= toInteger (minBound :: Int64)
  , n <= toInteger (maxBound :: Int64) = Just (fromInteger n)
asInt64 _ = Nothing


-- | IO-aware coercion of a 'Val' to 'Int64'.  Like 'asInt64'
-- but additionally peeks through @VCon \"IS\" [thunk]@ — the
-- source-loaded ghc-bignum 'Integer' small-Int constructor —
-- by forcing the inner thunk.  See @IHC.Eval.matchPat@'s @IS@
-- pattern arm for the symmetric pattern-direction bridge.
coerceInt64 :: Val -> IO (Maybe Int64)
coerceInt64 v = case asInt64 v of
    Just n -> pure (Just n)
    Nothing -> case v of
        VCon "IS" [t] -> do
            v' <- force legacyHooks t
            coerceInt64 v'
        _ -> pure Nothing


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

--------------------------------------------------------------------------------
-- Phase 2.8: misc primops
--------------------------------------------------------------------------------

cstringLengthB :: IO Val
cstringLengthB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
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
    av <- force legacyHooks a
    case av of
        VPrimObj (PrimPtr p) -> do
            s <- peekCAString (castPtr p)
            stringToListValIO s
        VStr s -> stringToListValIO (BC.unpack s)
        _ -> error ("unpackCString#: bad arg: " <> showValForDebug av)

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

withB :: IO Val
withB = pure $ VFun $ \valT -> pure $ VFun $ \fT -> pure $ VIO $ do
    valV <- force legacyHooks valT
    fV <- force legacyHooks fT
    p <- mallocBytes 8
    fillBytes p 0 8
    case valV of
        VInt n -> poke (castPtr p :: Ptr CInt) (fromIntegral n)
        _      -> pure ()
    ptrT <- newWHNFThunk (VPrimObj (PrimPtr p))
    r <- apply legacyHooks fV ptrT
    runIOVal legacyHooks r

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

divModB :: IO Val
divModB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt y) -> do
            let (d, m) = x `divMod` y
            dT <- newWHNFThunk (VInt d); mT <- newWHNFThunk (VInt m)
            pure (VCon "(,)" [dT, mT])
        _ -> error "divMod: bad args"

quotRemB :: IO Val
quotRemB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    mx <- coerceInt64 av
    my <- coerceInt64 bv
    case (mx, my) of
        (Just x, Just y) -> do
            let (q, r) = x `quotRem` y
            qT <- newWHNFThunk (VInt q); rT <- newWHNFThunk (VInt r)
            pure (VCon "(,)" [qT, rT])
        _ -> error ("quotRem: bad args: a=" <> showValForDebug av
                   <> " b=" <> showValForDebug bv)

shiftLB :: IO Val
shiftLB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftL` fromIntegral n))
        _ -> error "shiftL: bad args"

shiftRB :: IO Val
shiftRB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x `shiftR` fromIntegral n))
        _ -> error "shiftR: bad args"

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

bitComplementB :: IO Val
bitComplementB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt x -> pure (VInt (complement x))
        _ -> error "complement: bad arg"

popCountB :: IO Val
popCountB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (fromIntegral (popCount (fromIntegral n :: Word64))))
        _ -> error "popCount: bad arg"

bitB :: IO Val
bitB = pure $ VFun $ \a -> do
    av <- force legacyHooks a
    case av of
        VInt n -> pure (VInt (1 `shiftL` fromIntegral n))
        _ -> error "bit: bad arg"

testBitB :: IO Val
testBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (boolVal ((x `shiftR` fromIntegral n) .&. 1 /= 0))
        _ -> error "testBit: bad args"

clearBitB :: IO Val
clearBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
    case (av, bv) of
        (VInt x, VInt n) -> pure (VInt (x .&. complement (1 `shiftL` fromIntegral n)))
        _ -> error "clearBit: bad args"

setBitB :: IO Val
setBitB = pure $ VFun $ \a -> pure $ VFun $ \b -> do
    av <- force legacyHooks a; bv <- force legacyHooks b
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
    av <- force legacyHooks a
    bv <- force legacyHooks b
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
    av <- force legacyHooks a
    bv <- force legacyHooks b
    let toD (VFloat d) = d
        toD (VInt n)   = fromIntegral n
        toD v          = error ("(^^)/(** ): non-numeric: " <> showValForDebug v)
    pure (VFloat (toD av ** toD bv))

--------------------------------------------------------------------------------
-- Simple file IO: readFile, writeFile, appendFile
--------------------------------------------------------------------------------

-- | @readFile path@ — read the entire file as a String ([Char]).
readFileB :: IO Val
readFileB = pure $ VFun $ \a -> pure $ VIO $ do
    pv   <- force legacyHooks a
    path <- valToString pv
    contents <- readFile path
    stringToListValIO contents

-- | @writeFile path contents@ — write a String to the file (truncating).
writeFileB :: IO Val
writeFileB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    pv   <- force legacyHooks a
    path <- valToString pv
    cv   <- force legacyHooks b
    s    <- valToString cv
    writeFile path s
    pure VUnit

-- | @appendFile path contents@ — append a String to the file.
appendFileB :: IO Val
appendFileB = pure $ VFun $ \a -> pure $ VFun $ \b -> pure $ VIO $ do
    pv   <- force legacyHooks a
    path <- valToString pv
    cv   <- force legacyHooks b
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
    buildLam :: Name -> Int -> [Thunk] -> Val
    buildLam name 0    acc = VCon name (reverse acc)
    buildLam name left acc = VFun $ \t ->
        if left == 1
            then do
                let thunks = reverse (t : acc)
                strict <- lookupCtorStrictness name
                forceStrictFields strict thunks
                pure (VCon name thunks)
            else pure (buildLam name (left - 1) (t : acc))

    -- | Walk the strict-field bitmap and force each marked thunk in
    -- place, ignoring extra/missing entries gracefully.
    forceStrictFields :: [Bool] -> [Thunk] -> IO ()
    forceStrictFields []         _      = pure ()
    forceStrictFields _          []     = pure ()
    forceStrictFields (s : ss) (t : ts) = do
        when s (() <$ force legacyHooks t)
        forceStrictFields ss ts

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
                       <> "` applied to non-constructor value"))

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
        | otherwise = Nothing

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

-- | @forkIO action@ - fork a new thread running the IO action.
forkIOB :: IO Val
forkIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    tid <- forkIO $ do
        _ <- runIOVal legacyHooks av
        pure ()
    pure (VPrimObj (PrimThreadId tid))

-- | @fork# :: IO () -> State# RealWorld -> (# State# RealWorld, ThreadId# #)@
-- — GHC primop used by source-loaded @forkIO@ and warp's @defaultFork@
-- (the latter inlines the primop call).  No Haskell source in
-- @ghc-prim@; we host it.  Wraps the host 'forkIO' and packages the
-- result as a state-passing unboxed tuple so source-side
-- @case fork# io s of (# s', tid #) -> ...@ matches our pattern bridge.
forkHashB :: IO Val
forkHashB = pure $ VFun $ \aT -> pure $ VFun $ \_sT -> do
    av <- force legacyHooks aT
    tid <- forkIO $ do
        _ <- runIOVal legacyHooks av
        pure ()
    rwT  <- newWHNFThunk (VPrimObj PrimRealWorld)
    tidT <- newWHNFThunk (VPrimObj (PrimThreadId tid))
    pure (VCon "(#,#)" [rwT, tidT])

-- | @killThread tid@ - asynchronously raise 'ThreadKilled' in the thread.
killThreadB :: IO Val
killThreadB = pure $ VFun $ \tidT -> pure $ VIO $ do
    tidV <- force legacyHooks tidT
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

fromThreadIdB :: IO Val
fromThreadIdB = pure $ VFun $ \tidT -> do
    tidV <- force legacyHooks tidT
    case tidV of
        VPrimObj (PrimThreadId tid) ->
            pure (VInt (fromIntegral (threadIdKey tid)))
        other -> error ("fromThreadId: not a ThreadId: " <> showValForDebug other)
  where
    threadIdKey tid =
        let s = show tid
            step h c = h * 16777619 + fromIntegral (ord c)
        in foldl step (2166136261 :: Word64) s

myThreadIdHashB :: IO Val
myThreadIdHashB = pure $ VFun $ \stT -> do
    tid <- myThreadId
    tidT <- newWHNFThunk (VPrimObj (PrimThreadId tid))
    pure (VCon "(#,#)" [stT, tidT])

labelThreadB :: IO Val
labelThreadB = pure $ VFun $ \_tidT -> pure $ VFun $ \_labelT ->
    pure $ VIO $ pure VUnit

labelThreadByteArrayHashB :: IO Val
labelThreadByteArrayHashB = pure $ VFun $ \_tidT -> pure $ VFun $ \_labelT ->
    pure $ VIO $ pure VUnit

-- | @threadDelay microseconds@ - sleep.
threadDelayB :: IO Val
threadDelayB = pure $ VFun $ \nT -> pure $ VIO $ do
    nv <- force legacyHooks nT
    case nv of
        VInt n -> do { threadDelay (fromIntegral n); pure VUnit }
        _ -> error ("threadDelay: not an Int: " <> showValForDebug nv)

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

-- | Unwrap a @Fd@-like argument to its underlying @Int@.  Accepts the
-- common shapes the source @System.Posix.Types.Fd@ newtype can take
-- after interpretation: bare 'VInt', or @VCon "Fd" [VInt n]@.
fdArgToInt :: Thunk -> String -> IO Int64
fdArgToInt t primName = do
    v <- force legacyHooks t
    case v of
        VInt n          -> pure n
        VCon _ [inner] -> do
            iv <- force legacyHooks inner
            case iv of
                VInt n -> pure n
                _ -> error (primName <> ": Fd payload not VInt: " <> showValForDebug iv)
        _ -> error (primName <> ": not Fd-like: " <> showValForDebug v)

getSystemEventManagerB :: IO Val
getSystemEventManagerB = pure $ VIO $ pure (VCon "Nothing" [])

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

timeManagerWithHandleB :: IO Val
timeManagerWithHandleB = pure $ VFun $ \_mgrT -> pure $ VFun $ \_timeoutActionT -> pure $ VFun $ \actionT ->
    pure $ VIO $ do
        actionV <- force legacyHooks actionT
        h <- emptyTimeManagerHandle
        hT <- newWHNFThunk h
        r <- apply legacyHooks actionV hT
        runIOVal legacyHooks r
  where
    emptyTimeManagerHandle = do
        timeoutT <- newWHNFThunk (VInt 0)
        actionT <- newWHNFThunk (VIO (pure VUnit))
        keyRefT <- newWHNFThunk VUnit
        stateT <- newWHNFThunk VUnit
        pure (VCon "Handle" [timeoutT, actionT, keyRefT, stateT])

-- | @System.TimeManager.initialize :: Int -> IO Manager@.  Upstream's
-- implementation since time-manager 0.3.0 is just @pure . Manager . max 0@,
-- since timeouts are implemented via the GHC RTS timer manager.  IHC does
-- not run the RTS timer manager either, so we mirror the same behaviour:
-- box the (clamped) timeout value into a @Manager@ constructor and hand it
-- back. 'timeManagerWithHandleB' / 'timeManagerStopManagerB' do not look at
-- the payload, so any well-formed @VCon "Manager" [VInt n]@ is fine.
timeManagerInitializeB :: IO Val
timeManagerInitializeB = pure $ VFun $ \timeoutT -> pure $ VIO $ do
    timeoutV <- force legacyHooks timeoutT
    case timeoutV of
        VInt n -> do
            nT <- newWHNFThunk (VInt (max 0 n))
            pure (VCon "Manager" [nT])
        other -> error ("System.TimeManager.initialize: not an Int: " <> showValForDebug other)

-- | @System.TimeManager.stopManager :: Manager -> IO ()@.  Upstream marked
-- this as deprecated in 0.3.0 and now defines it as @\\_ -> pure ()@; we do
-- the same.
timeManagerStopManagerB :: IO Val
timeManagerStopManagerB = pure $ VFun $ \_mgrT -> pure $ VIO $ pure VUnit

setFdOptionB :: IO Val
setFdOptionB = pure $ VFun $ \fdT -> pure $ VFun $ \_optT -> pure $ VFun $ \enabledT -> pure $ VIO $ do
    fdV <- force legacyHooks fdT
    enabledV <- force legacyHooks enabledT
    fd <- unwrapFd fdV
    PosixIO.setFdOption (fromIntegral fd :: Fd) PosixIO.CloseOnExec (isTruthy enabledV)
    pure VUnit
  where
    -- Accept either a raw VInt (host-backed sockets pass the raw fd) or
    -- the @Fd@ newtype wrapper @VCon "Fd" [VInt _]@ (source code that
    -- imports @System.Posix.Types (Fd)@ and constructs values through
    -- the constructor).
    unwrapFd (VInt n) = pure n
    unwrapFd (VCon "Fd" [innerT]) = do
        innerV <- force legacyHooks innerT
        unwrapFd innerV
    unwrapFd other = error ("setFdOption: not an fd: " <> showValForDebug other)

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
    av <- force legacyHooks aT
    mv <- newMVar av
    pure (VPrimObj (PrimMVar mv))

newEmptyMVarB :: IO Val
newEmptyMVarB = pure $ VIO $ do
    mv <- newEmptyMVar
    pure (VPrimObj (PrimMVar mv))

takeMVarB :: IO Val
takeMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "takeMVar" mvv
    takeMVar mv

putMVarB :: IO Val
putMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "putMVar" mvv
    av  <- force legacyHooks aT
    putMVar mv av
    pure VUnit

readMVarB :: IO Val
readMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "readMVar" mvv
    readMVar mv

modifyMVar_B :: IO Val
modifyMVar_B = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "modifyMVar_" mvv
    fv  <- force legacyHooks fT
    modifyMVar_ mv $ \cur -> do
        curT <- newWHNFThunk cur
        rv   <- apply legacyHooks fv curT
        runIOVal legacyHooks rv
    pure VUnit

modifyMVarB :: IO Val
modifyMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "modifyMVar" mvv
    fv  <- force legacyHooks fT
    modifyMVar mv $ \cur -> do
        curT  <- newWHNFThunk cur
        rv    <- apply legacyHooks fv curT
        pairV <- runIOVal legacyHooks rv
        case pairV of
            VCon _ [newT, extraT] -> do
                newV   <- force legacyHooks newT
                extraV <- force legacyHooks extraT
                pure (newV, extraV)
            _ -> error ("modifyMVar: f did not return a pair: "
                        <> showValForDebug pairV)

tryTakeMVarB :: IO Val
tryTakeMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "tryTakeMVar" mvv
    r   <- tryTakeMVar mv
    case r of
        Nothing -> pure (VCon "Nothing" [])
        Just v  -> do { t <- newWHNFThunk v; pure (VCon "Just" [t]) }

tryPutMVarB :: IO Val
tryPutMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "tryPutMVar" mvv
    av  <- force legacyHooks aT
    ok  <- tryPutMVar mv av
    pure (boolVal ok)

isEmptyMVarB :: IO Val
isEmptyMVarB = pure $ VFun $ \mvT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "isEmptyMVar" mvv
    b   <- isEmptyMVar mv
    pure (boolVal b)

withMVarB :: IO Val
withMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "withMVar" mvv
    fv  <- force legacyHooks fT
    withMVar mv $ \cur -> do
        curT <- newWHNFThunk cur
        rv   <- apply legacyHooks fv curT
        runIOVal legacyHooks rv

swapMVarB :: IO Val
swapMVarB = pure $ VFun $ \mvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    mvv <- force legacyHooks mvT
    mv  <- requireMVar "swapMVar" mvv
    av  <- force legacyHooks aT
    swapMVar mv av

--------------------------------------------------------------------------------
-- Phase 2.10a: STM primitives
--------------------------------------------------------------------------------

atomicallyB :: IO Val
atomicallyB = pure $ VFun $ \stmT -> pure $ VIO $ do
    stmV <- force legacyHooks stmT
    runIOVal legacyHooks stmV

retryB :: IO Val
retryB = pure $ VIO $ atomically retry

orElseB :: IO Val
orElseB = pure $ VFun $ \aT -> pure $ VFun $ \bT -> pure $ VIO $ do
    av <- force legacyHooks aT
    bv <- force legacyHooks bT
    -- Approximate: try av first, fall back to bv on exception
    CE.catch (runIOVal legacyHooks av) (\(_ :: CE.SomeException) -> runIOVal legacyHooks bv)

checkB :: IO Val
checkB = pure $ VFun $ \bT -> pure $ VIO $ do
    bv <- force legacyHooks bT
    atomically (check (isTruthy bv))
    pure VUnit

newTVarB :: IO Val
newTVarB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    tv <- newTVarIO av
    pure (VPrimObj (PrimTVar tv))

newTVarIOB :: IO Val
newTVarIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    tv <- newTVarIO av
    pure (VPrimObj (PrimTVar tv))

readTVarB :: IO Val
readTVarB = pure $ VFun $ \tvT -> pure $ VIO $ do
    tvv <- force legacyHooks tvT
    tv  <- requireTVarPrim "readTVar" tvv
    atomically (readTVar tv)

writeTVarB :: IO Val
writeTVarB = pure $ VFun $ \tvT -> pure $ VFun $ \aT -> pure $ VIO $ do
    tvv <- force legacyHooks tvT
    av  <- force legacyHooks aT
    tv  <- requireTVarPrim "writeTVar" tvv
    atomically (writeTVar tv av)
    pure VUnit

modifyTVar'B :: IO Val
modifyTVar'B = pure $ VFun $ \tvT -> pure $ VFun $ \fT -> pure $ VIO $ do
    tvv <- force legacyHooks tvT
    fv  <- force legacyHooks fT
    tv  <- requireTVarPrim "modifyTVar'" tvv
    cur  <- atomically (readTVar tv)
    curT <- newWHNFThunk cur
    new  <- apply legacyHooks fv curT
    atomically (writeTVar tv new)
    pure VUnit

readTVarIOB :: IO Val
readTVarIOB = pure $ VFun $ \tvT -> pure $ VIO $ do
    tvv <- force legacyHooks tvT
    tv  <- requireTVarPrim "readTVarIO" tvv
    readTVarIO tv

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
-- Since ihc is single-threaded at the eval level, an STM action IS an
-- IO action in our world — we just apply the state-transformer.
atomicallyHashB :: IO Val
atomicallyHashB = pure $ VFun $ \stmT -> pure $ VFun $ \sT -> do
    stmV <- force legacyHooks stmT
    rRaw <- apply legacyHooks stmV sT
    v    <- runIOVal legacyHooks rRaw
    ensureStatePair v

-- | @retry# :: State# RealWorld -> (# State# RealWorld, a #)@.
--
-- In a concurrent runtime this blocks until a watched TVar changes.
-- Since we're single-threaded, a retry can never succeed — treat it
-- as an exception (the host 'atomically' call would do the same on
-- the underlying 'BlockedIndefinitelyOnSTM').
retryHashB :: IO Val
retryHashB = pure $ VFun $ \_sT -> atomically retry

-- | @catchRetry# :: (State# RealWorld -> (# State# RealWorld, a #))
--                -> (State# RealWorld -> (# State# RealWorld, a #))
--                -> State# RealWorld
--                -> (# State# RealWorld, a #)@
--
-- Source-loaded: @orElse (STM m) e = STM $ \\s -> catchRetry# m (unSTM e) s@.
-- Try the first action; if it raises a retry-like exception, fall
-- back to the second.
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
        Left _  -> do
            v <- runAction bV
            ensureStatePair v

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

throwIOB :: IO Val
throwIOB = pure $ VFun $ \aT -> pure $ VIO $ do
    av  <- force legacyHooks aT
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
    rRes <- CE.try @IhcException (CE.try @SomeException runAction)
    case rRes of
        Right (Right v) -> ensurePair v
        Right (Left se) -> do
            -- Non-IhcException host error — wrap & hand to handler.
            let msg = BC.pack (show se)
            excT <- newWHNFThunk (VStr msg)
            invokeHandler hV excT
        Left exc -> do
            excVal <- ihcExceptionToVal exc
            excT   <- newWHNFThunk excVal
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

-- | @fromException :: Exception e => SomeException -> Maybe e@ — inverse
-- of 'toException'. In real GHC the dispatch is type-directed; at the Val
-- level there is no type to check against, so we always unwrap and wrap in
-- 'Just'. Source-loaded @catch@ uses this to route handlers: returning
-- 'Just' keeps the exception value flowing to the user handler (which is
-- what we want), 'Nothing' would rethrow.
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
    -- Defaulting to 'Nothing' is the safe answer for exception-type
    -- DOWNCASTS — the common pattern in warp / wai / standard handler
    -- code.  Code that genuinely wants the wrapped value can pattern
    -- match @SomeException e <- ...@ directly (we wrap host
    -- exceptions in 'VCon "SomeException" [innerT]', and the record-
    -- accessor / matchPat legacyHooks paths already descend through the wrap).
    _ <- force legacyHooks eT
    pure (VCon "Nothing" [])

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

catchB :: IO Val
catchB = pure $ VFun $ \aT -> pure $ VFun $ \hT -> pure $ VIO $ do
    av <- force legacyHooks aT
    hv <- force legacyHooks hT
    catch
        (catch
            (runIOVal legacyHooks av)
            (\(exc :: IhcException) -> do
                excVal <- ihcExceptionToVal exc
                excT   <- newWHNFThunk excVal
                rv     <- apply legacyHooks hv excT
                runIOVal legacyHooks rv))
        (\(exc :: SomeException) -> do
            excVal <- hostExceptionToVal exc
            excT   <- newWHNFThunk excVal
            rv     <- apply legacyHooks hv excT
            runIOVal legacyHooks rv)

handleB :: IO Val
handleB = pure $ VFun $ \hT -> pure $ VFun $ \aT -> pure $ VIO $ do
    hv <- force legacyHooks hT
    av <- force legacyHooks aT
    catch
        (catch
            (runIOVal legacyHooks av)
            (\(exc :: IhcException) -> do
                excVal <- ihcExceptionToVal exc
                excT   <- newWHNFThunk excVal
                rv     <- apply legacyHooks hv excT
                runIOVal legacyHooks rv))
        (\(exc :: SomeException) -> do
            let msg = BC.pack (show exc)
            excT <- newWHNFThunk (VStr msg)
            rv   <- apply legacyHooks hv excT
            runIOVal legacyHooks rv)

-- | @Control.Exception.try :: Exception e => IO a -> IO (Either e a)@.
--
-- The previous implementation only caught 'IhcException' (the
-- interpreter-thrown wrapper for source-level @throw@ / @throwIO@).
-- Anything thrown by a host-backed builtin — most notably the
-- 'IOException's that 'Network.Socket' operations raise — slipped
-- straight past, terminating the calling thread silently.  Warp's
-- @acceptNewConnection@ depends on @try@ catching the syscall errors
-- that @accept@/@setSocketOption@ throw, so this regression caused
-- @runSettings@ to bail after one iteration of the accept loop with
-- exit code 0 and no diagnostic output.
--
-- Now we catch @SomeException@ and convert it to a Val:
--   * 'IhcException' is unwrapped to its embedded 'Val' (preserves
--     source-level @throw v@ semantics).
--   * Anything else is materialised via 'hostExceptionToVal' as a
--     stub @IOError@ record so source code that pattern-matches on
--     fields like 'ioe_errno' doesn't blow up.
tryB :: IO Val
tryB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    r  <- CE.try @SomeException (runIOVal legacyHooks av)
    case r of
        Right v -> do
            vT <- newWHNFThunk v
            pure (VCon "Right" [vT])
        Left e -> do
            excVal <- case CE.fromException e of
                Just (ihcExc :: IhcException) -> ihcExceptionToVal ihcExc
                Nothing                       -> hostExceptionToVal e
            excT <- newWHNFThunk excVal
            pure (VCon "Left" [excT])

-- | Convert a host-thrown 'SomeException' into a Val that source-level
-- pattern matching on 'IOError' can introspect.  Most Haskell code in
-- the ecosystem reaches for 'ioe_errno', 'ioeGetErrorType', and
-- 'displayException', so we materialise a record that supplies these
-- fields with conservative defaults (no errno, OtherError type, the
-- exception's @show@ as description).
hostExceptionToVal :: SomeException -> IO Val
hostExceptionToVal e = do
    descT <- newWHNFThunk =<< stringToListValIO (show e)
    handleT <- newWHNFThunk (VCon "Nothing" [])
    typeT <- newWHNFThunk (VCon "OtherError" [])
    locT <- newWHNFThunk =<< stringToListValIO ""
    errnoT <- newWHNFThunk (VCon "Nothing" [])
    fileT <- newWHNFThunk (VCon "Nothing" [])
    -- Wrap in 'SomeException' so handlers that pattern-match
    -- @\(SomeException e) -> ...@ (e.g. warp's 'throughAsync',
    -- 'settingsOnException') see the expected ctor.  Existing handlers
    -- that match on the inner @IOError@ ctor still work via
    -- newtype-transparent pattern matching: 'matchPat' on
    -- @PCon "IOError"@ against a single-field @VCon "SomeException"@
    -- projects the inner field and retries.
    let ioErrVal = VCon "IOError" [handleT, typeT, locT, descT, errnoT, fileT]
    ioErrT <- newWHNFThunk ioErrVal
    pure (VCon "SomeException" [ioErrT])

evaluateB :: IO Val
evaluateB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    _  <- evaluate av
    pure av

mask_B :: IO Val
mask_B = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    mask_ (runIOVal legacyHooks av)

allowInterruptB :: IO Val
allowInterruptB = pure $ VIO $ pure VUnit

interruptibleB :: IO Val
interruptibleB = pure $ VFun $ \aT -> pure $ VIO $ do
    av <- force legacyHooks aT
    runIOVal legacyHooks av

maskB :: IO Val
maskB = pure $ VFun $ \fT -> pure $ VIO $ do
    fv <- force legacyHooks fT
    mask $ \restore -> do
        let restoreVal = VFun $ \aT -> pure $ VIO $ do
                av <- force legacyHooks aT
                restore (runIOVal legacyHooks av)
        restoreT <- newWHNFThunk restoreVal
        rv <- apply legacyHooks fv restoreT
        runIOVal legacyHooks rv

bracketB :: IO Val
bracketB = pure $ VFun $ \acqT -> pure $ VFun $ \relT -> pure $ VFun $ \useT -> pure $ VIO $ do
    acqV <- force legacyHooks acqT
    relV <- force legacyHooks relT
    useV <- force legacyHooks useT
    bracket
        (runIOVal legacyHooks acqV)
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply legacyHooks relV resT
            _    <- runIOVal legacyHooks rv
            pure ())
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply legacyHooks useV resT
            runIOVal legacyHooks rv)

bracketOnErrorB :: IO Val
bracketOnErrorB = pure $ VFun $ \acqT -> pure $ VFun $ \relT -> pure $ VFun $ \useT -> pure $ VIO $ do
    acqV <- force legacyHooks acqT
    relV <- force legacyHooks relT
    useV <- force legacyHooks useT
    bracketOnError
        (runIOVal legacyHooks acqV)
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply legacyHooks relV resT
            _    <- runIOVal legacyHooks rv
            pure ())
        (\res -> do
            resT <- newWHNFThunk res
            rv   <- apply legacyHooks useV resT
            runIOVal legacyHooks rv)

bracket_B :: IO Val
bracket_B = pure $ VFun $ \befT -> pure $ VFun $ \aftT -> pure $ VFun $ \thingT -> pure $ VIO $ do
    befV   <- force legacyHooks befT
    aftV   <- force legacyHooks aftT
    thingV <- force legacyHooks thingT
    bracket_
        (runIOVal legacyHooks befV >> pure ())
        (runIOVal legacyHooks aftV >> pure ())
        (runIOVal legacyHooks thingV)

finallyB :: IO Val
finallyB = pure $ VFun $ \aT -> pure $ VFun $ \cleanT -> pure $ VIO $ do
    av     <- force legacyHooks aT
    cleanV <- force legacyHooks cleanT
    finally
        (runIOVal legacyHooks av)
        (runIOVal legacyHooks cleanV >> pure ())

onExceptionB :: IO Val
onExceptionB = pure $ VFun $ \aT -> pure $ VFun $ \cleanT -> pure $ VIO $ do
    av     <- force legacyHooks aT
    cleanV <- force legacyHooks cleanT
    onException
        (runIOVal legacyHooks av)
        (runIOVal legacyHooks cleanV >> pure ())

throwToB :: IO Val
throwToB = pure $ VFun $ \tidT -> pure $ VFun $ \excT -> pure $ VIO $ do
    tidV <- force legacyHooks tidT
    excV <- force legacyHooks excT
    case tidV of
        VPrimObj (PrimThreadId tid) -> do
            exc <- valToIhcException excV
            throwTo tid exc
            pure VUnit
        _ -> error ("throwTo: not a ThreadId: " <> showValForDebug tidV)

displayExceptionB :: IO Val
displayExceptionB = pure $ VFun $ \eT -> do
    ev <- force legacyHooks eT
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
    dictV <- force legacyHooks dictT
    extractTypeRep dictV

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

toDynB :: IO Val
toDynB = pure $ VFun $ \dictT -> pure $ VFun $ \valT -> do
    dictV <- force legacyHooks dictT
    tr    <- extractTypeRep dictV
    trT   <- newWHNFThunk tr
    pure (VCon "Dynamic" [trT, valT])

fromDynamicB :: IO Val
fromDynamicB = pure $ VFun $ \dictBT -> pure $ VFun $ \dynT -> do
    dictBV <- force legacyHooks dictBT
    dynV   <- force legacyHooks dynT
    trB    <- extractTypeRep dictBV
    case dynV of
        VCon "Dynamic" [trAT, storedT] -> do
            trA <- force legacyHooks trAT
            eq  <- typeRepEq trA trB
            pure (if eq then VCon "Just" [storedT] else VCon "Nothing" [])
        _ -> pure (VCon "Nothing" [])

fromDynB :: IO Val
fromDynB = pure $ VFun $ \dictT -> pure $ VFun $ \dynT -> pure $ VFun $ \defT -> do
    dictV <- force legacyHooks dictT
    dynV  <- force legacyHooks dynT
    trB   <- extractTypeRep dictV
    case dynV of
        VCon "Dynamic" [trAT, storedT] -> do
            trA <- force legacyHooks trAT
            eq  <- typeRepEq trA trB
            if eq then force legacyHooks storedT else force legacyHooks defT
        _ -> force legacyHooks defT

dynTypeRepB :: IO Val
dynTypeRepB = pure $ VFun $ \dynT -> do
    dynV <- force legacyHooks dynT
    case dynV of
        VCon "Dynamic" [trT, _] -> force legacyHooks trT
        _ -> mkTypeRep "Unknown"

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

tyConNameB :: IO Val
tyConNameB = pure $ VFun $ \tyConT -> do
    tyConV <- force legacyHooks tyConT
    case tyConV of
        VCon "TyCon" [nameT] -> force legacyHooks nameT
        _ -> pure (VCon "[]" [])

typeRepTyConB :: IO Val
typeRepTyConB = pure $ VFun $ \trT -> do
    trV <- force legacyHooks trT
    case trV of
        VCon "TypeRep" [tcT, _] -> force legacyHooks tcT
        _ -> pure (VCon "TyCon" [])

typeRepArgsB :: IO Val
typeRepArgsB = pure $ VFun $ \trT -> do
    trV <- force legacyHooks trT
    case trV of
        VCon "TypeRep" [_, argsT] -> force legacyHooks argsT
        _ -> pure (VCon "[]" [])

-- | Extract a TypeRep from a Typeable dict or raw TypeRep value.
extractTypeRep :: Val -> IO Val
extractTypeRep (VCon "Dict_Typeable" [trT]) = force legacyHooks trT
extractTypeRep v@(VCon "TypeRep" _)         = pure v
extractTypeRep _                            = mkTypeRep "Unknown"

-- | Dynamic constructor as a curried function value.
dynamicCtorB :: IO Val
dynamicCtorB = pure $ VFun $ \trT -> pure $ VFun $ \valT -> do
    trV    <- force legacyHooks trT
    trThunk <- newWHNFThunk trV
    pure (VCon "Dynamic" [trThunk, valT])

-- | Build built-in Typeable instance dictionaries for well-known types.
--
-- Each dict is registered as a 'LazyBuiltin' thunk so startup doesn't pay
-- the cost of allocating a TypeRep + wrapper VCon for every primitive
-- type — a hello-world program never touches any of these.
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
        dictT <- newLazyBuiltinThunk $ do
            tr  <- mkTypeRep tyName
            trT <- newWHNFThunk tr
            pure (VCon "Dict_Typeable" [trT])
        pure ("typeableDict_" <> tag, dictT)
