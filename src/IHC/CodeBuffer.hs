-- | A growing JIT code buffer with convenience helpers for emitting
-- aarch64 instructions. A single 'CodeBuffer' owns exactly one MAP_JIT
-- page (Phase 1.0 — we'll grow into multi-page allocators later).
--
-- Usage pattern:
--
-- >>> cb <- newCodeBuffer 16384
-- >>> entry <- withWritable cb $ \cb' -> do
-- >>>     p <- currentAddr cb'
-- >>>     emitInsn cb' 0xD2800540       -- mov x0, #42
-- >>>     emitInsn cb' 0xD65F03C0       -- ret
-- >>>     pure p
-- >>> callInt entry
--
-- 'withWritable' toggles the thread-local JIT write protection and
-- flushes the I-cache on exit, so callers don't have to think about
-- W^X.
module IHC.CodeBuffer
    ( CodeBuffer(..)
    , newCodeBuffer
    , freeCodeBuffer
    , currentAddr
    , currentOffset
    , withWritable
    , emitInsn
    , emitInsns
    , callInt
    ) where

import Data.Bits ((.&.), shiftR)
import Data.IORef
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, FunPtr, castPtrToFunPtr, plusPtr)
import Foreign.Storable (poke)

import IHC.Jit

-- | A single JIT page plus a bump pointer (in bytes from the base).
data CodeBuffer = CodeBuffer
    { cbBase :: !(Ptr ())
    , cbSize :: !Int
    , cbOff  :: !(IORef Int)
    }

newCodeBuffer :: Int -> IO CodeBuffer
newCodeBuffer requested = do
    page <- jitPageSize
    let sz = max requested page
    base <- jitAlloc sz
    off  <- newIORef 0
    pure (CodeBuffer base sz off)

freeCodeBuffer :: CodeBuffer -> IO ()
freeCodeBuffer cb = jitFree (cbBase cb) (cbSize cb)

currentAddr :: CodeBuffer -> IO (Ptr ())
currentAddr cb = do
    o <- readIORef (cbOff cb)
    pure (cbBase cb `plusPtr` o)

currentOffset :: CodeBuffer -> IO Int
currentOffset cb = readIORef (cbOff cb)

-- | Bracket an emission: make the page writable, run the action, make it
-- executable again, and flush the I-cache for the newly-written range.
withWritable :: CodeBuffer -> (CodeBuffer -> IO a) -> IO a
withWritable cb act = do
    startOff <- readIORef (cbOff cb)
    jitWritable
    r <- act cb
    jitExecutable
    endOff <- readIORef (cbOff cb)
    let len = endOff - startOff
    jitFlush (cbBase cb `plusPtr` startOff) len
    pure r

-- | Emit a 32-bit aarch64 instruction in native little-endian order.
-- Must be called inside 'withWritable'.
emitInsn :: CodeBuffer -> Word32 -> IO ()
emitInsn cb insn = do
    o <- readIORef (cbOff cb)
    let p = cbBase cb `plusPtr` o :: Ptr Word8
    pokeLE32 p insn
    writeIORef (cbOff cb) (o + 4)

emitInsns :: CodeBuffer -> [Word32] -> IO ()
emitInsns cb = mapM_ (emitInsn cb)

pokeLE32 :: Ptr Word8 -> Word32 -> IO ()
pokeLE32 p w = do
    poke (p           ) (fromIntegral  (w              .&. 0xFF) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral ((w `shiftR`  8) .&. 0xFF) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)

-- | Call an emitted entry point as @() -> Int@.
foreign import ccall "dynamic"
    mkCallInt :: FunPtr (IO Int) -> IO Int

callInt :: Ptr () -> IO Int
callInt p = mkCallInt (castPtrToFunPtr p)
