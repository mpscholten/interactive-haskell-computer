{-# LANGUAGE ForeignFunctionInterface #-}

-- | Phase-0 sanity: allocate a MAP_JIT page, write a two-instruction aarch64
-- program that returns the integer 42, flip the page to executable, call it,
-- and assert we got 42 back.
--
-- If this passes, our MAP_JIT + W/X + I-cache flush + codesign setup is
-- sound. Everything else in the interpreter is built on top of this.
module JitSmoke (spec) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, FunPtr, castPtrToFunPtr, castPtr)
import Test.Hspec

import IHC.Jit

-- | aarch64 MOVZ (64-bit): @mov Xd, #imm16, LSL 0@.
--
-- Encoding:  1 | 10 | 100101 | hw(2) | imm16(16) | Rd(5)
--            sf=1, opc=10 (MOVZ), hw=00 (no shift).
movzImm :: Int -> Word32 -> Word32
movzImm rd imm16 =
    0xD2800000
    .|. ((imm16 .&. 0xFFFF) `shiftL` 5)
    .|. (fromIntegral rd .&. 0x1F)

-- | @ret x30@ — return to link register.
ret :: Word32
ret = 0xD65F03C0

encodeInsns :: [Word32] -> ByteString
encodeInsns = BS.pack . concatMap le4
  where
    le4 :: Word32 -> [Word8]
    le4 w =
        [ fromIntegral  (w               .&. 0xFF)
        , fromIntegral ((w `shiftR`  8)  .&. 0xFF)
        , fromIntegral ((w `shiftR` 16)  .&. 0xFF)
        , fromIntegral ((w `shiftR` 24)  .&. 0xFF)
        ]

foreign import ccall "dynamic"
    mkCallInt :: FunPtr (IO Int) -> IO Int

spec :: Spec
spec = describe "JIT smoke test (MAP_JIT + W^X + flush + exec)" do
    it "executes `mov x0, #42 ; ret` and returns 42" do
        runJitProgram [movzImm 0 42, ret] `shouldReturnIO` 42

    it "survives a second page alloc+exec round-trip" do
        runJitProgram [movzImm 0 7, ret] `shouldReturnIO` 7

    it "handles a different immediate (proves we're not reading a stale page)" do
        runJitProgram [movzImm 0 1234, ret] `shouldReturnIO` 1234

-- | Allocate, write, flip to executable, flush, call, free.
runJitProgram :: [Word32] -> IO Int
runJitProgram insns = do
    pageSz <- jitPageSize
    p <- jitAlloc pageSz
    let bs = encodeInsns insns
    jitWritable
    BS.useAsCStringLen bs \(src, len) ->
        copyBytes (castPtr p) (castPtr src) len
    jitExecutable
    jitFlush p (BS.length bs)
    let fn = castPtrToFunPtr p :: FunPtr (IO Int)
    n <- mkCallInt fn
    jitFree p pageSz
    pure n

shouldReturnIO :: (Eq a, Show a) => IO a -> a -> Expectation
shouldReturnIO action expected = do
    got <- action
    got `shouldBe` expected
