-- | Hand-rolled aarch64 instruction encoders — enough for Phase 1.0.
-- Expand only as new primitives land. If this file grows past a few
-- dozen instructions, replace with a proper stencil/table.
module IHC.Encode
    ( -- * Moves
      movzImm
    , movkImm
      -- * Return / branch
    , retX30
      -- * Composed: materialize a signed 64-bit into a register
    , loadInt64
    ) where

import Data.Bits ((.&.), shiftR, shiftL, (.|.))
import Data.Word (Word32, Word64)
import Data.Int (Int64)

-- | @MOVZ Xd, #imm16, LSL (hw*16)@ — 64-bit.
-- Encoding: 1 | 10 | 100101 | hw(2) | imm16(16) | Rd(5).
movzImm :: Int -> Int -> Word32 -> Word32
movzImm rd hw imm16 =
    0xD2800000
    .|. ((fromIntegral hw    .&. 0x3)    `shiftL` 21)
    .|. ((imm16              .&. 0xFFFF) `shiftL` 5)
    .|.  (fromIntegral rd    .&. 0x1F)

-- | @MOVK Xd, #imm16, LSL (hw*16)@ — 64-bit.
-- Encoding: 1 | 11 | 100101 | hw(2) | imm16(16) | Rd(5).
movkImm :: Int -> Int -> Word32 -> Word32
movkImm rd hw imm16 =
    0xF2800000
    .|. ((fromIntegral hw    .&. 0x3)    `shiftL` 21)
    .|. ((imm16              .&. 0xFFFF) `shiftL` 5)
    .|.  (fromIntegral rd    .&. 0x1F)

-- | @RET x30@.
retX30 :: Word32
retX30 = 0xD65F03C0

-- | Materialize a 64-bit integer into register @Xrd@, using up to four
-- MOVZ/MOVK instructions (covers the full 64-bit range). For small
-- positive values only one instruction is emitted.
--
-- Returns the list of instructions ready to be emitted in order.
loadInt64 :: Int -> Int64 -> [Word32]
loadInt64 rd n = build firstHw [] (fromIntegral n :: Word64) True
  where
    firstHw = 0 :: Int
    build hw acc word isFirst
        | hw > 3       = reverse acc
        | chunk == 0 && not isFirst
                       = build (hw + 1) acc word isFirst
        | isFirst      = build (hw + 1) (movzImm rd hw chunkW32 : acc) word False
        | otherwise    = build (hw + 1) (movkImm rd hw chunkW32 : acc) word False
      where
        chunk    = (word `shiftR` (16 * hw)) .&. 0xFFFF
        chunkW32 = fromIntegral chunk :: Word32
