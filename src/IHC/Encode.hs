-- | Hand-rolled aarch64 instruction encoders — enough for Phase 1.0.
-- Expand only as new primitives land. If this file grows past a few
-- dozen instructions, replace with a proper stencil/table.
module IHC.Encode
    ( -- * Moves
      movzImm
    , movkImm
      -- * Return / branch
    , retX30
      -- * Arithmetic (register-register)
    , addXXX
    , subXXX
    , mulXXX
      -- * Stack push/pop (single 64-bit register)
    , pushX0
    , popX1
      -- * Function prologue / epilogue (16-byte frame — no arg slot)
    , pushFpLr
    , popFpLr
    , movFpSp
      -- * Function prologue / epilogue (32-byte frame — includes arg slot)
    , pushFpLr32
    , popFpLr32
    , strX0ArgSlot
    , ldrX0ArgSlot
      -- * Indirect call
    , blrX16
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

-- | @ADD Xd, Xn, Xm@ (64-bit, shift=0).
-- Encoding: 1 | 00 | 01011 | 00 | 0 | Rm(5) | 000000 | Rn(5) | Rd(5).
addXXX :: Int -> Int -> Int -> Word32
addXXX rd rn rm =
    0x8B000000
    .|. (fromIntegral rm .&. 0x1F) `shiftL` 16
    .|. (fromIntegral rn .&. 0x1F) `shiftL` 5
    .|.  fromIntegral rd .&. 0x1F

-- | @SUB Xd, Xn, Xm@ (64-bit, shift=0).
-- Encoding: 1 | 10 | 01011 | 00 | 0 | Rm(5) | 000000 | Rn(5) | Rd(5).
subXXX :: Int -> Int -> Int -> Word32
subXXX rd rn rm =
    0xCB000000
    .|. (fromIntegral rm .&. 0x1F) `shiftL` 16
    .|. (fromIntegral rn .&. 0x1F) `shiftL` 5
    .|.  fromIntegral rd .&. 0x1F

-- | @MUL Xd, Xn, Xm@ (64-bit) — alias for MADD Xd,Xn,Xm,XZR.
-- Encoding: 1 | 00 | 11011 | 000 | Rm(5) | 0 | 11111 | Rn(5) | Rd(5).
mulXXX :: Int -> Int -> Int -> Word32
mulXXX rd rn rm =
    0x9B007C00
    .|. (fromIntegral rm .&. 0x1F) `shiftL` 16
    .|. (fromIntegral rn .&. 0x1F) `shiftL` 5
    .|.  fromIntegral rd .&. 0x1F

-- | @STR X0, [SP, #-16]!@ — pre-index store, decrements SP by 16.
-- Keeps SP 16-byte aligned (Apple ABI).
pushX0 :: Word32
pushX0 = 0xF81F0FE0

-- | @LDR X1, [SP], #16@ — post-index load, increments SP by 16.
popX1 :: Word32
popX1 = 0xF84107E1

-- | @STP X29, X30, [SP, #-16]!@ — push FP+LR pair, decrement SP by 16.
pushFpLr :: Word32
pushFpLr = 0xA9BF7BFD

-- | @LDP X29, X30, [SP], #16@ — pop FP+LR pair, increment SP by 16.
popFpLr :: Word32
popFpLr = 0xA8C17BFD

-- | @MOV X29, SP@ — alias for @ADD X29, SP, #0@.
movFpSp :: Word32
movFpSp = 0x910003FD

-- | @BLR X16@ — branch with link through register x16 (scratch).
blrX16 :: Word32
blrX16 = 0xD63F0200

-- | @STP X29, X30, [SP, #-32]!@ — push FP+LR, allocate 32-byte frame.
-- Layout after mov x29, sp:
--   [fp + 0]  saved old fp
--   [fp + 8]  saved old lr
--   [fp + 16] arg-slot (written by 'strX0ArgSlot')
--   [fp + 24] padding
pushFpLr32 :: Word32
pushFpLr32 = 0xA9BE7BFD

-- | @LDP X29, X30, [SP], #32@ — pop FP+LR, deallocate 32-byte frame.
popFpLr32 :: Word32
popFpLr32 = 0xA8C27BFD

-- | @STR X0, [SP, #16]@ — save the first argument to the arg slot.
-- Only valid immediately after a 32-byte 'pushFpLr32' prologue, before
-- any further SP changes.
--
-- Encoding: 0xF9000000 base, (imm12/8)=2 -> (2<<10)=0x800, Rn=SP=0x3E0.
-- 0x800 | 0x3E0 = 0xBE0. Writing 0x7E0 here would target [sp,#8] and
-- corrupt the saved LR.
strX0ArgSlot :: Word32
strX0ArgSlot = 0xF9000BE0

-- | @LDR X0, [X29, #16]@ — load the first argument from the arg slot.
-- Uses FP so it remains correct even if SP has moved inside the body
-- (e.g. via pushX0 spills during expression evaluation).
ldrX0ArgSlot :: Word32
ldrX0ArgSlot = 0xF9400BA0

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
