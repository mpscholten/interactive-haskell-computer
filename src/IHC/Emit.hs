-- | Expands a 'Binding' to aarch64 in a 'CodeBuffer', resolving
-- identifier references against a name -> address map.
--
-- Address loads use a fixed 4-instruction @movz/movk@ chain so the
-- pre-emit byte size ('sizeOfItem' @(ICall _)@ = 20) stays
-- deterministic, which the scheduler relies on for layout.
module IHC.Emit
    ( emitBinding
    , emitItem
    ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, ptrToWordPtr)

import IHC.CodeBuffer
import IHC.Encode
import IHC.IR

-- | Emit a full binding: arity-dependent prologue, items, matching epilogue.
emitBinding :: CodeBuffer -> Map ByteString (Ptr ()) -> Binding -> IO ()
emitBinding cb addrs b = do
    -- Prologue.
    if null (bindParams b)
        then do
            emitInsn cb pushFpLr
            emitInsn cb movFpSp
        else do
            emitInsn cb pushFpLr32
            emitInsn cb movFpSp
            emitInsn cb strX0ArgSlot

    -- Body.
    mapM_ (emitItem cb addrs) (bindItems b)

    -- Epilogue.
    if null (bindParams b)
        then emitInsn cb popFpLr
        else emitInsn cb popFpLr32
    emitInsn cb retX30

emitItem :: CodeBuffer -> Map ByteString (Ptr ()) -> Item -> IO ()
emitItem cb addrs = \case
    IPushX0   -> emitInsn cb pushX0
    IPopX1    -> emitInsn cb popX1
    ILitInt n -> emitInsns cb (loadInt64 0 n)
    IAddX1X0  -> emitInsn cb (addXXX 0 1 0)
    ISubX1X0  -> emitInsn cb (subXXX 0 1 0)
    IMulX1X0  -> emitInsn cb (mulXXX 0 1 0)
    ICmpLe    -> do
        emitInsn cb cmpX1X0
        emitInsn cb csetLeX0
    IArg      -> emitInsn cb ldrX0ArgSlot
    ICall nm  -> callTo cb addrs nm
    ICall1 nm -> callTo cb addrs nm
    IIfThenElse cond thenB elseB -> do
        -- Emit the condition, leaving 0 or 1 in x0.
        mapM_ (emitItem cb addrs) cond
        -- PC-relative distances in instructions.
        --
        -- Layout from the CBZ instruction:
        --   +0:                  cbz x0, ...        (this insn)
        --   +4:                  then-items         (thenSize bytes)
        --   +4 + thenSize:       b ...              (jump past else)
        --   +4 + thenSize + 4:   else-items         (elseSize bytes)
        --   +8 + thenSize + elseSize: instruction after if
        let thenSize = sizeOfItems thenB
            elseSize = sizeOfItems elseB
            -- CBZ target = first instruction of else. Distance from CBZ
            -- in instructions = (4 + thenSize + 4) / 4 = thenSize/4 + 2.
            cbzInsts = (thenSize + 8) `div` 4
            -- B is at +4+thenSize. Target = +8+thenSize+elseSize.
            -- Distance = (4 + elseSize) / 4 = elseSize/4 + 1.
            bInsts   = (elseSize + 4) `div` 4
        emitInsn cb (cbzX0Offset cbzInsts)
        mapM_ (emitItem cb addrs) thenB
        emitInsn cb (bOffset bInsts)
        mapM_ (emitItem cb addrs) elseB

callTo :: CodeBuffer -> Map ByteString (Ptr ()) -> ByteString -> IO ()
callTo cb addrs nm = case Map.lookup nm addrs of
    Nothing -> error ("IHC.Emit: unresolved call to `" <> BC.unpack nm <> "`")
    Just p  -> do
        emitInsns cb (loadAddr64 16 p)
        emitInsn  cb blrX16

-- | Load a 64-bit pointer using a fixed 4-insn movz/movk chain, so
-- the emitted size is constant regardless of target address.
loadAddr64 :: Int -> Ptr () -> [Word32]
loadAddr64 rd p =
    [ movzImm rd 0 (chunk 0)
    , movkImm rd 1 (chunk 1)
    , movkImm rd 2 (chunk 2)
    , movkImm rd 3 (chunk 3)
    ]
  where
    w :: Word64
    w = fromIntegral (ptrToWordPtr p)
    chunk hw = fromIntegral ((w `shiftR` (16 * hw)) .&. 0xFFFF)
