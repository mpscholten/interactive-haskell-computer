-- | Expands an 'Item' list to aarch64 in a 'CodeBuffer', resolving
-- identifier references against a name -> address map.
--
-- Address loads use a fixed 4-instruction @movz/movk@ chain so the
-- pre-emit byte size ('sizeOfItem' @(ICall _)@ = 20) is deterministic,
-- which the scheduler relies on to lay out binding entry points.
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

-- | Emit a full binding: uniform prologue, items, uniform epilogue.
emitBinding :: CodeBuffer -> Map ByteString (Ptr ()) -> [Item] -> IO ()
emitBinding cb addrs items = do
    emitInsn cb pushFpLr
    emitInsn cb movFpSp
    mapM_ (emitItem cb addrs) items
    emitInsn cb popFpLr
    emitInsn cb retX30

emitItem :: CodeBuffer -> Map ByteString (Ptr ()) -> Item -> IO ()
emitItem cb addrs = \case
    IPushX0   -> emitInsn cb pushX0
    IPopX1    -> emitInsn cb popX1
    ILitInt n -> emitInsns cb (loadInt64 0 n)
    IAddX1X0  -> emitInsn cb (addXXX 0 1 0)
    ISubX1X0  -> emitInsn cb (subXXX 0 1 0)
    IMulX1X0  -> emitInsn cb (mulXXX 0 1 0)
    ICall nm  -> case Map.lookup nm addrs of
        Nothing -> error ("IHC.Emit: unresolved call to `" <> BC.unpack nm <> "`")
        Just p  -> do
            emitInsns cb (loadAddr64 16 p)
            emitInsn  cb blrX16

-- | Load a full 64-bit pointer into a register using a fixed 4-insn
-- @movz/movk@ chain. Unlike 'loadInt64' we always emit four to keep
-- 'ICall' layout size constant regardless of the target address.
loadAddr64 :: Int -> Ptr () -> [Word32]
loadAddr64 rd p =
    [ movzImm rd 0  (chunk 0)
    , movkImm rd 1  (chunk 1)
    , movkImm rd 2  (chunk 2)
    , movkImm rd 3  (chunk 3)
    ]
  where
    w :: Word64
    w = fromIntegral (ptrToWordPtr p)
    chunk :: Int -> Word32
    chunk hw = fromIntegral ((w `shiftR` (16 * hw)) .&. 0xFFFF)
