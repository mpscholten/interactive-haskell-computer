-- | Expand a 'Binding' to aarch64 in a 'CodeBuffer'.
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

emitBinding :: CodeBuffer -> Map ByteString (Ptr ()) -> Binding -> IO ()
emitBinding cb addrs b = do
    let arity = length (bindParams b)
    emitPrologue cb arity
    mapM_ (emitItem cb addrs) (bindItems b)
    emitEpilogue cb arity

emitPrologue :: CodeBuffer -> Int -> IO ()
emitPrologue cb arity
    | arity == 0 = do
        emitInsn cb pushFpLr
        emitInsn cb movFpSp
    | arity <= 2 = do
        emitInsn cb pushFpLr32       -- 32-byte frame
        emitInsn cb movFpSp
        -- Save x0..x(arity-1) into frame arg slots at [sp, #16], [sp, #24].
        mapM_ (\i -> emitInsn cb (strXnToSp i (16 + 8 * i))) [0 .. arity - 1]
    | otherwise =
        error ("IHC.Emit.emitPrologue: arity > 2 not supported yet: " <> show arity)

emitEpilogue :: CodeBuffer -> Int -> IO ()
emitEpilogue cb arity = do
    if arity == 0
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
    IArg i    -> emitInsn cb (ldrXnFromFp 0 (16 + 8 * i))
    ICall nm arity -> do
        -- Pop @arity@ values from the spill stack into x0..x(arity-1).
        -- After N pushes, the Nth push is on top at [sp, #0], the 1st
        -- push (oldest) is at [sp, #(N-1)*16].
        case arity of
            0 -> pure ()
            _ -> do
                mapM_ (\i -> emitInsn cb (ldrXnFromSp i ((arity - 1 - i) * 16)))
                      [0 .. arity - 1]
                emitInsn cb (addSpImm (arity * 16))
        callTo cb addrs nm
    IIfThenElse cond thenB elseB -> do
        mapM_ (emitItem cb addrs) cond
        let thenSize = sizeOfItems thenB
            elseSize = sizeOfItems elseB
            cbzInsts = (thenSize + 8) `div` 4
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

-- | Fixed 4-insn address-load into register @rd@. Size is constant so
-- 'bindingBytes' can lay out bindings before knowing target addresses.
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
