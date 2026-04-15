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
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, castPtr, ptrToWordPtr)

import IHC.CodeBuffer
import IHC.Encode
import IHC.IR

emitBinding
    :: CodeBuffer
    -> Map ByteString (Ptr ())     -- name -> code address
    -> Map ByteString CString      -- string-literal content -> pool ptr
    -> Binding
    -> IO ()
emitBinding cb addrs strs b = do
    let arity = length (bindParams b)
    emitPrologue cb arity
    mapM_ (emitItem cb addrs strs) (bindItems b)
    emitEpilogue cb arity

emitPrologue :: CodeBuffer -> Int -> IO ()
emitPrologue cb arity
    | arity == 0 = do
        emitInsn cb pushFpLr
        emitInsn cb movFpSp
    | arity <= 2 = do
        emitInsn cb pushFpLr32       -- 32-byte frame
        emitInsn cb movFpSp
        mapM_ (\i -> emitInsn cb (strXnToSp i (16 + 8 * i))) [0 .. arity - 1]
    | arity <= 4 = do
        emitInsn cb pushFpLr48       -- 48-byte frame
        emitInsn cb movFpSp
        mapM_ (\i -> emitInsn cb (strXnToSp i (16 + 8 * i))) [0 .. arity - 1]
    | otherwise =
        error ("IHC.Emit.emitPrologue: arity > 4 not supported yet: " <> show arity)

emitEpilogue :: CodeBuffer -> Int -> IO ()
emitEpilogue cb arity = do
    case arity of
        0                 -> emitInsn cb popFpLr
        n | n <= 2        -> emitInsn cb popFpLr32
          | n <= 4        -> emitInsn cb popFpLr48
          | otherwise     -> error ("IHC.Emit.emitEpilogue: arity > 4 not supported yet: " <> show n)
    emitInsn cb retX30

emitItem :: CodeBuffer -> Map ByteString (Ptr ()) -> Map ByteString CString -> Item -> IO ()
emitItem cb addrs strs = \case
    IPushX0   -> emitInsn cb pushX0
    IPopX1    -> emitInsn cb popX1
    ILitInt n -> emitInsns cb (loadInt64 0 n)
    ILitStr s -> case Map.lookup s strs of
        Just p  -> emitInsns cb (loadAddr64 0 (castPtr p))
        Nothing -> error "IHC.Emit: ILitStr content missing from string pool"
    IAddX1X0  -> emitInsn cb (addXXX 0 1 0)
    ISubX1X0  -> emitInsn cb (subXXX 0 1 0)
    IMulX1X0  -> emitInsn cb (mulXXX 0 1 0)
    ICmp cond -> do
        emitInsn cb cmpX1X0
        emitInsn cb (csetX0Cond cond)
    IAndX1X0  -> emitInsn cb (andXXX 0 1 0)
    IOrX1X0   -> emitInsn cb (orrXXX 0 1 0)
    INegX0    -> emitInsn cb negX0X0
    IArg i    -> emitInsn cb (ldrXnFromFp 0 (16 + 8 * i))
    ICall nm arity -> do
        case arity of
            0 -> pure ()
            _ -> do
                mapM_ (\i -> emitInsn cb (ldrXnFromSp i ((arity - 1 - i) * 16)))
                      [0 .. arity - 1]
                emitInsn cb (addSpImm (arity * 16))
        callTo cb addrs nm
    IIfThenElse cond thenB elseB -> do
        mapM_ (emitItem cb addrs strs) cond
        let thenSize = sizeOfItems thenB
            elseSize = sizeOfItems elseB
            cbzInsts = (thenSize + 8) `div` 4
            bInsts   = (elseSize + 4) `div` 4
        emitInsn cb (cbzX0Offset cbzInsts)
        mapM_ (emitItem cb addrs strs) thenB
        emitInsn cb (bOffset bInsts)
        mapM_ (emitItem cb addrs strs) elseB

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
