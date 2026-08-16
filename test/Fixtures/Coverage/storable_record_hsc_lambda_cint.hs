-- hsc2hs emits field reads through an immediately-applied lambda.  The
-- source-defined Storable method must propagate the record field's CInt type
-- through the do bind and into peekByteOff instead of leaving a function.
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)

type ProtocolNumber = CInt

data AddrInfo = AddrInfo
  { aiFlags :: CInt
  , aiFamily :: CInt
  , aiSocketType :: CInt
  , aiProtocol :: ProtocolNumber
  }

instance Storable AddrInfo where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek p = do
    flags <- ((\q -> peekByteOff q 0)) p
    family <- ((\q -> peekByteOff q 4)) p
    socketType <- ((\q -> peekByteOff q 8)) p
    protocol <- ((\q -> peekByteOff q 12)) p
    pure AddrInfo
      { aiFlags = flags
      , aiFamily = family
      , aiSocketType = socketType
      , aiProtocol = protocol
      }
  poke _ _ = pure ()

main :: IO ()
main = alloca $ \p -> do
  pokeByteOff p 12 (7 :: Word8)
  pokeByteOff p 13 (0 :: Word8)
  pokeByteOff p 14 (0 :: Word8)
  pokeByteOff p 15 (0 :: Word8)
  info <- peek p
  print (aiProtocol info)
