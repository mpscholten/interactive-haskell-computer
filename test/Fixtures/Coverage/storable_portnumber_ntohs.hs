-- Source Storable.peek of a PortNumber-shaped newtype applies ntohs.
-- network's instance is:
--   peek p = PortNum . ntohs <$> peek (castPtr p)
-- The type name is not the constructor, so type-directed
-- `peek :: IO T` must discover the instance (not a raw Word16 wrap).
import Data.Word (Word8, Word16)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..), peek, pokeByteOff)

newtype PortN = PNum Word16

foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16

instance Storable PortN where
    sizeOf _ = 2
    alignment _ = 2
    poke p (PNum po) = poke (castPtr p) po
    peek p = PNum . ntohs <$> peek (castPtr p)

main :: IO ()
main = allocaBytes 2 $ \p -> do
    -- network-order 80 (00 50).  LE Word16 bits are 0x5000 = 20480.
    pokeByteOff p 0 (0x00 :: Word8)
    pokeByteOff p 1 (0x50 :: Word8)
    n <- peek (castPtr p) :: IO PortN
    case n of
        PNum w -> print (fromIntegral w :: Int)
