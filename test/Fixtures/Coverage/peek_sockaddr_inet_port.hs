-- Expected-type flow from constructor args into unannotated peekByteOff.
-- network's peekSockAddr AF_INET branch is:
--   addr <- peekByteOff p 4
--   port <- peekByteOff p 2
--   return (SockAddrInet port addr)
-- PortNumber is a Word16-sized field; without constructor-arg flow
-- peekByteOff defaults to 1 or 4 bytes and mashes sin_port with sin_addr.
import Data.Word (Word8, Word16, Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peekByteOff, pokeByteOff)

type PortNumber = Word16
type HostAddress = Word32
data SockAddr = SockAddrInet PortNumber HostAddress

main :: IO ()
main = allocaBytes 16 $ \p -> do
    pokeByteOff p 0 (2 :: Word8)
    pokeByteOff p 1 (0 :: Word8)
    -- port 80 as little-endian Word16 (0x0050)
    pokeByteOff p 2 (80 :: Word8)
    pokeByteOff p 3 (0 :: Word8)
    -- HostAddress 127.0.0.1 = 16777343, poked as bytes
    pokeByteOff p 4 (0x7f :: Word8)
    pokeByteOff p 5 (0 :: Word8)
    pokeByteOff p 6 (0 :: Word8)
    pokeByteOff p 7 (1 :: Word8)
    addr <- peekByteOff p 4
    port <- peekByteOff p 2
    case SockAddrInet port addr of
        SockAddrInet po _ -> print (fromIntegral po :: Int)
