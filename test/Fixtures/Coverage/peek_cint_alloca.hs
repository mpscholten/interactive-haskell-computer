-- getaddrinfo / getSockOpt: peek CInt from an allocated buffer.
-- getSockOpt is:
--   alloca $ \ptr -> do { … ; peek ptr }
-- getSocketOption does:
--   n :: CInt <- getSockOpt s so
-- PatternSignatures / `peek p :: IO CInt` / GND unwrap to
-- instance Storable Int32 must produce a CInt, not a leftover
-- function or a bare VInt (host peekB reads one byte).
-- Bytes are poked as Word8 so this fixture isolates peek, not poke.
-- 300 = 0x012C does not fit in a Word8.
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peek, pokeByteOff)

main :: IO ()
main = allocaBytes 4 $ \p -> do
    pokeByteOff p 0 (0x2c :: Word8)
    pokeByteOff p 1 (0x01 :: Word8)
    pokeByteOff p 2 (0x00 :: Word8)
    pokeByteOff p 3 (0x00 :: Word8)
    n :: CInt <- peek p
    case n of
        CInt x -> print x
    m <- peek p :: IO CInt
    case m of
        CInt y -> print y
