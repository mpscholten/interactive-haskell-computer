-- PatternSignatures dest type must flow into nested unannotated peek
-- inside alloca.  network getSockOpt / getAddrInfo is:
--   n :: CInt <- alloca $ \p -> do
--       poke …
--       peek p
-- Coverage/peek_cint_alloca (n :: CInt <- peek p) is GREEN.
-- This pins the dest on the alloca bind, not the peek itself.
-- Bytes are poked as Word8 so this isolates dest-first peek, not poke.
-- Do not name-list CInt / alloca / peek.
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peek, pokeByteOff)

main :: IO ()
main = do
  n :: CInt <- allocaBytes 4 $ \p -> do
    pokeByteOff p 0 (0x07 :: Word8)
    pokeByteOff p 1 (0x00 :: Word8)
    pokeByteOff p 2 (0x00 :: Word8)
    pokeByteOff p 3 (0x00 :: Word8)
    peek p
  case n of
    CInt x -> print x
