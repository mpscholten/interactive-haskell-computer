-- setSockOpt / getSockOpt poke and peek a CInt through a Ptr.
-- CInt is `newtype CInt = CInt Int32 deriving newtype Storable`;
-- dispatch must GND-unwrap to instance Storable Int32 (STORABLE macro),
-- not host-poke a Word8 or reject the constructor.
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke, peek)

main :: IO ()
main = alloca $ \p -> do
  poke p (CInt 7)
  CInt n <- peek p
  print n
