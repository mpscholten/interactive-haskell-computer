-- thenIO leftover: (IO m) k matches (# new_s, _ #) against leftover
-- State# VFun (memcpyFp >> pokeFp / hPutBuf). Custom Buf ADT so the
-- fixture does not depend on ByteString hPut.
-- memcpyFp is unsafeWithForeignPtr (copyBytes after coerce).
-- Rematch must not treat a cons PAP (`:`) as State#.
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Storable (poke, peek)
import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Utils (copyBytes)
import qualified GHC.ForeignPtr as GHC

data Buf = Buf (ForeignPtr Word8) Int

memcpyFp :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyFp fp fq n = GHC.unsafeWithForeignPtr fp $ \p ->
                     GHC.unsafeWithForeignPtr fq $ \q -> copyBytes p q n

pokeFp :: ForeignPtr Word8 -> Word8 -> IO ()
pokeFp fp val = GHC.unsafeWithForeignPtr fp $ \p -> poke p val

fill :: Buf -> Buf -> Word8 -> IO ()
fill (Buf dst _) (Buf src slen) c = do
    memcpyFp dst src slen
    pokeFp (dst `plusForeignPtr` slen) c

main :: IO ()
main = do
    src <- mallocForeignPtrBytes 1
    dst <- mallocForeignPtrBytes 2
    pokeFp src 65
    fill (Buf dst 2) (Buf src 1) 66
    GHC.unsafeWithForeignPtr dst $ \d -> do
        a <- peek d
        b <- peek (d `plusPtr` 1)
        print (fromEnum a, fromEnum b)
