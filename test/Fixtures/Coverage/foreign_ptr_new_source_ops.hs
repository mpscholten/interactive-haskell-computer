import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified GHC.Internal.Foreign.ForeignPtr.Imp as Imp

main :: IO ()
main = do
    p <- mallocBytes 1 :: IO (Ptr Word8)
    fp <- newForeignPtr nullFunPtr p
    withForeignPtr fp $ \ptr -> poke ptr (71 :: Word8)
    putStrLn "public"

    q <- mallocBytes 1 :: IO (Ptr Word8)
    fp2 <- Imp.newForeignPtr nullFunPtr q
    withForeignPtr fp2 $ \ptr -> poke ptr (72 :: Word8)
    putStrLn "ghc"
