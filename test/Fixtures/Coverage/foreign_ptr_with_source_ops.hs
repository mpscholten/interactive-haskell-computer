import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import qualified GHC.ForeignPtr as GHC
import qualified GHC.Internal.Foreign.ForeignPtr as Internal

main :: IO ()
main = do
    fp <- mallocForeignPtrBytes 1 :: IO (ForeignPtr Word8)

    withForeignPtr fp $ \ptr -> poke ptr (65 :: Word8)
    a <- withForeignPtr fp peek
    putStrLn ("with " ++ show (fromIntegral a :: Int))

    GHC.unsafeWithForeignPtr fp $ \ptr -> poke ptr (66 :: Word8)
    b <- Internal.withForeignPtr fp peek
    putStrLn ("internal " ++ show (fromIntegral b :: Int))

    touchForeignPtr fp
    putStrLn "touch"
