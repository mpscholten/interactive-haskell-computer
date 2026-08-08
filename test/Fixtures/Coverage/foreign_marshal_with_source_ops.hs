import Foreign.Marshal.Utils (with)
import Foreign.Storable (peekByteOff, poke)
import qualified GHC.Internal.Foreign.Marshal.Utils as Internal

main :: IO ()
main = do
    x <- with (33 :: Int) $ \ptr -> peekByteOff ptr 0
    print (x :: Int)

    y <- Internal.with (44 :: Int) $ \ptr -> do
        poke ptr (45 :: Int)
        peekByteOff ptr 0
    print (y :: Int)
