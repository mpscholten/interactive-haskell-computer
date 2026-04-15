-- Phase 2.8: mallocForeignPtrBytes + withForeignPtr + poke + peek round-trip
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Storable (poke, peek)

main :: IO ()
main = do
    fp <- mallocForeignPtrBytes 4
    withForeignPtr fp $ \p -> do
        poke p (65 :: Int)
        v <- peek p
        print v   -- 65
