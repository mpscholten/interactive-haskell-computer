-- Sanity probe 1: withForeignPtr with an empty body.
-- Allocates a ForeignPtr, calls withForeignPtr with a no-op body.
-- Expected: prints "ok" and exits.

import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

main :: IO ()
main = do
    putStrLn "before alloc"
    fp <- mallocForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    putStrLn "after alloc"
    withForeignPtr fp $ \ptr -> do
        -- body is a no-op
        pure ()
    putStrLn "ok"
