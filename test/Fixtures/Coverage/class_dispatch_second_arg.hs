-- Test: class method dispatch where the class type variable is in
-- the second argument position, not the first.
-- Example: SocketAddress.pokeSocketAddress :: Ptr a -> sa -> IO ()
-- The dispatcher must try arg2 (sa) when arg1 (Ptr) doesn't match.

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeByteOff)

class MyAddr sa where
    sizeOfAddr :: sa -> Int
    peekAddr :: Ptr sa -> IO sa
    pokeAddr :: Ptr a -> sa -> IO ()

data SimpleAddr = SimpleAddr Int

instance MyAddr SimpleAddr where
    sizeOfAddr _ = 8
    peekAddr p = do
        n <- peekByteOff p 0 :: IO Int
        return (SimpleAddr n)
    pokeAddr p (SimpleAddr n) =
        pokeByteOff p 0 n

withAddr :: MyAddr sa => sa -> (Ptr sa -> Int -> IO a) -> IO a
withAddr addr f = do
    let sz = sizeOfAddr addr
    allocaBytes sz $ \p -> do
        pokeAddr p addr
        f (castPtr p) sz

main :: IO ()
main = do
    withAddr (SimpleAddr 42) $ \ptr sz -> do
        val <- peekByteOff ptr 0 :: IO Int
        print val
