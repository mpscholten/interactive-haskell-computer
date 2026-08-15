-- Gap: socketPair leftover — peekArray 2 on allocaBytes (2 * sizeOf CInt)
-- still peeks 8-byte Int ([335007449165,0] = (78<<32)|77). FFI mark of
-- Ptr CInt + dest-first peekElemOff is landed; dest buffer is not yet
-- marked on this memcpy/peekArray path. No send/sendAll shim.
{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf, pokeByteOff)

foreign import ccall unsafe "memcpy"
  c_memcpy_cint :: Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr CInt)

main :: IO ()
main = allocaBytes (2 * sizeOf (1 :: CInt)) $ \src -> do
  pokeByteOff src 0 (77 :: CInt)
  pokeByteOff src (sizeOf (1 :: CInt)) (78 :: CInt)
  allocaBytes (2 * sizeOf (1 :: CInt)) $ \dst -> do
    _ <- c_memcpy_cint dst src (fromIntegral (2 * sizeOf (1 :: CInt)))
    xs <- peekArray 2 dst
    print (map fromIntegral xs :: [Int])
