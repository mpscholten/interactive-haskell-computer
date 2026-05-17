-- Dual-path sibling of foreign_ptr_arith.hs (cf. prelude_show /
-- prelude_show_qualified precedent): same source-loaded
-- Ptr-arithmetic exercise, but the four symbols arrive through an
-- explicit import list rather than the open `import Foreign.Ptr`.
-- Locks down that the env-fallback source-load of GHC.Internal.Ptr
-- resolves identically under a selective import.
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr, castPtr)
import Foreign.Storable (poke)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

main :: IO ()
main = do
    p <- mallocBytes 16 :: IO (Ptr Word8)
    let q = p `plusPtr` 7 :: Ptr Word8
    print (q `minusPtr` p)                                  -- 7
    print ((p `plusPtr` 0) `minusPtr` p)                    -- 0
    let r = (p `plusPtr` 10) `plusPtr` 3 :: Ptr Word8
    print (r `minusPtr` p)                                  -- 13
    let ci = castPtr p  :: Ptr Int
        cb = castPtr ci :: Ptr Word8
    print ((cb `plusPtr` 4) `minusPtr` cb)                  -- 4
    print ((nullPtr :: Ptr Word8) `minusPtr` (nullPtr :: Ptr Word8))                 -- 0
    print (((nullPtr :: Ptr Word8) `plusPtr` 9) `minusPtr` (nullPtr :: Ptr Word8))   -- 9
    free p
    bs <- BSI.create 4 $ \buf -> do
        poke (buf `plusPtr` 0 :: Ptr Word8) 104             -- 'h'
        poke (buf `plusPtr` 1 :: Ptr Word8) 105             -- 'i'
        poke (buf `plusPtr` 2 :: Ptr Word8) 33              -- '!'
        poke (buf `plusPtr` 3 :: Ptr Word8) 33              -- '!'
    print bs                                                -- "hi!!"
    print (BS.length bs)                                    -- 4
