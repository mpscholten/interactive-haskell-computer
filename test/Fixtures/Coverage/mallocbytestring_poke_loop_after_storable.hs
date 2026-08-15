-- After Foreign.Storable loads, source-loaded Storable.poke used to
-- dest-first-dispatch to instance Storable (Ptr b).  A custom poke
-- loop into mallocByteString then hung (or left the buffer as NULs).
-- Host pokeB (import Foreign.Storable (poke)) stayed GREEN; this
-- fixture uses the source method.  No Date name list.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Internal.Foreign.Storable (poke)

main :: IO ()
main = do
  fp <- BSI.mallocByteString 8
  withForeignPtr fp $ \p ->
    mapM_ (\(i,b) -> poke (p `plusPtr` i :: Ptr Word8) b)
          (zip [0..] ([65,66,67,68,69,70,71,72] :: [Word8]))
  print (BS.unpack (BSI.fromForeignPtr fp 0 8))
