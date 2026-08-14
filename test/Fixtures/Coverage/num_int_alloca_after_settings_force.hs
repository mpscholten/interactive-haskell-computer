-- After defaultSettings is forced, unscoped Num/Bits methods used by
-- allocaBytesAligned (alignment 8 → isPowerOfTwo does x .&. (x-1))
-- must stay the class dispatcher.  An instance dictionary whose
-- methods return Unknown on non-Between (Num Size after Settings
-- imports Text) must not replace Num Int: that was args=8 Unknown
-- on warp hello, before listen.
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Data.Word (Word8)
import Network.Wai.Handler.Warp.Settings (defaultSettings)
import Network.Wai.Handler.Warp.Internal (settingsPort)

main :: IO ()
main = do
    print (settingsPort defaultSettings)
    allocaBytesAligned 16 8 $ \(p :: Ptr Word8) -> do
        poke p 0
        putStrLn "alloca-ok"
