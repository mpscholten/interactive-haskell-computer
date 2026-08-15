-- mallocBytes then defaultSettings then fromIntegral / mallocBytes.
-- sendResponse of responseLBS does createWriteBuffer (malloc) then
-- settingsServerName = C8.pack ("Warp/" ++ warpVersion), which mallocs
-- again.  Pre-fix: Num.fromInteger of a VInt after Settings last-writer
-- Fusion.Size (`exactSize . fromInteger`) re-entered the dispatcher and
-- composed (.) forever (g x / n spin, exit 124).  Typed Size literals
-- still use ETypedMethod.  No host shim.  No name list.
import Foreign.Marshal.Alloc (mallocBytes)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp.Internal (settingsPort)

main :: IO ()
main = do
    _p1 <- mallocBytes 16
    print (settingsPort defaultSettings)
    _p2 <- mallocBytes 16
    print (fromIntegral (12 :: Int) :: Int)
