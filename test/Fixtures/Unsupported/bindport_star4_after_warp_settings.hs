-- Gap: bindPortTCP "*4" after defaultSettings still hangs at socket on some trees
-- warp_hello leftover after SizeOf/setSocketOption GREEN:
--   Non-exhaustive patterns I# I# args=8 Unknown
-- Existing Size/Num fixtures (allocaBytesAligned I# 8#, 8-1 after
-- Settings, 0*1000000) are green. The remaining crash is bindPortTCP
-- with host "*4" (HostIPv4) after defaultSettings is forced and the
-- warp_hello import graph (http-types + wai + warp) is loaded.
-- Gap is inside bindPortTCP after getAddrInfo returns — not a
-- host-shim candidate.
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp.Internal (settingsPort)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (close)

main :: IO ()
main = do
    let set = defaultSettings { settingsPort = 18765 }
        _ = (responseLBS, status200)
    s <- bindPortTCP (settingsPort set) "*4"
    close s
    putStrLn "ok"
