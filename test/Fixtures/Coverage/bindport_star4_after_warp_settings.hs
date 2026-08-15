-- Warp hello: bindPortTCP (settingsPort set) "*4" after defaultSettings
-- plus the warp_hello import graph (http-types + wai + warp).
-- The do-bind must force settingsPort before entering bindPortTCP;
-- otherwise Settings last-writes class methods while bindPortTCP's
-- body is in flight and eval spins on compose (@g x@).
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
