-- Gap: imported Warp.runSettingsSocket returns without
-- runSettingsConnection.  Custom-ADT record as-pattern + do
-- (record_aspat_do_second) is GREEN.  User bindPortTCP +
-- runSettingsConnection / Maker / Secure print before-loop.
-- User bind then this call prints listen-ok then returns.
-- setBeforeMainLoop never runs.  Bracket in runSettings then
-- closes the listen socket.  Do not put "accept" back in
-- ffiBuiltinNames.  No Settings / Warp name list.
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import Data.Streaming.Network (bindPortTCP)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "start" >> hFlush stdout
    let set = setBeforeMainLoop (putStrLn "before-loop" >> hFlush stdout)
            $ setPort 14093
              defaultSettings
    sock <- bindPortTCP 14093 "*4"
    putStrLn "listen-ok" >> hFlush stdout
    runSettingsSocket set sock $ \_req respond -> do
        putStrLn "app" >> hFlush stdout
        respond $ responseLBS status200 [] "Hello, Warp!"
    putStrLn "after-socket" >> hFlush stdout
