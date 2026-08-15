-- Warp leftover after setHost: Settings record update
-- `setHost x y = y{settingsHost = x}` plus
-- `defaultSettings { settingsPort = p }`.
import Network.Wai.Handler.Warp (defaultSettings, setHost)
import Network.Wai.Handler.Warp.Internal (settingsPort, settingsHost)

main :: IO ()
main = do
    let s = setHost "127.0.0.1" defaultSettings
    print (settingsPort s)
    putStrLn (show (settingsHost s))
    let s2 = defaultSettings { settingsPort = 8080 }
    print (settingsPort s2)
