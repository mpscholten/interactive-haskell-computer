-- Warp leftover after setHost was a Settings record update.
-- Reduced: custom Settings ADT, no Warp names in the interpreter.
-- setHost is `y{settingsHost = x}`; callers also write
-- `defaultSettings { settingsPort = p }`.
data Settings = Settings
    { settingsPort :: Int
    , settingsHost :: String
    }

defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = "*4"
    }

setHost :: String -> Settings -> Settings
setHost x y = y{settingsHost = x}

main :: IO ()
main = do
    let s = setHost "127.0.0.1" defaultSettings
    print (settingsPort s)
    putStrLn (settingsHost s)
    let s2 = defaultSettings { settingsPort = 8080 }
    print (settingsPort s2)
    putStrLn (settingsHost s2)
