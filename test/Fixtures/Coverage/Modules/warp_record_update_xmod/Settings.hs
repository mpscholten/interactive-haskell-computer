module Settings where

data Settings = Settings { port :: Int, host :: String } deriving Show

defaultSettings :: Settings
defaultSettings = Settings { port = 3000, host = "localhost" }
