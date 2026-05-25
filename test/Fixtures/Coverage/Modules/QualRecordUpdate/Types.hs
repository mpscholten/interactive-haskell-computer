module Modules.QualRecordUpdate.Types where

data Config = Config
    { cfgPort :: Int
    , cfgHost :: String
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { cfgPort = 80, cfgHost = "localhost" }
