module Modules.ReexportTypeFields.Internal where

data Hints = Hints
    { hFlags :: [String]
    , hType  :: Int
    } deriving (Show)

defaultHints :: Hints
defaultHints = Hints { hFlags = [], hType = 0 }
