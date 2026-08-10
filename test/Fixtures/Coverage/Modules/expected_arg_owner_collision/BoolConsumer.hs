module BoolConsumer (consume, boolResult) where

consume :: Int -> Bool -> String
consume _ = show

boolResult :: String
boolResult = consume 0 True
