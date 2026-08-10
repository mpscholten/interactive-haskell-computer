module BoolConsumer (consume, boolResult) where

consume :: Int -> Bool -> Bool
consume _ = id

boolResult :: Bool
boolResult = consume 0 True
