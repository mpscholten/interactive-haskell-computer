module Modules.NestedRecordUpdateCollision.Wide (wideName) where

-- Wider homonym of the entry module's `Outer`.  Bare constructor
-- names are not unique; unioning this module's field registry into
-- the update desugarer used to inflate `Outer`'s arity and die as
-- "record update: unknown constructor".
data Outer = Outer
    { w0 :: Int
    , w1 :: Int
    , w2 :: Int
    , w3 :: Int
    , w4 :: Int
    , w5 :: Int
    }

wideName :: String
wideName = "wide"
