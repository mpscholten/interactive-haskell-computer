-- Minimal stubs sharing the token shape of GHC.Exts's exports:
-- record-style constructors plus MagicHash identifiers.
module Stubs (Ptr(..), Int(..), Addr#, indexWord8OffAddr#) where

data Ptr a = Ptr a
data Int   = Int

-- These two carry `#` in their name. Before the parseImportList fix,
-- encountering them in an explicit import list aborted parsing mid-list.
type Addr# = Int
indexWord8OffAddr# :: Addr# -> Int -> Int
indexWord8OffAddr# _ _ = Int
