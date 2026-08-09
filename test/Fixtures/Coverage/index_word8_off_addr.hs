{-# LANGUAGE MagicHash #-}

-- Neutral coverage for the source-less GHC.Prim raw-address indexing leaf.
-- The embedded byte string gives us an Addr# without relying on a library
-- shim or mutable-memory helper.
import GHC.Exts

main :: Int
main = I# (word2Int# (word8ToWord# (indexWord8OffAddr# "\128"# 1#)))
