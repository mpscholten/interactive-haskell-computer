-- Gap: Unboxed-tuple syntax `(# a, b #)`. Seen in: GHC/Conc/Sync.hs (used transitively by warp and network). Ref: warp-dryrun-findings.md (blocker #4).
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

import GHC.Exts (Int#, (+#))

addPair# :: (# Int#, Int# #) -> Int#
addPair# (# a, b #) = a +# b

main = putStrLn "ok"
