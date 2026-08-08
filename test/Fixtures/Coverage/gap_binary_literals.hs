-- Gap: BinaryLiterals extension `0b1010` (and `0B`). Ref: HsExtLiterals.hs.
{-# LANGUAGE BinaryLiterals #-}
main = print (0b1010 :: Int)
