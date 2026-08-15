-- pattern Empty <- (null -> True) (text/bytestring).  sendResponse of
-- a lazy-ByteString body expands Empty to PView; matchPat must apply
-- the view, not error.  No host shim.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

pattern Empty <- (null -> True)

main :: IO ()
main = do
    print $ case [] of
        Empty -> "empty"
        _     -> "no"
    print $ case [1 :: Int] of
        Empty -> "empty"
        _     -> "no"
