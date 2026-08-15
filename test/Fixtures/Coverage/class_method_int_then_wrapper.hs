-- The class parameter lives in the second argument (a wrapper around
-- `s`), not the leading Int.  Mutual class defaults that call each
-- other must not run: dispatch has to wait for the wrapper and then
-- recover `s` from it.
data Chunk = Chunk
data PosState s = PosState
    { pstateInput :: s
    , pstateOffset :: !Int
    , pstateLinePrefix :: [Char]
    }

class Walk s where
    walk :: Int -> PosState s -> Int
    walk n pst = walkNoLine n pst

    walkNoLine :: Int -> PosState s -> Int
    walkNoLine n pst = walk n pst

instance Walk Chunk where
    walk _ _ = 47
    walkNoLine n pst = walk n pst

main :: IO ()
main = print (walkNoLine 0 (PosState Chunk 0 []))
