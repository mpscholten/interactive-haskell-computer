data Text = Text
data PosState s = PosState
    { pstateInput :: s
    , pstateOffset :: !Int
    , pstateLinePrefix :: [Char]
    }

class TraversableStream s where
    reachOffset :: Int -> PosState s -> Int
    reachOffset n pst = reachOffsetNoLine n pst

    reachOffsetNoLine :: Int -> PosState s -> Int
    reachOffsetNoLine n pst = reachOffset n pst

instance TraversableStream Text where
    reachOffset _ _ = 47
    reachOffsetNoLine n pst = reachOffset n pst

main :: Int
main = reachOffsetNoLine 0 (PosState Text 0 [])
