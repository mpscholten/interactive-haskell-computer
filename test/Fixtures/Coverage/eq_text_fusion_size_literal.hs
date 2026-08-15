-- Megaparsec / text fusion compares a Size hint to an unannotated 0.
-- Data.Text.Internal.Fusion.Size is Between Int Int | Unknown with
-- Num.fromInteger = exactSize . fromInteger.  Unknown == 0 must
-- fromInteger the literal (False), not crash derived Eq on VInt.
--
-- Local ADT — same shape as the text-fusion type, no text import.
data Size = Between Int Int | Unknown deriving (Show, Eq)

exactSize :: Int -> Size
exactSize n = Between n n

unknownSize :: Size
unknownSize = Unknown

instance Num Size where
    fromInteger n = exactSize (fromInteger n :: Int)
    Between a b + Between c d = Between (a + c) (b + d)
    _ + _ = Unknown
    Between a b - Between c d = Between (max (a - d) 0) (max (b - c) 0)
    _ - _ = Unknown
    Between a b * Between c d = Between (a * c) (b * d)
    _ * _ = Unknown
    abs = id
    signum (Between a b) = Between (signum a) (signum b)
    signum Unknown = Unknown
    negate (Between a b) = Between (negate a) (negate b)
    negate Unknown = Unknown

main :: IO ()
main = do
    print (unknownSize == 0)
    print (exactSize 0 == 0)
    print (0 == unknownSize)
    print (0 == exactSize 0)
    print (exactSize 3 == 3)
    print (exactSize 1 == 0)
