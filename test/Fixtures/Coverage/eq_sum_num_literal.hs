-- Unannotated integer literals next to a Num sum type must go through
-- fromInteger before derived Eq.  GHC treats @0@ as @Num a => a@ and
-- unifies @a@ with the other operand; IHC used to leave the literal as
-- a bare VInt, so derived Eq of a multi-ctor type died
-- ("expected constructor values, got VCon … and VInt").
--
-- Custom ADT — same shape as Data.Text.Internal.Fusion.Size
-- (binary ctor | nullary ctor) + Num.fromInteger, no text import.
data Hint = Range Int Int | None deriving (Show, Eq)

instance Num Hint where
    fromInteger n = Range (fromInteger n) (fromInteger n)
    Range a b + Range c d = Range (a + c) (b + d)
    _ + _ = None
    Range a b - Range c d = Range (a - d) (b - c)
    _ - _ = None
    Range a b * Range c d = Range (a * c) (b * d)
    _ * _ = None
    abs = id
    signum (Range a b) = Range (signum a) (signum b)
    signum None = None
    negate (Range a b) = Range (negate a) (negate b)
    negate None = None

main :: IO ()
main = do
    print (None == 0)
    print (Range 0 0 == 0)
    print (0 == None)
    print (0 == Range 0 0)
    print (Range 3 3 == 3)
    print (Range 1 1 == 0)
    print (None == None)
    print (Range 2 2 == Range 2 2)
