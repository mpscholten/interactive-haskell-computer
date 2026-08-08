-- Regression (Part 1 + Part 2 of the @Array StdMethod@ blocker), the
-- pieces that do NOT depend on the separate listArray fill bug
-- (see test/Fixtures/Unsupported/listarray_minbound_fill.hs).
--
-- Part 1: a CAF's declared signature drives unannotated nullary class
-- methods nested in its RHS — @bnds :: (Method, Method); bnds =
-- (minBound, maxBound)@ must resolve to @Bounded Method@, not the Int
-- default.  Without it, @index bnds PUT@ would force the host @Ix Int@
-- shim and die with "Ix Int.index: non-Int index (VCon PUT)".
--
-- Part 2: @deriving Ix@ on an all-nullary (enumeration) type yields a
-- usable @Ix Method@ instance, so @rangeSize@ / @index@ / @inRange@
-- dispatch to the derived instance (constructor-order arithmetic).
import Data.Ix (Ix, rangeSize, index, inRange)

data Method = GET | POST | HEAD | PUT
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

bnds :: (Method, Method)
bnds = (minBound, maxBound)

main :: IO ()
main = do
    print (rangeSize bnds)          -- 4   (GET..PUT)
    print (index bnds PUT)          -- 3
    print (index bnds HEAD)         -- 2
    print (inRange bnds POST)       -- True
    print (inRange (POST, HEAD) GET) -- False
