import Data.String.Conversions (cs)

keep :: a -> a -> a
keep x _ = x

-- There is no useful String -> Bool conversion.  Expected-type inference of
-- the ignored argument must fail softly and preserve normal lazy evaluation.
main :: IO ()
main = print (keep True (cs "ignored"))
