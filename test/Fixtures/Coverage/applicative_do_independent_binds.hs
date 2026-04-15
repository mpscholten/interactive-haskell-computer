{-# LANGUAGE ApplicativeDo #-}
-- Two independent binds followed by an expression using both results.
-- Under real ApplicativeDo this would desugar to liftA2; under our
-- monadic desugaring it sequences with >>= instead.  The observable
-- output is identical because IO Monad's Applicative instance is
-- sequential anyway.

getX = pure 3
getY = pure 7

main = do
    x <- getX
    y <- getY
    print (x + y)
