-- Regression (Part 1 of the @Array StdMethod@ blocker): a top-level
-- binding's declared signature must reach UNANNOTATED nullary class
-- methods (@minBound@ / @maxBound@) nested inside its RHS.
--
-- http-types builds @methodArray = listArray (minBound, maxBound) …@
-- with @methodArray :: Array StdMethod Method@.  The bounds tuple is
-- unannotated, so under deferred typing @minBound@/@maxBound@ default
-- to the @Int@ instance ('IHC.Builtins.minBoundB'/'maxBoundB' → VInt)
-- and the array ends up with Int bounds.
--
-- This fixture isolates the type-propagation half (no Array / Ix):
-- @bounds :: (Method, Method)@ with RHS @(minBound, maxBound)@.  The
-- signature must drive both nullary methods to @Bounded Method@, not
-- @Bounded Int@.  Before the fix this printed Int's min/max; after,
-- it prints the enum's first/last constructor.
data Method = GET | POST | HEAD | PUT
    deriving (Show, Eq, Ord, Enum, Bounded)

bounds :: (Method, Method)
bounds = (minBound, maxBound)

main :: IO ()
main = print bounds
