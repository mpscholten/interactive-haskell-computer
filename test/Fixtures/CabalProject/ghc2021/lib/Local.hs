-- | Fixture that relies on GHC2021-implied extensions.
module Local where

data Point = Point { x :: Int, y :: Int }

-- NamedFieldPuns: brace shorthand below desugars to
-- getX (Point { x = x }) = x
getX :: Point -> Int
getX (Point { x }) = x

-- TupleSections: (, 3) is shorthand for (\v -> (v, 3))
prefill :: [(Int, Int)]
prefill = map (, 3) [1, 2, 3]

-- ScopedTypeVariables + ExplicitForAll
idScoped :: forall a. a -> a
idScoped z = (z :: a)
