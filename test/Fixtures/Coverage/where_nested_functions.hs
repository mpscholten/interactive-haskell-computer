-- Nested where bindings
quadratic a b c = (x1, x2)
  where
    disc      = b * b - 4 * a * c
    sqrtDisc  = intSqrt disc
    x1        = (-b + sqrtDisc) `div` (2 * a)
    x2        = (-b - sqrtDisc) `div` (2 * a)
    intSqrt n = floor (sqrt (fromIntegral n :: Double))

main = do
    case quadratic 1 (-5) 6 of
        (x1, x2) -> do
            print x1
            print x2
