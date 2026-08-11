data Box a = Empty | One a | Pair a a

class Build f where
    one :: a -> f a
    pair :: a -> a -> f a

instance Build Box where
    one = One
    pair = Pair

main :: IO Int
main = pure (case (one 23 :: Box Int) of
    One x -> case ((pair 10 :: Int -> Box Int) 7) of
        Pair y z -> x + y + z
        _ -> 0
    _ -> 0)
