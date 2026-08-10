{-# LANGUAGE GADTs #-}

class Pick a where
    pick :: Box a -> Int
    choose :: Either String a -> Int
    refine :: Refined a -> Int

instance Pick Int where
    pick _ = 11
    choose _ = 12
    refine _ = 13

instance Pick Bool where
    pick _ = 21
    choose _ = 22
    refine _ = 23

data Box a where
    Box :: a -> Box a

data Either l r where
    Left :: l -> Either l r
    Right :: r -> Either l r

data Refined a where
    AsInt :: Int -> Refined Int
    AsBool :: Bool -> Refined Bool

main :: Int
main = pick (Box 4) + choose (Right 8) + refine (AsBool False)
