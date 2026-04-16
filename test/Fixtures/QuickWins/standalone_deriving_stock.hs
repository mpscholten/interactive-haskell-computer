-- StandaloneDeriving + DerivingStrategies: `deriving stock instance`
-- parses and routes through the same synthesis path as the bare form.
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

data Box a = Box a

deriving stock instance Show a => Show (Box a)
deriving stock instance Eq a => Eq (Box a)

main :: IO ()
main = do
    let b = Box (7 :: Int)
    print b
    print (b == Box 7)
