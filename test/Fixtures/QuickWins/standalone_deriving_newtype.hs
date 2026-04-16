-- StandaloneDeriving + GeneralizedNewtypeDeriving: `deriving newtype instance`
-- over a `newtype` parses and round-trips Show/Eq via the wrapped type.
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Bar a = Bar a

deriving newtype instance Show a => Show (Bar a)
deriving newtype instance Eq a => Eq (Bar a)

main :: IO ()
main = do
    let b = Bar (5 :: Int)
    print b
    print (b == Bar 5)
