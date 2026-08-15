{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- Multiple visible type applications (`parse @Void @Text`).
class Pair a b where
    mkPair :: a -> b -> (a, b)

instance Pair Int Bool where
    mkPair = (,)

instance Pair Bool Int where
    mkPair a b = (a, b)

const2 :: a -> b -> a
const2 x _ = x

main = do
    print (const2 @Int @Bool 1 True)
    print (mkPair @Int @Bool 1 True)
    print (mkPair @Bool @Int False 2)
