{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

data Text = Text
data Flag = Flag
data Box a b = Box a b

class Choose a b where
    first :: Box a b -> Int
    second :: Box a b -> Int

instance Choose Text Flag where
    first _ = 52
    second box = first box

main :: Int
main = second @Text @Flag (Box Text Flag)
