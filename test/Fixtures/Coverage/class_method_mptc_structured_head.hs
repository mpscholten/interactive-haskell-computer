{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

data Text = Text
data Wrap a = Wrap a

class Choose phantom payload where
    first :: payload -> Int
    second :: payload -> Int

instance Choose phantom (Wrap Text) where
    first _ = 53
    second payload = first payload

main :: Int
main = second (Wrap Text)
