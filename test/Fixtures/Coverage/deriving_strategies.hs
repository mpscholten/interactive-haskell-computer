-- DerivingStrategies + GeneralizedNewtypeDeriving: strategy modifiers are
-- accepted after `deriving` keyword; derivation semantics are ignored.
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Wrapper = Wrapper Int
    deriving stock Show
    deriving newtype Eq

data Color = Red | Green | Blue
    deriving stock (Show, Eq)

main :: IO ()
main = putStrLn "ok"
