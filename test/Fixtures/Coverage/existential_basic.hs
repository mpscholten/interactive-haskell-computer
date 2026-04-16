-- ExistentialQuantification: data ctor with forall + constraint prefix.
-- The scanner must parse the ctor and register its arity correctly.
{-# LANGUAGE ExistentialQuantification #-}

data Dyn = forall a. Show a => Dyn a

wrap :: Int -> Dyn
wrap n = Dyn n

main :: IO ()
main = putStrLn "ok"
