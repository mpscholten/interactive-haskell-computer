-- RankNTypes: nested forall in a type signature, parsed and discarded.
{-# LANGUAGE RankNTypes #-}

applyFn :: (forall a. [a] -> Int) -> [Int] -> Int
applyFn f xs = f xs

myLen :: [a] -> Int
myLen [] = 0
myLen (_:rest) = 1 + myLen rest

main :: IO ()
main = print (applyFn myLen [1, 2, 3])
