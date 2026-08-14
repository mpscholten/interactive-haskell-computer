-- Foldable [] implements foldr as `foldr = List.foldr`.
-- That re-export must evaluate the list foldr in GHC.Internal.Base,
-- not re-enter the Foldable dispatcher (which used to hang).
main = print (foldr (+) 0 [1, 2, 3 :: Int])
