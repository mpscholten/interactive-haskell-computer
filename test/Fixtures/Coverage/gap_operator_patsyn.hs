-- Gap: operator pattern synonyms `pattern x :< xs = ...`. Ref: Scan.hs operator patsyn skip; HsExtPatterns.hs.
{-# LANGUAGE PatternSynonyms #-}

pattern (:<) :: a -> [a] -> [a]
pattern x :< xs = x : xs

main = case [1, 2, 3] of
  a :< rest -> print (a, length rest)
  _         -> print (0 :: Int, 0 :: Int)
