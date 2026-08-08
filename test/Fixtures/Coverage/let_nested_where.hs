-- Nested `where` on a let-binding (bytestring foldl'/foldr' shape).
-- Pre-fix: parse died with "expected `in` in let-binding; saw TkWhere",
-- so S.foldl' never loaded and digit folds (readInt64) hung.
data Pair = Pair Int Int

f :: (Int -> Int -> Int) -> Int -> Pair -> Int
f step v = \(Pair fp len) ->
  let
    g ptr = go v ptr
      where
        end = ptr + len
        go z p | p == end = z
               | otherwise = go (step z p) (p + 1)
  in g fp

main :: IO ()
main = do
  print (f (\a _ -> a + 1) 0 (Pair 0 3))
  print (f (\a p -> a + p) 0 (Pair 10 3))
