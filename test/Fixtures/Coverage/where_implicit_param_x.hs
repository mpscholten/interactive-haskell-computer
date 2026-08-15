-- Non-CallStack IP: where ?x = n must bind, not error "?x is not in scope".
{-# LANGUAGE ImplicitParams #-}

f :: Int -> String
f n = show (?x :: Int)
  where ?x = n

main :: IO ()
main = putStrLn (f 7)
