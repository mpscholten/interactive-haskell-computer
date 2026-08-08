-- EmptyCase: empty alternative list.
-- Defining f is enough; calling it would throw PatternMatchFail.
{-# LANGUAGE EmptyCase #-}
f :: Bool -> Int
f x = case x of {}
main = putStrLn "ok"
