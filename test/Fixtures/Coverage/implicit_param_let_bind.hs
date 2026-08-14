-- HSX-shaped leftover: a nullary binding reads ?x from the call site
-- after `let ?x = …`, then applies `show`.  Wanted: 42
--
--   f = show ?x
--   main = let ?x = (42 :: Int) in print (f :: String)
--
-- Distinct from implicit_param_basic.hs (plain `f = ?x`).
{-# LANGUAGE ImplicitParams #-}

f :: (?x :: Int) => String
f = show ?x

main :: IO ()
main = let ?x = (42 :: Int) in print (f :: String)
