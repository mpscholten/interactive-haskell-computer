-- Coverage for ImplicitParams as a language feature: a nullary binding
-- with an IP constraint reads ?x from the call site, not from a CAF
-- closed over the empty definition-site map.
--
--   f :: (?x :: Int) => Int
--   f = ?x
--   main = print (let ?x = 7 in f)
--
-- GHC elaborates the constraint as dictionary-passing. IHC threads the
-- implicit-param map into EVar / force so the caller's ?x is visible.
{-# LANGUAGE ImplicitParams #-}

f :: (?x :: Int) => Int
f = ?x

main :: IO ()
main = print (let ?x = 7 in f)
