-- Phase 3.6: lexical capture of implicit params.
-- f is a closure created where ?x = 1. When called from where ?x = 99,
-- it still sees ?x = 1 (lexical capture wins over call-site binding).
-- Expected output: 1
main :: IO ()
main =
    let f = let ?x = (1 :: Int) in \dummy -> ?x
    in print (let ?x = 99 in f ())
