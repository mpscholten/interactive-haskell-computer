-- Lexical capture: f is defined inside let ?x = 1, passed outside.
-- When called from a scope where ?x = 99, f should still see ?x = 1.
getX = ?x

main =
    let f = let ?x = 1 in getX
        result = let ?x = 99 in f
    in print result
