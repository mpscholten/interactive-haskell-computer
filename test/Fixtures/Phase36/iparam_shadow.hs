-- Nested let ?x bindings: inner wins.
main =
    let ?x = 1 in
    let ?x = 2 in
    print ?x
