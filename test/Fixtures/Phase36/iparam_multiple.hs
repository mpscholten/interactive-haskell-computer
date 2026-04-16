-- Multiple implicit params in one let.
main =
    let ?x = 10
        ?y = 20
    in print (?x + ?y)
