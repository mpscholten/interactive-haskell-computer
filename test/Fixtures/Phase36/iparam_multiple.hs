-- Multiple implicit params in one let.
-- iparam_multiple: ?x and ?y both in scope; 10 + 20 = 30
main = print (let ?x = 10 in let ?y = 20 in ?x + ?y)
