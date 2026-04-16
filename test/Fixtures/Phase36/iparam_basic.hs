-- Basic implicit parameter: foo uses ?x from its call site
foo n = n + ?x

main = let ?x = 10 in print (foo 5)
