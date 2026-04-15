-- Bang patterns just parse; strictness not enforced semantically.
strictAdd !x !y = x + y
main = print (strictAdd 40 2)
