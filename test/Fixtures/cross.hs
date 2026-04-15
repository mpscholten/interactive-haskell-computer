-- main depends on a and b, which live after main in source order.
-- The scanner must find them on demand; emission of main's body must
-- pause to compile a and b; final linkage done via absolute-address
-- load + BLR.
main = a + b
a = 10
b = 32
