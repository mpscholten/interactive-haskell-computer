-- Phase 2.8: quot, rem, div, divMod, quotRem
fst2 (x, _) = x
snd2 (_, y) = y

main :: IO ()
main = do
    print (quot 17 5)
    print (rem  17 5)
    print (div  17 5)
    print (fst2 (divMod 17 5))
    print (snd2 (divMod 17 5))
    print (fst2 (quotRem 17 5))
    print (snd2 (quotRem 17 5))
