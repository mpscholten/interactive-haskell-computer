-- TupleSections: missing first element
pairWith42 = (, 42)

fst3 t = case t of { (x, _) -> x }
snd3 t = case t of { (_, y) -> y }

main = do
    let p = pairWith42 1
    print (fst3 p)
    print (snd3 p)
