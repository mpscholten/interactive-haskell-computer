myWords []       = []
myWords (' ':cs) = myWords cs
myWords cs       = let (w, rest) = spanNonSpace cs
                   in w : myWords rest

spanNonSpace []       = ([], [])
spanNonSpace (' ':cs) = ([], ' ':cs)
spanNonSpace (c:cs)   = let (w, r) = spanNonSpace cs in (c:w, r)

joinWith _ []     = ""
joinWith _ [x]    = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

main = do
    let ws = myWords "hello world foo"
    print (length ws)
    putStrLn (joinWith "-" ws)
