-- Gap: `TupleSections` — partial tuple syntax `(, x)` / `(x, )`. Seen in: IHP 2 files. Ref: ihp-unsupported-scan.md.
{-# LANGUAGE TupleSections #-}

tagLeft :: Int -> (Int, String)
tagLeft = (, "left")

tagRight :: String -> (Int, String)
tagRight = (7, )

main = do
    print (tagLeft 3)
    print (tagRight "done")
