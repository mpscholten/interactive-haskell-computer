-- Gap: `PatternSynonyms` declaration (`pattern Name = ...` / `pattern Name <- ...`). Seen in: lens-5.3.6/Control/Lens/Cons.hs:1:2 (`pattern (:<) ...`). Ref: hackage-parser-gaps.md (lens novel bucket).
{-# LANGUAGE PatternSynonyms #-}

pattern Head :: a -> [a]
pattern Head x <- (x:_)

firstOf :: [Int] -> Int
firstOf (Head x) = x
firstOf _        = 0

main = do
    print (firstOf [10, 20, 30])
    print (firstOf [])
