-- Gap: Case-alternative guards `case x of { p | g -> e }`. Seen in: lens-5.3.6/Control/Lens/Internal/FieldTH.hs:3:33, conduit/List.hs:8:17, IHP/HSX/Parser.hs:7:13. Ref: ihp-parser-gaps.md bucket 7.
classify :: Int -> String
classify n = case n of
    x | x < 0     -> "negative"
      | x == 0    -> "zero"
      | otherwise -> "positive"

main = do
    putStrLn (classify (-3))
    putStrLn (classify 0)
    putStrLn (classify 7)
