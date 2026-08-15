{-# LANGUAGE TemplateHaskell #-}
-- Ladder 3: `:: T` inside `[| |]` must not become VarE "<unsupported>".
x = 1
main = do
    print $( [| x :: Int |] )
    print $( [| (2 :: Int) |] )
