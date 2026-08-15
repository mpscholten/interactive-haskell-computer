{-# LANGUAGE TemplateHaskell #-}
-- Ladder 2: `[| x |]` where `x = 1` (quoted name splices back to the binding).
x = 1
main = print $( [| x |] )
