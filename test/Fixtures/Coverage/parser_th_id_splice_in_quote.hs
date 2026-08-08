-- Gap: TH id-splice `$var` inside [| … |] (IHP HSX QQ compileToHaskell).
-- Residual probe: unexpected token; saw TkDollar.
-- The quote/splice forms need only to parse; we discard them and print a
-- constant so Coverage has a stable golden without requiring full TH eval.
{-# LANGUAGE TemplateHaskell #-}

f :: Int -> Int
f n =
    let _q = [| $n $(id n) |]
    in n + 1

main = print (f 41)
