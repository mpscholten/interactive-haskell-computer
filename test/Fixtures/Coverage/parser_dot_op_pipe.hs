-- Regression: conduit's (.|) must lex as a single TkSymOp, not TkDot + TkBar.
-- Splitting broke fuseStream-style infix uses: "saw TkBar" after a section.
infixl 7 .|
(.|) :: Int -> Int -> Int
a .| b = a + b

main :: IO ()
main = print ((1 :: Int) .| 2)
