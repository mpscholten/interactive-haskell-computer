-- Regression: lens's (.#) must lex as a single TkSymOp, not TkDot + TkSymOp "#".
-- Splitting broke sections like (iextract .# Molten): "saw TkSymOp \"#\"".
infixl 9 .#
(.#) :: Int -> Int -> Int
a .# b = a * b

iextract, molten :: Int
iextract = 3
molten = 4

main :: IO ()
main = do
    print ((3 :: Int) .# 4)
    print (iextract .# molten)
    print ((iextract .# molten))
