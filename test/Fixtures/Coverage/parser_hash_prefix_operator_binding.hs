-- Prefix operator bindings whose name starts with # lex as TkLUnbox.
-- The scanner must still discover them as ordinary top-level bindings.

(#.) :: Int -> Int -> Int
(#.) x y = x + y

main :: IO ()
main = print ((1 :: Int) #. 2)
