-- Regression: multi-char operators starting with '.' must not be split into
-- TkDot + remainder.  aeson/lens use (.=) in object builders like
-- ["error" .= msg]; splitting made the parser report "saw TkDot" / "saw TkEq".
infixr 8 .=
(.=) :: a -> b -> a
a .= _ = a

main :: IO ()
main = print (["error" .= True, "ok" .= False] :: [String])
