-- User-defined @instance Eq@ on a multi-field ADT, plus the
-- class-default @/=@ (@x /= y = not (x == y)@).  Locks down explicit
-- Eq instance registration + the derived @/=@ default once @==@/@/=@
-- are no longer host shims.
data RGB = RGB Int Int Int

instance Eq RGB where
    RGB a b c == RGB d e f = a == d && b == e && c == f

main :: IO ()
main = do
    print (RGB 1 2 3 == RGB 1 2 3)
    print (RGB 1 2 3 == RGB 1 2 4)
    print (RGB 1 2 3 /= RGB 9 9 9)
    print (RGB 1 2 3 /= RGB 1 2 3)
