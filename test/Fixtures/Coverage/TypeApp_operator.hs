{-# LANGUAGE TypeApplications #-}
-- Visible type application on operators:
--   * prefix parenthesized `(+) @Int`
--   * type app inside the parens `(+ @Int)`
--   * infix backtick `x `const` @Int @Bool y`
--   * type app on a class-method operator `(+++) @Int`
class Add a where
    (+++) :: a -> a -> a

instance Add Int where
    x +++ y = x + y

main = do
    print ((+) @Int 10 32)
    print ((+ @Int) 10 32)
    print (10 `const` @Int @Bool True)
    print ((+++) @Int 10 32)
