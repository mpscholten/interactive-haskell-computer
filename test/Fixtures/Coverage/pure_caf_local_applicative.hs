-- CAF signature pins result-poly `pure` to a local Applicative.
-- Same hole as `p :: Parser Char; p = pure 'z'` (no preceding do-action).
-- Custom ADT: no megaparsec / unParser / cok names.
newtype Box a = Box a

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure = Box
    Box f <*> Box x = Box (f x)

runBox (Box x) = x

p :: Box Char
p = pure 'z'

main = putStrLn [runBox p]
