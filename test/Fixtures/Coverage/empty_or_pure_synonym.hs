-- CAF `empty <|> pure 'z'` / `empty <|> q` on a synonym carrier.
-- Isolates: empty_caf_synonym_alternative (p = empty)
--           alt_pure_or_pure_synonym (p = pure 'a' <|> pure 'b')
-- Combined leftover: empty as a nullary Alternative CAF operand of `<|>`.
-- Left-biased <|> so empty must be a real Box, not a leftover class method.
-- Custom ADT: no megaparsec / Parser / ParsecT names.
import Control.Applicative

newtype Box e s a = Box a

instance Functor (Box e s) where
    fmap f (Box x) = Box (f x)

instance Applicative (Box e s) where
    pure = Box
    Box f <*> Box x = Box (f x)

instance Alternative (Box e s) where
    empty = Box '?'
    Box x <|> _ = Box x

type Mid e s = Box e s
type Alias = Mid () ()

runAlias (Box x) = x

p :: Alias Char
p = empty <|> pure 'z'

q :: Alias Char
q = pure 'y'

r :: Alias Char
r = empty <|> q

main = putStrLn [runAlias p, runAlias r]
