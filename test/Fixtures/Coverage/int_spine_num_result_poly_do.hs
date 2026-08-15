-- Int-spine Num.+ under a result-polymorphic do-carrier must not
-- treat fromInteger 0 as the carrier (`fromInteger = pure`).
-- Same leftover as compareText's
--   compareInternal `Prelude.compare` 0
-- / a `+` operand under ParsecT: left=0 right=<carrier>.
-- Custom ADT + local Num; no megaparsec / Text.
import Control.Applicative (liftA2)

newtype Box e s a = Box (String -> Maybe (a, String))

instance Functor (Box e s) where
    fmap f (Box g) = Box $ \s -> case g s of
        Nothing -> Nothing
        Just (a, s') -> Just (f a, s')

instance Applicative (Box e s) where
    pure x = Box $ \s -> Just (x, s)
    Box f <*> Box x = Box $ \s -> case f s of
        Nothing -> Nothing
        Just (g, s') -> case x s' of
            Nothing -> Nothing
            Just (a, s'') -> Just (g a, s'')

instance Monad (Box e s) where
    Box m >>= k = Box $ \s -> case m s of
        Nothing -> Nothing
        Just (a, s') -> case k a of
            Box n -> n s'

instance Num a => Num (Box e s a) where
    fromInteger n = pure (fromInteger n)
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate

type Alias = Box () ()

runAlias (Box g) = case g "" of
    Just (c, _) -> [c]
    Nothing -> "fail"

-- compare-shaped: leftover 0 next to an Int (min / +) under the do.
cmp0 n = n `compare` 0

p :: Alias Char
p = do
    _ <- pure ()
    let n = min 2 2 + 0
        o = cmp0 n
    if o == GT then pure 'x' else pure 'y'

main = putStrLn (runAlias p)
