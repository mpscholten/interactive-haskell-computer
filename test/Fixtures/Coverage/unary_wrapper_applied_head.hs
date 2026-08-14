-- Unary wrapper instance heads: @instance C (F T)@ registers as
-- compound @F T@, while the runtime value is @VCon F [t]@ with
-- typeTagOf @F@.  Dispatch must retry @F <innerTag>@.
{-# LANGUAGE FlexibleInstances #-}

data Box = Box Char

newtype Wrap a = Wrap a

class C a where
    take1_ :: a -> Maybe (Char, a)

instance C (Wrap Box) where
    take1_ (Wrap (Box c)) = Just (c, Wrap (Box c))

instance C Box where
    take1_ b = case take1_ (Wrap b) of
        Nothing -> Nothing
        Just (c, Wrap b') -> Just (c, b')

main :: IO ()
main = case take1_ (Box 'h') of
    Just (c, _) -> putStrLn [c]
    Nothing -> putStrLn "none"
