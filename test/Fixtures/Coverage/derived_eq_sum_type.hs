-- Sum-type companion to 'derived_eq_via_class_dispatch' — the
-- synthesised structural @==@ must short-circuit to 'False' when
-- constructors differ (without recursing into the (impossible) field
-- match), and recurse via the dispatcher when constructors match.
--
-- Routed through a polymorphic @eqIt@ so the elaborator emits
-- @ETypedMethod \"Eq\" \"==\" \"Maybe2\"@ and exercises the same
-- source/synth path that bare @==@ now reaches through class dispatch.
data Maybe2 a = Nothing2 | Just2 a deriving Eq

eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt (Nothing2 :: Maybe2 Int) Nothing2)   -- True
    print (eqIt (Just2 1) (Just2 1))                 -- True
    print (eqIt (Just2 1) (Just2 2))                 -- False
    print (eqIt (Just2 1) Nothing2)                  -- False (diff ctor)
    print (eqIt Nothing2 (Just2 (1 :: Int)))         -- False (diff ctor)
