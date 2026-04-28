-- B.3 — overlap pragmas (Haskell user guide §6.8.7).
--
-- ihc's lexer consumes @{-# OVERLAPPING #-}@, @{-# OVERLAPPABLE #-}@,
-- and @{-# OVERLAPS #-}@ as whitespace, so they don't break parsing,
-- but the dispatcher does not currently use them for specificity
-- resolution — that requires distinguishing @[Char]@ from @[a]@ at
-- tag time, which is blocked on the typed-IR slice (C.2).
--
-- This fixture is a regression guard for the pragma-tolerance level:
-- declaring instances with overlap pragmas still loads cleanly and
-- produces the last-write-wins behaviour today's class registry
-- gives us.  When C.2 lands, this fixture's expected output should
-- change to reflect the new specificity-aware result.
module Main where

class Foo a where
    foo :: a -> Int

instance {-# OVERLAPPABLE #-} Foo a where
    foo _ = 0

instance Foo Int where
    foo _ = 1

main :: IO ()
main = do
    -- @Int@ instance is more specific and registered last;
    -- last-write-wins picks it.
    print (foo (5 :: Int))
