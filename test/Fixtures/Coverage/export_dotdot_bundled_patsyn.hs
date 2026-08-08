-- Regression: an export list entry of the bundled form @T(.., P)@ — a
-- @..@ wildcard followed by extra (pattern-synonym) names, e.g. base's
-- @ErrorCall(.., ErrorCall)@ in GHC.Internal.Exception — must not swallow
-- the rest of the header.  The export-subs parser used to return early at
-- the comma after @..@, leaving the cursor inside the paren group; the
-- group's own @)@ was then mistaken for the export-list terminator, so
-- @) where@ and EVERY import below were dropped.  Here that would leave
-- @sort@ unbound.  See IHC/ModuleHeader.hs parseExportSubs.
{-# LANGUAGE PatternSynonyms #-}
module Main (main, Foo(.., Bar)) where

import Data.List (sort)

data Foo = MkFoo Int

pattern Bar :: Foo
pattern Bar = MkFoo 0

main :: IO ()
main = print (sort [3, 1, 2 :: Int])
