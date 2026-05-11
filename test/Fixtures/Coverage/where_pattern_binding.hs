-- Regression test: pattern binding in a where clause.
--
-- @parseWherePatBind@ in 'IHC.Parser' used to call 'parseSubPat'
-- for the LHS pattern, which consumed only a single atomic token.
-- For @BS _ m = bs@ it would consume @BS@ as a nullary
-- @PCon "BS" []@ and then raise @expected `=`; saw TkUnderscore@.
-- The error was silently caught downstream, so the where-clause
-- pattern binding was dropped, the body's @m@ became unbound, and
-- evaluating @f (BS 0 5)@ returned a function-shaped value instead
-- of @6@.  Fix: use 'parseTopPat' for the LHS so applied
-- constructors and infix cons are handled.
--
-- Hits real Hackage code: 'Data.ByteString.Internal.Type.concat'
-- uses
--   goLen1 bss0 bs (BS _ len:bss) = goLen bss0 (len' + len) bss
--     where BS _ len' = bs
-- so until this fix the 'Data.ByteString.concat' shim couldn't be
-- removed.  Confirmed by the graduate of 'Data.ByteString.concat'
-- and 'Data.ByteString.Char8.concat' from 'IHC.Builtins' in the
-- same change.
module Main where

data MyBS = BS Int Int

f :: MyBS -> Int
f bs = m + 1
  where BS _ m = bs

main :: IO ()
main = print (f (BS 0 5))
