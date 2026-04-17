-- Verifies that source-loaded `error "..."` in a partial function
-- (here, `head []`) surfaces the real payload string through raise#.
--
-- Before the Scheduler's pre-discovery of GHC.Exception helpers and
-- raise#'s forceToException peek-at-helper-application path, this
-- would crash with
--   @IhcException: IHC.Eval: unbound variable errorCallWithCallStackException@
-- instead of the actual @Prelude.head: empty list@ message.
--
-- Runs are expected to exit non-zero — this fixture is exercised by a
-- dedicated test in "RunFile" rather than the auto-discovered
-- Coverage suite (which assumes exit code 0).
module Main where

import Data.List (head)

main :: IO ()
main = print (head ([] :: [Int]))
