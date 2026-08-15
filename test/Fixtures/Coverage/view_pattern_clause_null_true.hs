-- Reduction of Unsupported/warp_tobufio_responselbs leftover:
--   PView reached matchPat — view pattern not desugared:
--   EVar "null" -> PCon "True" []
--
-- desugarRecordPats used to rewrite only top-level PView in ECase alts.
-- Function-clause / lambda / do-bind view patterns (including nested
-- ones, and pattern-synonym bodies of the same shape) must become
-- let+case before eval. Custom ADT — no Warp / Data.Text.
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

data Box = Nil | Full Int

isEmpty :: Box -> Bool
isEmpty Nil      = True
isEmpty (Full _) = False

-- Function clause, same PView shape as (null -> True)
describe (isEmpty -> True) = "empty"
describe _                 = "full"

-- Nested view inside a constructor (function-clause ECase)
describeJust (Just (isEmpty -> True)) = "empty-just"
describeJust _                        = "full"

-- Lambda (top-level and nested)
viaLam x     = (\(isEmpty -> True) -> "empty") x
viaLamJust x = (\(Just (isEmpty -> True)) -> "empty") x

-- Do-bind (top-level and nested)
viaDo x = do
    (isEmpty -> True) <- return x
    return "empty"

viaDoJust x = do
    Just (isEmpty -> True) <- return x
    return "empty"

-- Pattern synonym: Data.Text `pattern Empty <- (null -> True)`
pattern Empty <- (isEmpty -> True)

viaSyn Empty = "empty"
viaSyn _     = "full"

main :: IO ()
main = do
    putStrLn (describe Nil)
    putStrLn (describe (Full 1))
    putStrLn (describeJust (Just Nil))
    putStrLn (describeJust (Just (Full 1)))
    putStrLn (viaLam Nil)
    putStrLn (viaLamJust (Just Nil))
    a <- viaDo Nil
    putStrLn a
    b <- viaDoJust (Just Nil)
    putStrLn b
    putStrLn (viaSyn Nil)
    putStrLn (viaSyn (Full 1))
