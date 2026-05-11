-- Builtins-removal: 'fromInteger' must resolve via the source-loaded
-- 'Num Int' instance (GHC.Internal.Num:115) — body is
-- @fromInteger i = I# (integerToInt# i)@.
--
-- 'integerToInt#' source body (ghc-bignum's GHC.Num.Integer:143-147)
-- has 3 clauses (IS / IP / IN); the IS arm fires for the parser-
-- produced 'VInt' (via the IS/IP/IN matchPat bridge in Eval.hs added
-- in PR #136), and 'I#' is a host identity shim, so the chain
-- bottoms on 'VInt' — same as the pre-graduation host shim, but
-- through the real source path.
--
-- (Round-tripping @fromInteger (toInteger n) :: Int@ is intentionally
-- NOT tested here because IHC's value-directed dispatch routes the
-- IS-tagged input to @Num Integer.fromInteger = id@ instead of the
-- annotation-directed @Num Int.fromInteger@.  That asymmetry is a
-- typeTagOf / annotation-aware-dispatch concern, not a fromInteger
-- graduation concern.)
module Main where

main :: IO ()
main = do
    print (fromInteger 5     :: Int)
    print (fromInteger 0     :: Int)
    print (fromInteger (-7)  :: Int)
    print (fromInteger 12345 :: Int)
    print (fromInteger ((-1234567) :: Integer) :: Int)
