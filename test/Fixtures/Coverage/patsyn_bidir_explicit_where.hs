{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- Regression: a bidirectional pattern synonym written in the explicit
-- form @pattern Name p1 p2 \<- pat where Name q1 q2 = expr@ must
-- register the @where@-clause as the expression-direction builder.
--
-- Surfaced by warp_hello via Data.ByteString.Internal.Type's
-- @pattern PS@:
--
--   pattern PS fp zero len <- BS fp ((0,) -> (zero, len)) where
--       PS fp o len = BS (plusForeignPtr fp o) len
--
-- Inside warp's @parseRequestLine@, the helper
-- @bs ptr p0 p1 = PS fptr o l@ uses PS in expression position; before
-- this fix the binding scanner saw the @<-@ on the first line, did not
-- find an @=@ on the same line, and walked the next-line @where@'s
-- @=@ — capturing a malformed LHS @"fp zero len <- BS fp ((0,) ->
-- (zero, len)) where    PS fp o"@ that parsed as params @[fp, zero,
-- len]@ but a body that referenced @o@ (the where-clause's third
-- param), producing @unbound variable o@ at the call site.
--
-- This fixture exercises the same shape with simpler types so the
-- failure is reproducible without bringing in bytestring's full
-- transitive load.

data Box = Box Int Int

plusOff :: Int -> Int -> Int
plusOff a b = a + b

pattern Pair :: Int -> Int -> Int -> Box
pattern Pair fp zero len <- Box ((0,) -> (zero, len)) fp where
    Pair fp o len = Box (plusOff fp o) len

mk :: Int -> Int -> Int -> Box
mk fp o l = Pair fp o l   -- expression direction; uses where-clause's params

main :: IO ()
main = do
    case mk 100 5 8 of
        Box a b -> putStrLn ("Box " ++ show a ++ " " ++ show b)
