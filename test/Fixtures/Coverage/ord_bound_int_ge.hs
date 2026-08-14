-- print (a >= b) for bound Ints must stay Ord Int.
--
-- elaborateExpectedArg InferFreely's `print :: Show a => a -> IO ()`
-- then ExpectType's the argument at that fresh @$t0@.  A second
-- FreshSource reused @$t0@ for Ord.>='s class parameter; unifying
-- the expected var with Bool rewrote the comparison to derived
-- Ord Bool ("expected constructor values, got VInt and VInt").
--
-- Warp Date / epochTimeToHTTPDate's `td >= aj` and `print` of a
-- converted HTTPDate both go through this path.
main :: IO ()
main = do
    let a = 239 :: Int
        b = 14 :: Int
    print (a >= b)
    print (b >= a)
    print (a >= a)
    -- Same comparison after quotRem (toYYMMDD / adjust).
    let (y, d) = 20679 `quotRem` 365
        leap = 14 :: Int
    print (y, d)
    print (d >= leap)
