-- Phase 2.8: runRW# is available as a builtin name.
-- It applies the function to the RealWorld token and returns the raw result.
-- The caller (e.g. runST) is responsible for any unboxed-tuple unwrapping.
main :: IO ()
main = do
    let r = runRW# (\_ -> (# realWorld#, 42 #))
    case r of
        (# _, result #) -> print result
