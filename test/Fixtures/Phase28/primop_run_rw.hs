-- Phase 2.8: runRW# is available as a builtin name.
-- We call it directly; the interpreter's builtin applies the function
-- to the RealWorld token and strips the result out of the unboxed pair.
main :: IO ()
main = do
    let result = runRW# (\_ -> (# realWorld#, 42 #))
    print result
