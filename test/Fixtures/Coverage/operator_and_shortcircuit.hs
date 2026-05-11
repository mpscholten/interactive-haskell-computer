-- Exercises (&&): basic truth table and short-circuit laziness in
-- the second argument. The source body in ghc-prim/GHC/Classes.hs is
--   True  && x = x
--   False && _ = False
-- so `False && undefined` must NOT force `undefined`.
main :: IO ()
main = do
    print (True  && True)
    print (True  && False)
    print (False && True)
    print (False && False)
    print (False && error "should not be forced")
    -- Nested / chained
    print (True && True && True)
    print (True && False && error "should not be forced either")
