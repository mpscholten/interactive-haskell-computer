-- User-defined Semigroup instance declared in prefix `(<>)` form.
-- Infix-style `a <> b = ...` currently hits class-dispatch issues
-- (see Scheduler.hs:1885), so we intentionally use the prefix form
-- to cover the instance-registration path while skirting that bug.
newtype SumI = SumI Int

instance Semigroup SumI where
    (<>) (SumI a) (SumI b) = SumI (a + b)

unwrap :: SumI -> Int
unwrap (SumI n) = n

main :: IO ()
main = do
    let s = (SumI 3) <> (SumI 4)
    print (unwrap s)
