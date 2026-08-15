-- Record field holding a PAP (partial application of a named
-- function).  Selector application and `seq` of the projected field
-- must yield the remaining function, not hang or leftover-apply the
-- PAP into the next record field.
--
-- Same shape as HSX `quoteExp = quoteHsxExpression settings`.
data R = R
    { run  :: Int -> Int
    , name :: String
    }

add :: Int -> Int -> Int
add x y = x + y

r :: R
r = R
    { run  = add 1
    , name = "ok"
    }

forceRun :: R -> Int -> Int
forceRun q = run q

main :: IO ()
main = do
    run r `seq` putStrLn "forced"
    print (run r 2)
    print (forceRun r 3)
    putStrLn (name r)
