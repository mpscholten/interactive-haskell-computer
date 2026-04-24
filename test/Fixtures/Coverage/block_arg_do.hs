-- BlockArguments: a do-block as a direct argument without parens.
-- @wrap do stmt1; stmt2@ parses as @wrap (do stmt1; stmt2)@.
wrap :: IO Int -> IO ()
wrap action = do
    n <- action
    print n

main = wrap do
    putStrLn "inside"
    pure 7
