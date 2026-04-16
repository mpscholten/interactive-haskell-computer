import Control.Exception

main = do
    result <- try (evaluate (div 1 0)) :: IO (Either SomeException Int)
    case result of
        Left _  -> putStrLn "caught division by zero"
        Right v -> print v
    -- catch version
    catch (do
        _ <- evaluate (error "boom")
        putStrLn "unreachable")
        (\e -> putStrLn ("caught: " ++ show (e :: SomeException)))
