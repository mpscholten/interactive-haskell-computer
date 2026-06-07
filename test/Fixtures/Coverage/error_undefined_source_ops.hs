import Control.Exception

main :: IO ()
main = do
    r1 <- try (evaluate (error "source error" :: Int)) :: IO (Either SomeException Int)
    case r1 of
        Left _  -> putStrLn "error-caught"
        Right _ -> putStrLn "error-missed"

    r2 <- try (evaluate (undefined :: Int)) :: IO (Either SomeException Int)
    case r2 of
        Left _  -> putStrLn "undefined-caught"
        Right _ -> putStrLn "undefined-missed"
