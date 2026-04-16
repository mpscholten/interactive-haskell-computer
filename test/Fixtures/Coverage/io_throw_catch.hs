import Control.Exception

main = do
    result <- try (throwIO (userError "test error")) :: IO (Either IOError ())
    case result of
        Left e  -> putStrLn ("caught: " ++ show e)
        Right _ -> putStrLn "no error"
