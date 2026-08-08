import Control.Exception

main :: IO ()
main = do
    r1 <- try (throwIO (userError "io boom")) :: IO (Either IOError ())
    case r1 of
        Left _  -> putStrLn "throwIO-caught"
        Right _ -> putStrLn "throwIO-missed"

    r2 <- try (evaluate (throw (userError "pure boom") :: Int)) :: IO (Either IOError Int)
    case r2 of
        Left _  -> putStrLn "throw-caught"
        Right _ -> putStrLn "throw-missed"

    r3 <- try (pure (7 :: Int)) :: IO (Either IOError Int)
    case r3 of
        Left _  -> putStrLn "clean-left"
        Right n -> putStrLn ("clean-right " ++ show n)
