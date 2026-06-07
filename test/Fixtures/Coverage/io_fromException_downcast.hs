-- Regression coverage for the Val-level fromException helper used by
-- source-loaded catch/try/handle. It must let matching concrete
-- exceptions through while letting non-matching downcasts fall through
-- to Nothing.

import Control.Exception

data MyErr = MyErr String deriving Show
instance Exception MyErr

main :: IO ()
main = do
    r1 <- try (throwIO (MyErr "mine")) :: IO (Either SomeException ())
    case r1 of
        Left se ->
            case fromException se of
                Just (MyErr s) -> putStrLn ("myerr: " ++ s)
                Nothing        -> putStrLn "myerr: nothing"
        Right _ -> putStrLn "myerr: right"

    r2 <- try (evaluate (error "boom" :: Int)) :: IO (Either SomeException Int)
    case r2 of
        Left se -> do
            case fromException se of
                Just (MyErr _) -> putStrLn "host-as-myerr"
                Nothing        -> putStrLn "host-not-myerr"
            case fromException se of
                Just (SomeException _) -> putStrLn "host-some"
                Nothing                -> putStrLn "host-some-nothing"
        Right _ -> putStrLn "host: right"
