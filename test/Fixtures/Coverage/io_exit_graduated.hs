import Control.Exception (try)
import System.Exit

main :: IO ()
main = do
    r <- try (exitWith (ExitFailure 2)) :: IO (Either ExitCode ())
    case r of
        Left e   -> putStrLn ("exitWith caught: " ++ show e)
        Right () -> putStrLn "exitWith returned (unexpected)"
    r2 <- try exitSuccess :: IO (Either ExitCode ())
    case r2 of
        Left e   -> putStrLn ("exitSuccess caught: " ++ show e)
        Right () -> putStrLn "exitSuccess returned (unexpected)"
