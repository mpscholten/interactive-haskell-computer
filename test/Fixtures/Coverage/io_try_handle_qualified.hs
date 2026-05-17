-- Same as io_try_handle.hs but with an explicit import list, exercising
-- the named-import path for source-loaded `try` / `handle` after the
-- host shims (tryB/handleB) were removed from Builtins.hs.
import Control.Exception (try, handle, SomeException, evaluate)

main :: IO ()
main = do
    -- try over a non-throwing action: exact Right payload
    r1 <- try (evaluate (7 :: Int)) :: IO (Either SomeException Int)
    case r1 of
        Left _  -> putStrLn "try-right: unexpected Left"
        Right v -> putStrLn ("try-right: Right " ++ show v)
    -- try over a throwing action: confirm a Left is produced
    r2 <- try (evaluate (error "bang" :: Int)) :: IO (Either SomeException Int)
    case r2 of
        Left _  -> putStrLn "try-left: caught"
        Right _ -> putStrLn "try-left: unexpected Right"
    -- handle recovering a thrown error to a fixed string
    msg <- handle (\(_ :: SomeException) -> pure "handled") (do
        _ <- evaluate (error "zap" :: Int)
        pure "unreachable")
    putStrLn msg
