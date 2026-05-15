-- Exercises source-loaded `try` and `handle` from
-- GHC.Internal.Control.Exception(.Base) after their host shims were
-- removed (Builtins.hs: tryB/handleB deleted; only `catch` stays host).
--   handle = flip catch
--   try a  = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))
-- A custom Exception type keeps the golden deterministic (no reliance
-- on the interpreter's stub `show` of a host SomeException Val).
import Control.Exception

data MyErr = MyErr String deriving Show
instance Exception MyErr

main :: IO ()
main = do
    -- try over a throwing action: assert the exact Left payload
    r1 <- try (throwIO (MyErr "bang")) :: IO (Either MyErr ())
    case r1 of
        Left (MyErr s) -> putStrLn ("try-left: MyErr " ++ s)
        Right _        -> putStrLn "try-left: unexpected Right"
    -- try over a non-throwing action: assert the exact Right payload
    r2 <- try (pure (7 :: Int)) :: IO (Either MyErr Int)
    case r2 of
        Left _  -> putStrLn "try-right: unexpected Left"
        Right v -> putStrLn ("try-right: Right " ++ show v)
    -- handle recovering a thrown exception to a fixed string
    msg <- handle (\(MyErr s) -> pure ("handled:" ++ s)) (do
        _ <- throwIO (MyErr "zap")
        pure "unreachable")
    putStrLn msg
