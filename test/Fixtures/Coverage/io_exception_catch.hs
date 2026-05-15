-- evaluate + try/catch over source-loaded `evaluate` (backed by the
-- `seq#` GHC.Prim primop). Asserts deterministic stdout: we do NOT
-- `show` the caught SomeException (its rendered form is an
-- interpreter-internal representation, not a stable golden).
import Control.Exception

main :: IO ()
main = do
    result <- try (evaluate (div 1 0)) :: IO (Either SomeException Int)
    case result of
        Left _  -> putStrLn "caught division by zero"
        Right v -> print v
    -- catch version
    catch (do
        _ <- evaluate (error "boom")
        putStrLn "unreachable")
        (\e -> let _ = (e :: SomeException) in putStrLn "caught boom")
