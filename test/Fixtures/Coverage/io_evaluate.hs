-- Exercises Control.Exception.evaluate via source-load (no host shim).
-- `evaluate a = IO $ \s -> seq# a s` in GHC.Internal.IO bottoms out
-- into the `seq#` GHC.Prim primop, which forces `a` to WHNF inside IO.
--
-- We assert exact stdout: `evaluate` must (a) force-and-return pure
-- values, and (b) uncover exceptions so `try`/`catch` see them. We do
-- NOT `show` the caught SomeException (its rendered form is an
-- interpreter-internal representation, not a stable golden).
import Control.Exception

main :: IO ()
main = do
    -- (a) forces to WHNF and returns the value in IO
    n <- evaluate (1 + 2 :: Int)
    print n
    -- (b) exception is uncovered: try catches the forced error
    r <- try (evaluate (error "bang" :: Int)) :: IO (Either SomeException Int)
    putStrLn (case r of
                Left e  -> let _ = (e :: SomeException) in "caught"
                Right v -> "got " ++ show v)
    -- value path through try: no exception -> Right branch
    r2 <- try (evaluate (length [1,2,3,4] :: Int)) :: IO (Either SomeException Int)
    putStrLn (case r2 of
                Left _  -> "caught2"
                Right v -> "got " ++ show v)
    -- catch path: evaluate forces a partial function, handler runs
    catch (evaluate (head ([] :: [Int])) >> putStrLn "unreachable")
          (\e -> let _ = (e :: SomeException) in putStrLn "caught3")
