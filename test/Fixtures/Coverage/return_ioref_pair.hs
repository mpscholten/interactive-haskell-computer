-- Regression: a named IO function with two or more <- binds followed
-- by return/pure.  The do-desugaring chains two >>=; when the
-- second >>=  continuation ran, the value slot was being applied
-- as a function (the first bind's result leaked into a thunk that
-- was mistaken for the State# state function).
import Data.IORef

mkSingle :: IO (IORef Int)
mkSingle = do
    a <- newIORef (0 :: Int)
    _ <- newIORef False
    return a

main :: IO ()
main = do
    r <- mkSingle
    v <- readIORef r
    putStrLn (show v)
