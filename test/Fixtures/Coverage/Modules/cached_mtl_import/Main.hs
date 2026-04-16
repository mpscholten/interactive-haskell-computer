import Control.Monad.State

counter :: State Int Int
counter = do
    n <- get
    put (n + 1)
    pure n

main :: IO ()
main = do
    let (v, s) = runState counter 0
    print v
    print s
