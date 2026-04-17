import Control.Monad.ST (runST)
main = print (runST (return 42 :: ST s Int))
