-- Gap (graduated): `runST` + `newSTRef`/`readSTRef`/`writeSTRef`. ST is
-- operationally identical to IO; the existing IORef bridge (matchPat ST/STRef
-- in src/IHC/Eval.hs) covers this. Seen in: hspec-core/Runner.hs,
-- hspec-core/Shuffle.hs.
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

counter :: Int -> Int
counter n = runST $ do
    r <- newSTRef 0
    let loop 0 = readSTRef r
        loop k = do
            v <- readSTRef r
            writeSTRef r (v + 1)
            loop (k - 1)
    loop n

main = print (counter 10)
