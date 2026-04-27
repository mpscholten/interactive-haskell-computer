-- Gap: `runST` + `newSTRef`/`readSTRef` — no `VSTRef` value, no `runST` primop. Seen in: hspec-core/Runner.hs, hspec-core/Shuffle.hs. Ref: hspec-dryrun-findings.md (blocker #3).
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
