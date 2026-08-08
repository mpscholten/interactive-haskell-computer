-- Signature-directed specialisation: measureOff maxBound (no :: Int).
import qualified Data.Text as T
import qualified Prelude as P

main = do
  let t = T.pack "hi"
  print (P.negate (T.measureOff P.maxBound t))
  print (T.length t)
