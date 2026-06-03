import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)

-- The faithful http-types Network.HTTP.Types.Method shape end-to-end:
--   methodArray :: Array StdMethod Method
--   methodArray = listArray (minBound, maxBound) (map (B8.pack . show) [minBound :: StdMethod .. maxBound])
--   renderStdMethod m = methodArray ! m
-- Exercises the whole stack at once: signature-directed (minBound, maxBound)
-- bounds, the [minBound :: M .. maxBound] arithmetic-sequence element list,
-- derived Ix construction, AND the value-level Ix-method-vs-ByteString
-- resolution on (!).  This is the warp request path's method rendering.
data M = GET | POST | HEAD deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array M ByteString
methodArray = listArray (minBound, maxBound) (map (B8.pack . show) [minBound :: M .. maxBound])

render :: M -> ByteString
render m = methodArray ! m

main :: IO ()
main = do
    B8.putStrLn (render GET)
    B8.putStrLn (render HEAD)
