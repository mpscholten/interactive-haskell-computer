import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)

-- Regression (warp/http-types value-level Ix blocker): an Array indexed by a
-- derived-Ix enum with ByteString ELEMENTS.  The array's (!) calls Ix's
-- unsafeIndex/index on the bounds tuple; once Data.ByteString is imported its
-- same-leaf-named unsafeIndex/index FUNCTIONS used to win the bare-name
-- runtime resolution (the unscoped global module scan ran before the
-- class-method check) and pattern-fail on the bounds tuple with
-- "Non-exhaustive [[PCon BS …]]".  A registered class method must win over a
-- scope-blind same-named top-level binding.  (Array M String works regardless
-- — String has no competing unsafeIndex — so this fixture pins the ByteString
-- case specifically.)
data M = GET | POST | HEAD deriving (Show, Eq, Ord, Enum, Bounded, Ix)

arr :: Array M ByteString
arr = listArray (GET, HEAD) [B8.pack "a", B8.pack "b", B8.pack "c"]

main :: IO ()
main = B8.putStrLn (arr ! POST)
