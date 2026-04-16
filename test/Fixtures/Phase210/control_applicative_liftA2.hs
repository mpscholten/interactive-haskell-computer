-- Fixture: source-load Control.Applicative from base cache and use liftA2.
-- Previously this module was short-circuited as a builtin stub; after the
-- whitelist trim it should load from ~/.cache/ihc/sources/base-4.19.0.0/.
import Control.Applicative (liftA2)

add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = print (liftA2 add (Just 3) (Just 4))
