-- Mixed imports: `import X (foo)` makes `foo` bare AND
-- `import qualified X as Y` makes `Y.foo` qualified.
-- Regression for renamer losing bindings under aliased reimports.
import Data.List (sort)
import qualified Data.List as L

main :: IO ()
main = do
    -- bare name
    print (sort [4, 1, 3 :: Int])
    -- same name via qualified import
    print (L.sort [4, 1, 3 :: Int])
