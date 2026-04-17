-- Data.Ord: `comparing`
--
-- `comparing` drives sortBy via a key projection function.
--
-- NOTE: using (Int, Char) pairs here intentionally — printing a
-- [(Int, String)] list currently evaluates to empty stdout (Show
-- instance for list-of-String-component-tuples has a bug).
-- NOTE: Data.Ord.Down currently trips `apply: not a function: <Down...>` —
-- the newtype wrapper is being stored without a newtype-unwrap path in
-- the evaluator. Left out of this fixture.
import Data.Ord (comparing)
import Data.List (sortBy)

main :: IO ()
main = do
    let xs = [(3 :: Int, 'c'), (1, 'a'), (2, 'b')]
    print (sortBy (comparing fst) xs)
    print (sortBy (comparing snd) xs)
    -- comparing over a computed key (descending by key)
    print (sortBy (comparing (\(k, _) -> negate k)) xs)
