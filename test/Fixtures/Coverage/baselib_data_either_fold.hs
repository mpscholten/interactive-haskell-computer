-- Data.Either: `either` fold function
--
-- NOTE: `lefts`, `rights`, `partitionEithers` trip over a separate bug
-- where `[Either String Int]` list literals evaluate to no output in
-- run-file mode (not a typecheck error, exit 0 with empty stdout).
-- This fixture is deliberately scoped to just `either`, which works.
import Data.Either (either)

main :: IO ()
main = do
    putStrLn (either ("L:" ++) (\n -> "R:" ++ show n) (Left "oops" :: Either String Int))
    putStrLn (either ("L:" ++) (\n -> "R:" ++ show n) (Right 42   :: Either String Int))
    print    (either (const (0 :: Int)) (+ 1)           (Left "x"  :: Either String Int))
    print    (either (const (0 :: Int)) (+ 1)           (Right 10  :: Either String Int))
