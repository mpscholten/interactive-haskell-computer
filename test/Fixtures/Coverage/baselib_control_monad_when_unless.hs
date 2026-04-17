-- Control.Monad: when, unless, void
--
-- NOTE: `forM_`, `mapM_`, `replicateM_`, `sequence_` currently fail
-- source-loaded (`flip`/`#.`/LoopException), so this fixture only
-- exercises the three helpers that resolve cleanly.
import Control.Monad (when, unless, void)

main :: IO ()
main = do
    when True    (putStrLn "when-true")
    when False   (putStrLn "when-false-should-be-hidden")
    unless True  (putStrLn "unless-true-should-be-hidden")
    unless False (putStrLn "unless-false")
    void (pure (42 :: Int))
    putStrLn "void-after"
