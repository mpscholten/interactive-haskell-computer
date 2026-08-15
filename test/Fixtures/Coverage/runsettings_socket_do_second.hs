-- runSettingsSocket leftover: record as-pattern then
--   do { leftover const (return ()); next IO }
-- User `do { first; second }` and `bracket` still sequence.  A leftover
-- first stmt that lands as VUnit / VClassMethod must not send the rest
-- through doMonadicSequence / ParsecT (Connection never runs).
-- No Settings / Warp / accept name list. Custom ADT.
{-# LANGUAGE RecordWildCards #-}
data Box = Box { boxHook :: (() -> IO ()), boxName :: String }

runBox box@Box{boxHook = hook} = do
    hook ()
    putStrLn (boxName box)

main :: IO ()
main = do
    putStrLn "before"
    runBox (Box (const (return ())) "second")
    putStrLn "after"
