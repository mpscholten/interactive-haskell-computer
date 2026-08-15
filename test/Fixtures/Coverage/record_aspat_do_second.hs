-- Record as-pattern + field used in where + do first stmt
-- `const (return ())` then a later putStrLn.  Warp.runSettingsSocket
-- is this shape; the leftover is *imported* Warp.Run, not this ADT.
{-# LANGUAGE RecordWildCards #-}
data Box = Box { boxHook :: (IO () -> IO ()), boxName :: String }

runBox box@Box{boxHook = hook} arg = do
    hook closeListen
    putStrLn (boxName box)
    putStrLn arg
  where
    closeListen = return ()

main :: IO ()
main = do
    putStrLn "before"
    runBox (Box (const (return ())) "second") "third"
    putStrLn "after"
