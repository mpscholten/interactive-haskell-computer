-- mask / mask_ / uninterruptibleMask_ source-loaded from GHC.Internal.IO
-- (Phase 2.10a shim removal). At the Val level there is no async-exception
-- masking state, so the source bodies reduce to running the action with an
-- identity restore. bracket (which is mask $ \restore -> ...) exercises the
-- restore path indirectly. Deterministic stdout, asserted via .out.
import Control.Exception

main :: IO ()
main = do
    mask_ (putStrLn "in mask")
    mask (\restore -> restore (putStrLn "restored") >> putStrLn "after")
    uninterruptibleMask_ (putStrLn "uninterruptible")
    r <- bracket
            (putStrLn "acquire" >> pure (7 :: Int))
            (\_ -> putStrLn "release")
            (\x -> putStrLn ("use " ++ show x) >> pure (x * 2))
    print r
