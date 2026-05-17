-- Same as io_mask_family but via an explicit selective import list,
-- exercising the Control.Exception -> GHC.Internal.IO source-load path
-- for the named mask combinators (Phase 2.10a shim removal).
import Control.Exception (mask, mask_, uninterruptibleMask_)

main :: IO ()
main = do
    mask_ (putStrLn "q in mask")
    mask (\restore -> restore (putStrLn "q restored") >> putStrLn "q after")
    uninterruptibleMask_ (putStrLn "q uninterruptible")
