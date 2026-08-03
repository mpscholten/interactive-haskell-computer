-- Coverage: Data.Vault.Lazy is a CPP template module
-- (@#define LAZINESS Lazy@ + @#include "IO.h"@). IHC must expand that
-- include as Haskell (not blank .h lines) so @Vault.newKey@ resolves —
-- warp's pauseTimeoutKey CAF shape.
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE pauseTimeoutKey #-}
pauseTimeoutKey :: Vault.Key ()
pauseTimeoutKey = unsafePerformIO Vault.newKey

main :: IO ()
main = do
    k <- Vault.newKey
    let !_ = pauseTimeoutKey
    -- Force the Key so we know newKey actually ran (not a no-op CAF).
    k `seq` pauseTimeoutKey `seq` print True
