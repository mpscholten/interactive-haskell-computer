-- GeneralizedNewtypeDeriving Bits on a local newtype of Int32
-- (same shape as CInt, without naming CInt).  Bits Int32 already
-- works; the dispatcher must unwrap every W argument, run Int32,
-- and wrap W because (.|.)'s scheme result is the class parameter.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Bits ((.|.))
import Data.Int (Int32)

newtype W = W Int32 deriving (Show, Bits)

main = print (W 1 .|. W 2)
