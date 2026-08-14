{-# LANGUAGE MagicHash #-}
-- I# x == I# y is `eqInt` = isTrue# (x ==# y).  Derived Eq must not
-- re-dispatch the Int# fields (VInt) through Eq Int / Eq Bool.
import GHC.Exts (Int(I#))

main = do
    print (I# 2# == I# 2#)
    print (I# 2# == I# 10#)
    print (I# 2# /= I# 10#)
