{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.CString
import GHC.Exts

main :: IO ()
main = do
  print (I# (cstringLength# "hello"#))
  putStrLn (unpackCString# "hello"#)
  putStrLn (unpackAppendCString# "ab"# "cd")
  putStrLn (unpackNBytes# "abcdef"# 3#)
  putStrLn (unpackCStringUtf8# "world"#)
