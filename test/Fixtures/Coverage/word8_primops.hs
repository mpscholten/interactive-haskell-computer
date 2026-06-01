-- GHC.Internal.Word and text's UTF-8 paths use the Word8# comparison
-- primops directly.
{-# LANGUAGE MagicHash #-}

import GHC.Exts (geWord8#, isTrue#, ltWord8#, wordToWord8#)

main :: IO ()
main = do
    print (isTrue# (ltWord8# (wordToWord8# 1##) (wordToWord8# 2##)))
    print (isTrue# (geWord8# (wordToWord8# 255##) (wordToWord8# 255##)))
