{-# LANGUAGE TemplateHaskell #-}
-- Ladder 4: `$([| 1 |])` round-trip without wrapping the quote in Q.
main = print $([| 1 |])
