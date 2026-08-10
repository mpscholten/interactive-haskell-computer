{-# LANGUAGE TemplateHaskell #-}

main :: Int
main = $(pure [| 42 |])
