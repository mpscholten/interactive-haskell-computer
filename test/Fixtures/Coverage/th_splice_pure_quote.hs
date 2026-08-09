{-# LANGUAGE TemplateHaskell #-}

main :: IO ()
main = print $(pure [| 40 + 2 |])
