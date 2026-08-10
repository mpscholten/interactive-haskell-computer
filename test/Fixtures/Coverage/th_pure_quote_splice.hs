{-# LANGUAGE TemplateHaskell #-}

main :: IO ()
main = print $(pure [| 42 |])
