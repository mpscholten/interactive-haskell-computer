{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH (Lit(..), litE)

main :: IO ()
main = print $(litE (IntegerL 14))
