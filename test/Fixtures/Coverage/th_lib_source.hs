{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Lib (litE)
import Language.Haskell.TH.Syntax (Lit(..))

main :: IO ()
main = print $(litE (IntegerL 12))
