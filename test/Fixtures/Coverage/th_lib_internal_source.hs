{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Lib.Internal (litE, varK)
import Language.Haskell.TH.Syntax (Lit(..), Type(..), mkName)

main :: IO ()
main = print $(litE (IntegerL 11))
