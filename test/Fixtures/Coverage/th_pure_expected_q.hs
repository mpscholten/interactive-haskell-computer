{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Q, runQ)
import Prelude (Int, pure)

main = runQ (pure 42 :: Q Int)
