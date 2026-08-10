{-# LANGUAGE TemplateHaskell #-}
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Syntax (Q)
import Prelude (IO, Int, pure)

main = TH.runQ (pure 42 :: Q Int)
