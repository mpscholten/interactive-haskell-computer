import Language.Haskell.TH.Lib.Internal (varK)
import Language.Haskell.TH.Syntax (Type(..), mkName)

main :: IO ()
main = varK (mkName "x") `seq` pure ()
