-- Phase 2.11: TH lift splice for Int literal
import Language.Haskell.TH (lift)

main :: IO ()
main = print $(lift (42 :: Int))
