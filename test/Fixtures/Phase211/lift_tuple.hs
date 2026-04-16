-- Phase 2.11: TH lift splice for a tuple
import Language.Haskell.TH (lift)

main :: IO ()
main = print $(lift (1 :: Int, "a"))
