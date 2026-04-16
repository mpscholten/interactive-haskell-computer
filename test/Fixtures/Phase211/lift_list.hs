-- Phase 2.11: TH lift splice for list of Ints
import Language.Haskell.TH (lift)

main :: IO ()
main = print $(lift ([1,2,3] :: [Int]))
