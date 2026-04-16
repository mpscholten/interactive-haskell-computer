-- Phase 2.11: TH lift splice for String
import Language.Haskell.TH (lift)

main :: IO ()
main = putStrLn $(lift "hello")
