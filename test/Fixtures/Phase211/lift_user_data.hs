-- Phase 2.11: TH lift splice for a user-defined ADT
-- Auto-derived Lift via liftVal's generic VCon path
import Language.Haskell.TH (lift)

data Color = Red | Green | Blue

main :: IO ()
main = do
    let r = $(lift Red)
    case r of
        Red   -> putStrLn "Red"
        Green -> putStrLn "Green"
        Blue  -> putStrLn "Blue"
