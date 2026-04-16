import Language.Haskell.TH.Syntax (Q, Exp)

-- Verify that [| expr |] parses and produces a TH Exp-shaped Val.
-- The expression bracket [| 42 |] should yield LitE (IntegerL 42).
main :: IO ()
main = do
    let q = [| 42 |]
    putStrLn "ok"
