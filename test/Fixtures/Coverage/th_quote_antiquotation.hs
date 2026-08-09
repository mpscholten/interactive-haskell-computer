{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (runQ)

main :: IO ()
main = do
    let hole = [| 40 |]
    expression <- runQ [| $hole + 2 |]
    putStrLn (take 4 (show expression))
