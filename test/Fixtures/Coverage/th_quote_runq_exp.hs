-- Template Haskell quote `[| ... |]` evaluated at runtime via runQ.
-- Proves that quotations produce an Exp AST without requiring the
-- full splice machinery. Reads the first few chars of show to keep
-- the .out stable against minor pprint formatting changes.
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (runQ)

main :: IO ()
main = do
    e <- runQ [| 1 + 2 |]
    putStrLn (take 4 (show e))
