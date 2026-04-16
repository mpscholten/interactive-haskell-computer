-- StandaloneDeriving: `deriving instance` at the top level is scanned and
-- ignored (the line is swallowed; no runtime semantics).
{-# LANGUAGE StandaloneDeriving #-}

data Pair a b = Pair a b

deriving instance (Show a, Show b) => Show (Pair a b)

main :: IO ()
main = putStrLn "ok"
