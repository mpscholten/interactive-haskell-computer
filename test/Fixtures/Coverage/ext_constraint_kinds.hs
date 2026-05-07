-- Gap: `ConstraintKinds` — naming a tuple of constraints with `type`. Seen in: IHP 22 files (`type ModelConstraints a = (Eq a, Show a, ...)`). Ref: ihp-unsupported-scan.md.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

type Showable a = (Show a, Eq a)

describe :: Showable a => a -> String
describe x = show x ++ " / self-eq=" ++ show (x == x)

main = putStrLn (describe (42 :: Int))
