-- Gap: `ScopedTypeVariables` — inner `forall a.` scope reused in function body. Seen in: IHP 30+ files (TransactionRunner.runInTransaction). Ref: ihp-unsupported-scan.md (table 1).
{-# LANGUAGE ScopedTypeVariables #-}

pairWithSame :: forall a. a -> (a, a)
pairWithSame x =
    let y = x :: a
    in (x, y)

main = print (pairWithSame (42 :: Int))
