-- Gap: `RankNTypes` — nested `forall` in an argument position. Seen in: IHP/ModelSupport/Types.hs:73 (`runInTransaction :: forall a. ...`), mtl/transformers, hasql Session API. Ref: ihp-unsupported-scan.md.
{-# LANGUAGE RankNTypes #-}

applyBoth :: (forall a. a -> a) -> (Int, String) -> (Int, String)
applyBoth f (x, y) = (f x, f y)

main = print (applyBoth id (42, "hello"))
