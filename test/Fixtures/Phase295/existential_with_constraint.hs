-- Phase 2.9.5: Existential with class constraint.
-- `forall a. Show a =>` is skipped; arity = 1 dict + 1 field = 2.
-- At runtime MkShow takes (showDict, value).
data MkShow = forall a. Show a => MkShow a

isBox :: MkShow -> Bool
isBox (MkShow _) = True

main :: IO ()
main = print (isBox (MkShow (99 :: Int)))
