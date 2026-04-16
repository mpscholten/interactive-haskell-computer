-- Phase 2.9.5: Existential constructor using forall form.
-- `forall a.` prefix is parsed and skipped; Wrap gets arity 1.
data Wrap = forall a. Wrap a

unwrap :: Wrap -> Bool
unwrap (Wrap _) = True

main :: IO ()
main = print (unwrap (Wrap (42 :: Int)))
