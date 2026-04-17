-- Rank-N type signature: inner `forall` nested inside a function-argument
-- position. ihc doesn't enforce types at runtime, so the parser must
-- parse-and-skip the inner forall; the function value flows through.
applyPoly :: (forall a. a -> a) -> (Int, Char)
applyPoly f = (f 42, f 'x')

myId :: a -> a
myId x = x

main = print (applyPoly myId)
