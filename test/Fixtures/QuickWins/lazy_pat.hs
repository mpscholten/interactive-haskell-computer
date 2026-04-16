-- Lazy/irrefutable pattern: ~pat.  Under ihc's already-lazy evaluator,
-- ~(x, y) has the same runtime semantics as (x, y), so we strip the
-- tilde at parse time.  Used pervasively by transformers and bytestring.
main = let ~(x, y) = (1 :: Int, 2 :: Int) in print (x + y)
