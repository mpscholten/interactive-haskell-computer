-- Inference can determine `box ~ []`, but there is deliberately no Select []
-- instance.  The metadata rewrite must be rejected and evaluation must fall
-- back once, without retrying shorter application prefixes.
class Select box where
    select :: a -> box a -> a

main :: Int
main = select 0 [1]
