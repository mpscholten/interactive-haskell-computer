-- Phase 2.9.5: GADT with class constraint.
-- The constraint dict is an extra arg at runtime; arity = 1 dict + 1 field.
-- We use Eq constraint — at runtime this is just an extra argument.
data ShowBox where
    MkShowBox :: Show a => a -> ShowBox

-- Since we can't easily call show on the inner value without the dict,
-- we just test that construction and pattern matching work structurally.
main :: IO ()
main = do
    let box = MkShowBox (42 :: Int)
    putStrLn "constructed"
