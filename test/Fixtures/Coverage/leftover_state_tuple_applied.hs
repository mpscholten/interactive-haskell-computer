-- leftover (# s, a #) applied as a function.  Inverse of leftover
-- State# VFun rematch (thenIO matches leftover VFun as (#,#)).
-- leftover IO applied once produces leftover (#,#); the second apply
-- is the rematch.  Custom newtype so the first apply is newtype-
-- transparent (same shape as a single-field carrier body).  No
-- megaparsec / combinator names.
newtype Box a = Box { runBox :: Int -> (a -> String) -> String }

leftover = return (\k -> k 42)

main = putStrLn (runBox leftover 0 show)
