-- where ?ip = e must wrap the enclosing RHS in EImplicitLet
-- (same as `let ?ip = e in body`).  GHC.Internal.Exception
-- errorCallWithCallStackException binds `where ?callStack = stk`.
f :: Int -> String
f stk = show (?callStack :: Int)
  where ?callStack = stk

main :: IO ()
main = putStrLn (f 7)
