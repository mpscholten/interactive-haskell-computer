-- Gap: Type annotation `(pat :: Type)` inside a parenthesised pattern. Seen in: IHP/RouterSupport.hs:1:22. Ref: ihp-parser-gaps.md (bucket 3).
import Control.Exception (SomeException, try, evaluate)

main = do
    r <- try (evaluate (div 1 0))
    case r of
        Left (e :: SomeException) -> putStrLn ("caught: " ++ show e)
        Right v                   -> print v
