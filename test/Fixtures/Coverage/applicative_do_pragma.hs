{-# LANGUAGE ApplicativeDo #-}
-- ApplicativeDo pragma is accepted and silently skipped; the do-block is
-- desugared monadically (>>=), which produces identical results because
-- Applicative is a superclass of Monad.

greet name = putStrLn ("hello " ++ name)

main = do
    greet "alice"
    greet "bob"
    let msg = "done"
    putStrLn msg
