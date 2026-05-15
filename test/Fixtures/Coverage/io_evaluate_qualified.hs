-- Same as io_evaluate.hs but with an explicit import list, exercising
-- the named-import resolution path for `evaluate` (source-loaded from
-- GHC.Internal.IO; backed by the `seq#` GHC.Prim primop). Deterministic
-- stdout only -- no `show` on the caught SomeException.
import Control.Exception (evaluate, try, SomeException)

main :: IO ()
main = do
    v <- evaluate ((6 * 4) :: Int)
    print v
    r <- try (evaluate (error "qbang" :: Int)) :: IO (Either SomeException Int)
    putStrLn (case r of
                Left _  -> "caught-q"
                Right x -> "got " ++ show x)
    r2 <- try (evaluate ((100 + 23) :: Int)) :: IO (Either SomeException Int)
    putStrLn (case r2 of
                Left _  -> "caught2-q"
                Right x -> "got " ++ show x)
