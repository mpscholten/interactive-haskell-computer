-- Nested source-IO do inside a continuation (accept / withFdSocket shape).
-- First action is source-constructed IO (VCon "IO"), not host VIO.
-- evalDo used to send that to doMonadicSequence → ParsecT, so the
-- result of `return (a, b)` was <ParsecT...> and the tuple bind failed.
wrap :: IO a -> IO a
wrap k = k

main :: IO ()
main = do
    r <- wrap $ do
        a <- return (1 :: Int)
        b <- return (2 :: Int)
        return (a, b)
    case r of
        (x, y) -> print (x + y)
