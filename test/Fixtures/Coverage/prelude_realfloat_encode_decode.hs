-- Builtins-removal: encodeFloat / decodeFloat source-load through
-- RealFloat Double rather than host shims in IHC.Builtins.
--
-- encodeFloat is result-polymorphic (Integer -> Int -> a), so optimistic
-- interpretation defaults it to the runtime VFloat tag "Double". decodeFloat
-- is argument-directed and dispatches from the Double value.
module Main where

main :: IO ()
main = do
    print (encodeFloat 3 4 :: Double)
    case decodeFloat (1.5 :: Double) of
        (m, e) -> putStrLn (show m ++ " * 2^" ++ show e)
