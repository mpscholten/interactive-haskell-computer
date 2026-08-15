{-# LANGUAGE TypeApplications #-}
-- Warp serveConnection: `if "PRI " `S.isPrefixOf` bs0`.
-- isPrefixOf ends in `return $! i == 0` tagged with a leftover
-- carrier that has no Monad instance.  Tag-driven miss must still
-- apply the value argument via the result-poly default (IO), not
-- leak leftover <classMethod return> into `if`.
import Data.ByteString (isPrefixOf, pack)
import System.IO.Unsafe (unsafePerformIO)

data Payload a = Payload a

main :: IO ()
main = do
    putStrLn (if pack "PRI " `isPrefixOf` pack "PRI * HTTP/2.0" then "h2" else "h1")
    putStrLn (if pack "PRI " `isPrefixOf` pack "GET / HTTP/1.1" then "h2" else "h1")
    putStrLn (if flag then "yes" else "no")
  where
    flag = unsafePerformIO $ do
        n <- return (0 :: Int)
        return @Payload $! n == 0
