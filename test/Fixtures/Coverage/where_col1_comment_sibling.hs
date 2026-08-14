-- Column-1 line comment inside a where-block must not end the binding.
-- Network.Socket.Syscall.connect has
--   _ | err == eINPROGRESS -> connectBlocked
-- --           _ | err == eAGAIN      -> connectBlocked
--   connectBlocked = do ...
-- findBodyEnd used to treat `--` at column 1 as the next top-level decl,
-- dropping connectBlocked (unbound at runtime).
import Control.Monad (when)

f :: Int -> IO Int
f _ = loop 0
  where
    loop fd = do
        r <- return (-1 :: Int)
        when (r == -1) $ do
            err <- return (1 :: Int)
            case () of
              _ | err == 0 -> loop fd
              _ | err == 1 -> connectBlocked
--           _ | err == 2 -> connectBlocked
              _otherwise   -> return 0

    connectBlocked = do
        return (99 :: Int)

main :: IO ()
main = do
    n <- f 0
    print n
