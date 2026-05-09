-- Regression: Network.Socket.Options' SocketOption pattern synonyms
-- (NoDelay, KeepAlive, ReuseAddr, Broadcast, ReusePort, Linger, ...)
-- must resolve via the source-loaded path, not via per-name host shims.
-- Exercises BOTH expression and pattern positions to flush the binding-
-- scanner value path AND the IHC.PatSyn registry path.  `Linger` has no
-- host shim and is the load-bearing case here — it MUST come from
-- source-loaded Network.Socket.Options regardless of the shim state.
--
-- Avoid `show` on `SocketOption` directly: `instance Show SocketOption`
-- routes through `bijectiveShow socketOptionBijection`, a 33-entry
-- patsyn list that taxes discovery.  Always destructure `SockOpt l n`
-- first and `show` the underlying `Int`.
import Network.Socket (SocketOption(..))

main :: IO ()
main = do
    case NoDelay of
        SockOpt l n -> putStrLn ("NoDelay "    ++ show l ++ " " ++ show n)
    case ReuseAddr of
        SockOpt l n -> putStrLn ("ReuseAddr "  ++ show l ++ " " ++ show n)
    case KeepAlive of
        SockOpt l n -> putStrLn ("KeepAlive "  ++ show l ++ " " ++ show n)
    case Linger of                      -- no shim; exercises source path
        SockOpt l n -> putStrLn ("Linger "     ++ show l ++ " " ++ show n)
    case NoDelay of                     -- pattern position
        NoDelay -> putStrLn "pat NoDelay ok"
        _       -> putStrLn "pat NoDelay fail"
