-- Regression: Network.Socket.Options' SocketOption pattern synonyms
-- (NoDelay, KeepAlive, ReuseAddr, Linger, ...) must resolve via the
-- source-loaded path, not via per-name host shims.  Exercises BOTH
-- expression and pattern positions to flush the binding-scanner
-- value path AND the IHC.PatSyn registry path.
--
-- `Linger` has no host shim (and never did) — it's the load-bearing
-- case proving the source-loaded path works for SocketOption patsyns.
--
-- Cross-platform note: only NoDelay (TCP_NODELAY=1 at IPPROTO_TCP=6)
-- has values that match across macOS and Linux.  ReuseAddr / KeepAlive
-- / Linger sit at SOL_SOCKET, which is 65535 on macOS and 1 on Linux.
-- We assert NoDelay's exact values, and for the others only verify
-- that the source-loaded patsyn resolves to a SockOpt structurally.
--
-- Avoid `show` on `SocketOption` directly: `instance Show SocketOption`
-- routes through `bijectiveShow socketOptionBijection`, a 33-entry
-- patsyn list that taxes discovery.  Always destructure first.
import Network.Socket (SocketOption(..))

main :: IO ()
main = do
    -- Exact values: portable across macOS / Linux.
    case NoDelay of
        SockOpt 6 1 -> putStrLn "NoDelay 6 1"
        SockOpt l n -> putStrLn ("NoDelay UNEXPECTED " ++ show l ++ " " ++ show n)
    -- Structural-only: SOL_SOCKET differs by platform.  These shims
    -- previously hard-coded macOS values (65535) and silently returned
    -- the wrong dispatch on Linux — yet another reason to source-load.
    case ReuseAddr of
        SockOpt _ _ -> putStrLn "ReuseAddr ok"
    case KeepAlive of
        SockOpt _ _ -> putStrLn "KeepAlive ok"
    case Linger of                      -- never had a shim
        SockOpt _ _ -> putStrLn "Linger ok"
    case NoDelay of                     -- pattern position via PatSyn registry
        NoDelay -> putStrLn "pat NoDelay ok"
        _       -> putStrLn "pat NoDelay fail"
