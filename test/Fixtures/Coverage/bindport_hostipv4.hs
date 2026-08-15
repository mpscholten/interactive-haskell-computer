-- Warp runSettings does bindPortTCP (settingsPort set) (settingsHost set).
-- Library Data.Streaming.Network calls NS.socket after Network.Socket
-- re-exports Syscall.socket.  A stale FFI pin without a host builtin
-- drops that facade sentinel (self-loop), so library NS.socket hangs
-- while a user-file `import Network.Socket (socket)` still works.
-- HostIPv4 constructor — no fromString, no Warp, no accept.
import Data.Streaming.Network (bindPortTCP)
import Data.Streaming.Network.Internal (HostPreference(..))
import Network.Socket (close)

main :: IO ()
main = do
    s <- bindPortTCP 15100 HostIPv4
    close s
    putStrLn "ok"
