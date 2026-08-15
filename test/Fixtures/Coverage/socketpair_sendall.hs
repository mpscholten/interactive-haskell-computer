-- Gap: socketPair leftover — peekArray 2 fdArr still mashes fd2=0 so
-- sendAll hits ENOTTY / "Socket operation on non-socket". FFI Ptr CInt
-- mark is landed; peekArray dest mark does not fire on this tree.
-- No send/sendAll shim.
import qualified Data.ByteString.Char8 as C8
import Network.Socket
import Network.Socket.ByteString (sendAll)

main :: IO ()
main = do
  (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
  sendAll s1 (C8.pack "ab")
  sendAll s2 (C8.pack "cd")
  close s1
  close s2
  putStrLn "ok"
