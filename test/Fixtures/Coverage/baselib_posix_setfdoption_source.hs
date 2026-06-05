-- Builtins-removal regression: System.Posix.IO.setFdOption should run
-- through unix source and only use FFI at c_fcntl_read/write.
import Network.Socket
import System.Posix.IO (FdOption(..), queryFdOption, setFdOption)
import System.Posix.Types (Fd(..))

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    fd <- fdSocket sock
    let posixFd = Fd fd
    setFdOption posixFd CloseOnExec True
    enabled <- queryFdOption posixFd CloseOnExec
    print enabled
    close sock
