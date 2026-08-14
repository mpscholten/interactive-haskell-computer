-- Network.Socket.socket wraps create in bracketOnError.
-- If this hangs or skips the body, bindPortTCP never reaches c_socket.
import Control.Exception (bracketOnError)

main = do
    n <- bracketOnError (pure (1 :: Int)) (\_ -> pure ()) (\x -> pure (x + 1))
    print n
