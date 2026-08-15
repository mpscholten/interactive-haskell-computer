-- Isolated leftover: Application-shaped type synonym + respond continuation + IO.
-- Custom ADT — no Wai / Warp names.  Same calling convention as
--   type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
data Request = DummyRequest
data Response = Response String
data ResponseReceived = ResponseReceived

type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

app :: Application
app _ respond = respond (Response "Hello, Warp!")

main :: IO ()
main = do
    _ <- app DummyRequest $ \resp -> do
        case resp of
            Response s -> putStrLn s
        return ResponseReceived
    return ()
