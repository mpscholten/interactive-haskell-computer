-- Warp hello reduced: apply a Network.Wai Application without Warp.run.
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- Dummy Request via defaultRequest; respond prints status + body marker.
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types (status200, statusCode)

app :: Application
app _ respond = respond $ responseLBS status200 [] "Hello, Warp!"

main :: IO ()
main = do
    _ <- app defaultRequest $ \resp -> do
        print (statusCode (responseStatus resp))
        putStrLn "Hello, Warp!"
        return ResponseReceived
    return ()
