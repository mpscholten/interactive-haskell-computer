import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3099 $ \_ respond ->
    respond $ responseLBS status200 [] "Hello, Warp!"
