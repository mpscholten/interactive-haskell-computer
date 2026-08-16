{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

main :: IO ()
main = run 3110 $ \_ respond -> do
    respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Warp!"
