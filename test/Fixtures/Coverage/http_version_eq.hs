-- HttpVersion in http-types uses multi-line record + multi-line deriving.
-- Warp composeHeader branches on httpversion == HttpVersion 1 1.
import Network.HTTP.Types (http11, http10, HttpVersion(..))

main :: IO ()
main = do
    print (http11 == http11)
    print (http11 == http10)
    print (http11 == HttpVersion 1 1)
