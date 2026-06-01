import Prelude (IO, print)
import qualified Network.HTTP.Types as H

main :: IO ()
main = print (H.statusCode H.status200)
