-- Empty [] at a qualified list-synonym expected type is the nil
-- constructor, not OverloadedStrings fromString.
--
-- responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
-- stores H.ResponseHeaders in the scheme; the scanner registers the
-- synonym under the bare name ResponseHeaders.  Pre-fix expandSyn
-- missed the qualified key, wrapped [] in fromString, and the
-- Response headers field was leftover <function>.
-- Import the defining module so the synonym is registered.
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Header ()

idH :: H.ResponseHeaders -> H.ResponseHeaders
idH xs = xs

main :: IO ()
main = print (idH [])
