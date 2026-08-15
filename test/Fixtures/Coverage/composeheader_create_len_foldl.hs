-- Warp composeHeader: signed IO + create len + foldl' fieldLength
-- where.  Unannotated [] at a qualified list type synonym
-- (H.ResponseHeaders = [Header]) must stay the nil constructor.
-- Parser [] shares EVar "[]" with desugared ""; if expandSyn misses
-- H.ResponseHeaders, [] is wrapped in leftover fromString and
-- Int+ sees args=19 <function>.
import Data.List (foldl')
import qualified Data.ByteString as S
import Data.ByteString.Internal (create, ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as H

composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader !httpversion !status !responseHeaders = create len $ \_ -> return ()
  where
    !len = 17 + slen + foldl' fieldLength 0 responseHeaders
    fieldLength !l (!k, !v) = l + S.length (CI.original k) + S.length v + 4
    !slen = S.length $ H.statusMessage status
    _ = httpversion

main :: IO ()
main = do
    bs <- composeHeader H.http11 H.status200 []
    print (S.length bs)
