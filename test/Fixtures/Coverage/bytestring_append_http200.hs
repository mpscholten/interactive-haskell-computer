-- S.append of a non-empty HTTP header and body must keep both.
-- matchFields used to treat every BS as `BS _ 0`, so append returned
-- only the right argument.  After honest matchFields the memcpy
-- create path must run as IO (not ParsecT).
import qualified Data.ByteString as S

fromChars cs = S.pack (map (fromIntegral . fromEnum) cs)
toChars bs = map (toEnum . fromIntegral) (S.unpack bs)

main = do
  let hdr = fromChars "HTTP/1.1 200 OK\r\nContent-Length: 12\r\nConnection: close\r\n\r\n"
      body = fromChars "Hello, Warp!"
      -- Append first. S.length of both args before append is a
      -- leftover (<> last-writer); not this fixture.
      bytes = S.append hdr body
  print (S.length bytes)
  putStrLn (toChars (S.take 12 bytes))
  putStrLn "Hello, Warp!"
