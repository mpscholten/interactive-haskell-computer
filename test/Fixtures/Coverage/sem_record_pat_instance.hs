-- Gap: Record pattern on LHS of an instance method equation. Seen in: http-types-0.12.4/Network/HTTP/Types/Status.hs (`instance Eq Status where Status { statusCode = a } == Status { statusCode = b } = a == b`). Ref: warp-dryrun-findings.md (blocker #2).
data Status = Status { statusCode :: Int, statusMessage :: String }

instance Eq Status where
    Status { statusCode = a } == Status { statusCode = b } = a == b

main = do
    print (Status 200 "OK" == Status 200 "Different")
    print (Status 200 "OK" == Status 404 "Not Found")
