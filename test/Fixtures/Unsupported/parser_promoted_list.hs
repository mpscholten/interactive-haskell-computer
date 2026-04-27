-- Gap: Promoted list syntax `'[...]` in type position. Seen in: servant-server-0.20.3.0/Server/Server.hs:1:46 (`Proxy :: Proxy '[]`). Ref: hackage-parser-gaps.md (servant-server novel bucket).
{-# LANGUAGE DataKinds #-}

data Proxy a = Proxy

witness :: Proxy '[Int, Bool]
witness = Proxy

main = case witness of
    Proxy -> putStrLn "ok"
