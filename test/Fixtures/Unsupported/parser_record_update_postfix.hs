-- Gap: Record-update postfix `expr { field = val }` after arbitrary expression. Seen in: IHP/AutoRefresh.hs:3:44, lens-5.3.6/Control/Lens/Internal/PrismTH.hs:1:17, hasql-pool-1.4.2/Hasql/Pool/Config/Setting.hs:2:30. Ref: ihp-parser-gaps.md bucket 8.
data Settings = Settings { sport :: Int, shost :: String }

defaultSettings :: Settings
defaultSettings = Settings { sport = 80, shost = "localhost" }

main = do
    let bump = \s -> s { sport = 8080 }
    let s' = bump defaultSettings
    print (sport s')
