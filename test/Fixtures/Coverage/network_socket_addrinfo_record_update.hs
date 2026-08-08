-- Coverage: Network.Socket re-exports AddrInfo(..) from Info.  Record
-- update/access via the facade (and the NS. alias streaming-commons uses)
-- must see Info's field registry.  Requires parseModuleHeader to skip
-- leading {-# LANGUAGE #-} so Socket is not mis-read as ExportAll.
import qualified Network.Socket as NS

main :: IO ()
main = do
    let h = NS.defaultHints
            { NS.addrSocketType = NS.Stream
            , NS.addrFlags = [NS.AI_PASSIVE]
            }
    print (NS.addrSocketType h == NS.Stream)
    print (NS.AI_PASSIVE `elem` NS.addrFlags h)
