-- Gap: sendResponse of responseLBS hangs after calling-sendResponse.
-- After toLazyByteString of a non-empty Builder is GREEN (0 then 12):
-- Extra.runBuilder, composeHeader http11 status200 [],
-- composeHeader Date+CL (76), Extra.runBuilder (hdr <> body) (88 / Done),
-- and toBufIOWith of that Builder are GREEN. indexResponseHeader /
-- mapM_ writeArray, addServer+addDate, tickle emptyHandle, create+copy
-- first-stmt are GREEN. sendRsp is composeHeaderBuilder + toBufIOWith
-- (RspBuilder) or composeHeader >>= connSendAll (RspNoBody / 204).
-- Both sendResponse 200 and 204 hang; standalone composeHeader /
-- toBufIOWith do not. Not Date cache (Date supplied). Not Warp.run.
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM (newTVarIO)
import Data.IORef
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Types (status200)
import Network.Socket (SockAddr (SockAddrInet))
import Network.Wai (responseLBS, defaultRequest)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.Warp.Internal
import System.TimeManager (emptyHandle, defaultManager)

main :: IO ()
main = do
    acc <- newIORef S.empty
    wb <- createWriteBuffer 16384
    wbRef <- newIORef wb
    h2 <- newIORef False
    apps <- newTVarIO 0
    let conn = Connection
            { connSendMany = \_ -> return ()
            , connSendAll = \bs -> modifyIORef acc (`S.append` bs)
            , connSendFile = \_ _ _ _ _ -> return ()
            , connClose = return ()
            , connRecv = return S.empty
            , connRecvBuf = \_ _ -> return True
            , connWriteBuffer = wbRef
            , connHTTP2 = h2
            , connMySockAddr = SockAddrInet 0 0
            , connAppsInProgress = apps
            }
        ii = InternalInfo
                defaultManager
                (return "Thu, 01 Jan 1970 00:00:00 GMT")
                (\_ -> return (Nothing, return ()))
                (\_ -> error "getFileInfo unused")
        resp = responseLBS status200
                [ ("Date", "Thu, 01 Jan 1970 00:00:00 GMT")
                , ("Content-Length", "12")
                ]
                (LC.pack "Hello, Warp!")
    _ <- sendResponse defaultSettings conn ii emptyHandle
            defaultRequest defaultIndexRequestHeader (return S.empty) resp
    got <- readIORef acc
    print (S.length got)
