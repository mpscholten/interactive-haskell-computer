-- makeGracefulRecv shape: atomically (checkShutdown $> True <|> sockWait $> False)
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as S
import Network.Socket.STM (waitReadSocketSTM)
import Data.Functor (($>))

main = do
  (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
  sockWait <- waitReadSocketSTM s1
  _ <- forkIO $ do
    threadDelay 100000
    NSB.sendAll s2 ("late" :: S.ByteString)
  shuttingDown <- newTVarIO False
  isShuttingDown <- atomically $
    (do sd <- readTVar shuttingDown; check sd; pure True)
    <|> (sockWait $> False)
  print isShuttingDown
  msg <- NSB.recv s1 100
  print (S.length msg)
