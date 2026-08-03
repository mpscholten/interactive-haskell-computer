-- waitReadSocketSTM + atomically after data is ready.
-- Warp makeGracefulRecv bottoms out here for cancellable recv.
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as S
import Network.Socket.STM (waitReadSocketSTM)

main = do
  (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
  NSB.sendAll s2 ("data" :: S.ByteString)
  wait <- waitReadSocketSTM s1
  atomically wait
  msg <- NSB.recv s1 100
  print (S.length msg)
