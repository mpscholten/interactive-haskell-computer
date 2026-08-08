-- Data.ByteString.foldl' uses nested where inside let; must source-load.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S

main :: IO ()
main = do
  print (S.foldl' (\a _ -> a + 1) (0 :: Int) ("abc" :: S.ByteString))
  print (S.foldl' (\a w -> a + fromIntegral w) (0 :: Int) ("ab" :: S.ByteString))
  -- readInt64 shape used by warp Content-Length / digit parse
  print (S.foldl' (\ !i !c -> i * 10 + fromIntegral (c - 0x30)) (0 :: Int) ("200" :: S.ByteString))
