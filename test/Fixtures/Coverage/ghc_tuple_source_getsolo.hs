import GHC.Tuple

main :: IO ()
main = do
  print (getSolo (MkSolo (7 :: Int)))
  case MkSolo "ok" of
    Solo s -> putStrLn s
