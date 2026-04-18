import qualified Data.Maybe as M

main :: IO ()
main = do
  let x = M.Nothing :: Maybe Int
  case x of
    M.Nothing -> putStrLn "nothing"
    M.Just _  -> putStrLn "just"
