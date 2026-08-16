newtype Wrap = Wrap { unWrap :: Maybe Int }

main :: IO ()
main = print (unWrap (Wrap (Just 3)))
