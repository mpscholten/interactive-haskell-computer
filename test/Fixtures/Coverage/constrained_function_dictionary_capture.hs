{-# LANGUAGE AllowAmbiguousTypes #-}

class Witness a where
    witness :: String

instance Witness Int where
    witness = "Int"

instance Witness Bool where
    witness = "Bool"

captured :: Witness a => a -> (() -> String)
captured _ = \() -> witness

main :: IO ()
main = do
    putStrLn (captured (1 :: Int) ())
    putStrLn (captured True ())
