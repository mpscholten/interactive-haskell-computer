-- DataKinds Tier 1: symbolVal / natVal runtime dispatch.
-- Both the explicit type annotation form and the @TypeApplications
-- form should yield the lifted type literal at the value level.
import Data.Proxy
import GHC.TypeLits (symbolVal, KnownSymbol, natVal, KnownNat)

main = do
    putStrLn (symbolVal (Proxy :: Proxy "hello"))
    putStrLn (symbolVal (Proxy @"world"))
    print (natVal (Proxy :: Proxy 42))
    print (natVal (Proxy @99))
