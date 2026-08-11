import GHC.Internal.Data.Bits (Bits((.&.)), oneBits)

main = print ((8 .&. 7) :: Int)
