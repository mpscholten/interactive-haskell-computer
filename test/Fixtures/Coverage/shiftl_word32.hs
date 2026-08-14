-- HostAddress pack helper: fromIntegral/shiftL on Word32 used to die
-- as W32#/I# args=127 24 because only W#/W8# had a VInt matchPat bridge.
import Data.Bits
import Data.Word
import Network.Socket (tupleToHostAddress, hostAddressToTuple)

main :: IO ()
main = do
    print (127 `shiftL` 24 :: Word32)
    let ha = tupleToHostAddress (127, 0, 0, 1)
    print ha
    print (hostAddressToTuple ha)
