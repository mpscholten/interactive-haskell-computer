-- Word8 must use Num Word8 (modular), not Num Int.
-- Pre-fix W8# collapsed to bare VInt → typeTagOf "Int" → wrong arithmetic.
import Data.Word (Word8)
import Data.Word8 (_0)

main :: IO ()
main = do
    print ((5 :: Word8) - _0)           -- 213, not -43
    print ((5 :: Word8) + 251)          -- 0 (wrap)
    print (_0 + fromIntegral (2 :: Int) :: Word8)  -- 50
