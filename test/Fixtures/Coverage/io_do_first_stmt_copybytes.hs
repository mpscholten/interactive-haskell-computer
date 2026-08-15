-- First statement of an IO do is leftover State# VFun
-- (copyBytes = coerce $ \dest src n s -> …; IO newtype dropped).
-- evalDo used to send that to doMonadicSequence / ParsecT because
-- ioSeq starts False. The do carrier is IO, so first-stmt VFun must
-- run as IO. Unannotated / ParsecT first-stmt VFun stays monadic
-- (see megaparsec_do_bind_two).
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (poke, peek)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

main :: IO ()
main = allocaBytes 1 $ \src -> do
    poke (src :: Ptr Word8) 65
    -- Isolation: this inner do's first statement is copyBytes VFun.
    allocaBytes 1 $ \dst -> do
        copyBytes (dst :: Ptr Word8) src 1
        c <- peek (dst :: Ptr Word8)
        print (fromEnum c)
