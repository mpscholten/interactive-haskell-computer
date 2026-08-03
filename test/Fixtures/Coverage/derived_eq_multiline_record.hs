-- Multi-line record + multi-line deriving must register stock Eq.
-- Pre-fix, '{' on the line after the ctor name was not recognized as
-- record syntax, so derived Eq never registered and Eq.== fell through
-- to the class default (==) = not (/=) / (/=) = not (==) infinite spin.
-- That blocked warp composeHeader (HttpVersion equality).
import System.IO (hFlush, stdout)

data HV = HV
    { maj :: !Int
    , min_ :: !Int
    }
    deriving
        ( Eq
        , Show
        )

main :: IO ()
main = do
    print (HV 1 1 == HV 1 1)
    print (HV 1 1 == HV 1 0)
    print (HV 2 0 == HV 2 0)
