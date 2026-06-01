-- Existential record constructors may carry a parenthesized constraint
-- context before the constructor. GHC.Internal.IO.Handle.Types uses this
-- shape for Handle__, whose haType selector must enter the field registry.
{-# LANGUAGE ExistentialQuantification #-}

class Device a
class Buffered a

data Box = forall dev. (Device dev, Buffered dev) => Box
    { boxDevice :: dev
    , boxName   :: String
    , boxCount  :: Int
    }

main :: IO ()
main = do
    let box = Box () "handle-like" 1
    let box' = box { boxCount = 2 }
    putStrLn (boxName box')
    print (boxCount box')
