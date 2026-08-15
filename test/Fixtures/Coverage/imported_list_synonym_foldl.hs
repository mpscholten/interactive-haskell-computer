-- Imported list type synonym in a function signature must expand
-- before [] is treated as an OverloadedStrings literal.
-- H.ResponseHeaders = [Header]; unexpanded, go [] became
-- fromString leftover and Int+ died as I# I# args=19 <function>
-- (Warp composeHeader !len = 17 + slen + foldl' …).
import Data.List (foldl')
import qualified Network.HTTP.Types as H

go :: H.ResponseHeaders -> Int
go xs = 17 + 2 + foldl' (\n _ -> n) 0 xs

main :: IO ()
main = print (go [])
