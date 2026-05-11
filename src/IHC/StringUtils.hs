-- | Shared ASCII string/ByteString utilities.
--
-- Several modules (Classes, Elaborate, Scan, Parser) had each grown a
-- local copy of the same two helpers — a four-char ASCII space
-- predicate (' ', '\\t', '\\n', '\\r') and a both-sides whitespace
-- trim built on it. This module is the canonical home so they don't
-- drift out of sync.
--
-- Note: the predicate intentionally matches only the four ASCII
-- whitespace characters that appear in Haskell source. It is /not/ a
-- substitute for 'Data.Char.isSpace', which also matches '\\v', '\\f',
-- and Unicode space characters. Use 'Data.Char.isSpace' if you need
-- the broader definition.
module IHC.StringUtils
    ( isAsciiSpace
    , trimAscii
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

-- | True for the four ASCII whitespace characters that appear in
-- Haskell source: space, tab, newline, carriage return.
isAsciiSpace :: Char -> Bool
isAsciiSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Drop leading and trailing 'isAsciiSpace' bytes from a ByteString.
-- Equivalent to the @reverse . dropWhile p . reverse . dropWhile p@
-- idiom that was duplicated across several modules.
trimAscii :: ByteString -> ByteString
trimAscii s =
    BC.dropWhile isAsciiSpace
        (BC.reverse (BC.dropWhile isAsciiSpace (BC.reverse s)))
