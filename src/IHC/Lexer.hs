-- | Minimal streaming lexer for Phase 1.0.
--
-- Single-pass demand-driven design: tokens are produced one at a time from a
-- @Cursor@ against the immutable @Source@. No token list is ever materialized
-- for the whole file. Cursors are cheap values (ints) so the parser/scanner
-- can save and restore them to rewind or skip regions of the file.
--
-- This iteration only handles what's needed to tokenize
--
--   >  main = 42
--
-- plus comments and whitespace. Subsequent slices add operators, strings,
-- class/instance/foreign/type-family keywords, and proper layout.
module IHC.Lexer
    ( -- * Cursor
      Cursor(..)
    , startCursor
      -- * Tokens
    , Token(..)
    , TokenKind(..)
      -- * Stepping
    , nextToken
    , skipTrivia
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)
import Data.Word (Word8)

import IHC.Source

-- | A lexer cursor is just a byte offset + 1-based line/col counters. Layout
-- tracking lands when we grow past single-line bindings.
data Cursor = Cursor
    { cPos  :: !Pos
    , cLine :: !Int
    , cCol  :: !Int
    } deriving (Eq, Show)

startCursor :: Cursor
startCursor = Cursor 0 1 1

data TokenKind
    = TkIdent !ByteString     -- ^ lowercase-start identifier
    | TkConId !ByteString     -- ^ uppercase-start identifier
    | TkInt   !Integer        -- ^ decimal integer literal
    | TkEq                    -- ^ @=@
    | TkPlus                  -- ^ @+@
    | TkMinus                 -- ^ @-@ (only when not followed by another '-')
    | TkStar                  -- ^ @*@
    | TkLParen                -- ^ @(@
    | TkRParen                -- ^ @)@
    | TkLe                    -- ^ @<=@
    | TkIf                    -- ^ keyword @if@
    | TkThen                  -- ^ keyword @then@
    | TkElse                  -- ^ keyword @else@
    | TkNewline               -- ^ one or more newlines; bumps layout
    | TkEof
    deriving (Eq, Show)

data Token = Token
    { tkKind   :: !TokenKind
    , tkStart  :: !Pos
    , tkEnd    :: !Pos
    , tkLine   :: !Int
    , tkCol    :: !Int
    } deriving (Eq, Show)

-- | Advance past spaces, tabs, line comments (@--@). Does not cross newlines —
-- the caller decides whether to treat newline as a token or trivia.
skipTrivia :: Source -> Cursor -> Cursor
skipTrivia s = goSp
  where
    goSp c = case peekByte s (cPos c) of
        Just 0x20 -> goSp (bump c)              -- ' '
        Just 0x09 -> goSp (bump c)              -- '\t'
        Just 0x2D                               -- '-' — possible comment start
            | Just 0x2D <- peekByte s (cPos c + 1) -> goLineComment (bump (bump c))
        _ -> c

    goLineComment c = case peekByte s (cPos c) of
        Just 0x0A -> c                          -- stop at newline; don't consume
        Nothing   -> c
        Just _    -> goLineComment (bump c)

    bump (Cursor p l col) = Cursor (p + 1) l (col + 1)

-- | Produce the next token. Always terminates with 'TkEof' at EOF; callers
-- loop until they see that.
nextToken :: Source -> Cursor -> (Token, Cursor)
nextToken s c0 =
    let c = skipTrivia s c0 in
    case peekByte s (cPos c) of
        Nothing   -> (mkTok TkEof c c, c)
        Just 0x0A -> eatNewlines c
        Just 0x0D -> eatNewlines c             -- treat CR same as LF for newline
        Just b
            | isDigit b        -> lexInt c
            | isLowerStart b   -> lexIdent False c
            | isUpperStart b   -> lexIdent True  c
            | b == 0x3D        -> (mkTok TkEq     c (step c), step c)  -- '='
            | b == 0x2B        -> (mkTok TkPlus   c (step c), step c)  -- '+'
            | b == 0x2A        -> (mkTok TkStar   c (step c), step c)  -- '*'
            | b == 0x2D        -> (mkTok TkMinus  c (step c), step c)  -- '-' (not '--', already skipped)
            | b == 0x28        -> (mkTok TkLParen c (step c), step c)  -- '('
            | b == 0x29        -> (mkTok TkRParen c (step c), step c)  -- ')'
            | b == 0x3C, Just 0x3D <- peekByte s (cPos c + 1)
                               -> let c' = step (step c) in (mkTok TkLe c c', c')  -- '<='
            | otherwise        ->
                error ("IHC.Lexer: unexpected byte 0x"
                       <> showHex b
                       <> " at "
                       <> show (lineCol s (cPos c)))
  where
    mkTok k start end = Token k (cPos start) (cPos end) (cLine start) (cCol start)

    step (Cursor p l col) = case peekByte s p of
        Just 0x0A -> Cursor (p + 1) (l + 1) 1
        _         -> Cursor (p + 1) l (col + 1)

    eatNewlines start = go start
      where
        go c = case peekByte s (cPos c) of
            Just 0x0A -> go (Cursor (cPos c + 1) (cLine c + 1) 1)
            Just 0x0D -> go (Cursor (cPos c + 1) (cLine c + 1) 1)
            _         -> (mkTok TkNewline start c, c)

    lexInt start = go (cPos start)
      where
        go p = case peekByte s p of
            Just b | isDigit b -> go (p + 1)
            _ -> let n   = read (BC.unpack (sliceBytes s (cPos start, p))) :: Integer
                     end = Cursor p (cLine start) (cCol start + (p - cPos start))
                 in (mkTok (TkInt n) start end, end)

    lexIdent isCon start = go (cPos start)
      where
        go p = case peekByte s p of
            Just b | isIdentCont b -> go (p + 1)
            _ -> let bs  = sliceBytes s (cPos start, p)
                     k   | isCon     = TkConId bs
                         | otherwise = keywordOr bs
                     end = Cursor p (cLine start) (cCol start + (p - cPos start))
                 in (mkTok k start end, end)

    keywordOr bs = case bs of
        "if"   -> TkIf
        "then" -> TkThen
        "else" -> TkElse
        _      -> TkIdent bs

isDigit :: Word8 -> Bool
isDigit b = b >= 0x30 && b <= 0x39

isLowerStart :: Word8 -> Bool
isLowerStart b = (b >= 0x61 && b <= 0x7A) || b == 0x5F    -- a-z or '_'

isUpperStart :: Word8 -> Bool
isUpperStart b = b >= 0x41 && b <= 0x5A                   -- A-Z

isIdentCont :: Word8 -> Bool
isIdentCont b =
       isLowerStart b
    || isUpperStart b
    || isDigit b
    || b == 0x27                                           -- '\''

showHex :: Word8 -> String
showHex b = [hex (fromIntegral b `div` 16), hex (fromIntegral b `mod` 16)]
  where
    hex n | n < 10    = chr (0x30 + n)
          | otherwise = chr (0x61 + n - 10)
