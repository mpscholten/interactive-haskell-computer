-- | Minimal streaming lexer for Phase 1.0.
--
-- Single-pass demand-driven design: tokens are produced one at a time from a
-- @Cursor@ against the immutable @Source@. No token list is ever materialized
-- for the whole file. Cursors are cheap values (ints) so the parser/scanner
-- can save and restore them to rewind or skip regions of the file.
--
-- Slice-1 expansion (bytestring-survey): pragma + nested block comment
-- skipping, extended keyword set (@module@/@import@/@qualified@/...), new
-- symbol tokens (@.@ @..@ @\@@ @!@ @\`@ @\\@ @$@ @=>@ @<-@), hex/octal int
-- literals, and a generic @TkSymOp@ token that catches user-defined symbolic
-- operators like @<>@, @>>=@, @<$>@, @>$<@, @.&.@, etc.
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
    | TkInt   !Integer        -- ^ integer literal (decimal, @0x...@ hex, @0o...@ octal)
    | TkFloat !Double         -- ^ floating-point literal (e.g. @1.5@, @1e9@, @1.5e-3@)
    | TkStr   !ByteString     -- ^ @\"...\"@ string literal (contents after
                              --   basic \\n \\t \\\\ \\\" escapes)
    | TkChar  !Char           -- ^ @\'c\'@ character literal
    | TkEq                    -- ^ @=@
    | TkPlus                  -- ^ @+@
    | TkPlusPlus              -- ^ @++@ (string / list concat)
    | TkMinus                 -- ^ @-@ (only when not followed by another '-')
    | TkStar                  -- ^ @*@
    | TkLParen                -- ^ @(@
    | TkRParen                -- ^ @)@
    | TkLe                    -- ^ @<=@
    | TkLt                    -- ^ @<@
    | TkGe                    -- ^ @>=@
    | TkGt                    -- ^ @>@
    | TkEqEq                  -- ^ @==@
    | TkNeq                   -- ^ @/=@
    | TkAnd                   -- ^ @&&@
    | TkOr                    -- ^ @||@
    | TkIf                    -- ^ keyword @if@
    | TkThen                  -- ^ keyword @then@
    | TkElse                  -- ^ keyword @else@
    | TkCase                  -- ^ keyword @case@
    | TkOf                    -- ^ keyword @of@
    | TkUnderscore            -- ^ wildcard pattern @_@
    | TkLet                   -- ^ keyword @let@
    | TkIn                    -- ^ keyword @in@
    | TkWhere                 -- ^ keyword @where@
    | TkDo                    -- ^ keyword @do@
    | TkData                  -- ^ keyword @data@
    | TkModule                -- ^ keyword @module@
    | TkImport                -- ^ keyword @import@
    | TkQualified             -- ^ keyword @qualified@
    | TkAs                    -- ^ keyword @as@
    | TkHiding                -- ^ keyword @hiding@
    | TkNewtype               -- ^ keyword @newtype@
    | TkTypeKw                -- ^ keyword @type@
    | TkClass                 -- ^ keyword @class@
    | TkInstance              -- ^ keyword @instance@
    | TkDeriving              -- ^ keyword @deriving@
    | TkInfixL                -- ^ keyword @infixl@
    | TkInfixR                -- ^ keyword @infixr@
    | TkInfix                 -- ^ keyword @infix@
    | TkBar                   -- ^ @|@ (data-decl alternative separator)
    | TkLBrace                -- ^ @{@
    | TkRBrace                -- ^ @}@
    | TkLBracket              -- ^ @[@
    | TkRBracket              -- ^ @]@
    | TkComma                 -- ^ @,@
    | TkColon                 -- ^ @:@ (list cons; Phase 2.2)
    | TkSemi                  -- ^ @;@
    | TkDColon                -- ^ @::@ (type-signature separator)
    | TkArrow                 -- ^ @->@ (type-arrow; also lambda body)
    | TkDArrow                -- ^ @=>@ (class-constraint arrow)
    | TkLArrow                -- ^ @<-@ (do-notation bind)
    | TkDot                   -- ^ @.@ (qualified-name separator / composition)
    | TkDotDot                -- ^ @..@ (export-all in Tree(..))
    | TkAt                    -- ^ @\@@ (as-pattern)
    | TkBang                  -- ^ @!@ (bang pattern, strict field)
    | TkBacktick              -- ^ @`@ (backtick infix)
    | TkBackslash             -- ^ @\\@ (lambda)
    | TkDollar                -- ^ @$@
    | TkPrimId !ByteString    -- ^ @name#@ / @name##@ MagicHash identifier
                              --   (e.g. @I#@, @runRW#@, @newByteArray#@)
    | TkLUnbox                -- ^ @(#@ unboxed-tuple open
    | TkRUnbox                -- ^ @#)@ unboxed-tuple close
    | TkSymOp !ByteString     -- ^ generic user-defined symbolic operator
                              --   (e.g. @<>@, @>>=@, @<$>@, @.&.@, @:|@)
    | TkImplicitRef !ByteString -- ^ @?name@ implicit parameter reference
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

-- | Advance past spaces, tabs, line comments (@--@), nested block comments
-- (@{- ... -}@), and pragmas (@{-# ... #-}@). Does not cross newlines — the
-- caller decides whether to treat newline as a token or trivia.
--
-- Block comments and pragmas MAY span multiple lines; when they do we keep
-- skipping past the enclosed newlines (they act like whitespace there).
skipTrivia :: Source -> Cursor -> Cursor
skipTrivia s = goSp
  where
    goSp c = case peekByte s (cPos c) of
        Just 0x20 -> goSp (bump c)              -- ' '
        Just 0x09 -> goSp (bump c)              -- '\t'
        Just 0x2D                               -- '-' — possible line comment
            | Just 0x2D <- peekByte s (cPos c + 1) -> goLineComment (bumpN 2 c)
        Just 0x7B                               -- '{' — pragma or block comment?
            | Just 0x2D <- peekByte s (cPos c + 1)
            , Just 0x23 <- peekByte s (cPos c + 2)
                -> goPragma (bumpN 3 c)            -- '{-#'
            | Just 0x2D <- peekByte s (cPos c + 1)
                -> goBlockComment 1 (bumpN 2 c)    -- '{-'
        _ -> c

    goLineComment c = case peekByte s (cPos c) of
        Just 0x0A -> c                          -- stop at newline; don't consume
        Nothing   -> c
        Just _    -> goLineComment (bump c)

    -- | Pragma: skip until @#-}@ then loop back to goSp.
    goPragma c = case peekByte s (cPos c) of
        Nothing -> c
        Just 0x23                                  -- '#'
            | Just 0x2D <- peekByte s (cPos c + 1)
            , Just 0x7D <- peekByte s (cPos c + 2)
                -> goSp (bumpN 3 c)
        Just 0x0A -> goPragma (newline c)
        Just _    -> goPragma (bump c)

    -- | Nested block comments. Depth counter; start at depth=1 just past
    -- the opening @{-@. @{- {- -} -}@ should fully close only at depth 0.
    goBlockComment :: Int -> Cursor -> Cursor
    goBlockComment 0 c = goSp c
    goBlockComment !d c = case peekByte s (cPos c) of
        Nothing -> c                                  -- unterminated; bail
        Just 0x7B                                     -- '{' — open nested?
            | Just 0x2D <- peekByte s (cPos c + 1)
                -> goBlockComment (d + 1) (bumpN 2 c)
        Just 0x2D                                     -- '-' — close?
            | Just 0x7D <- peekByte s (cPos c + 1)
                -> goBlockComment (d - 1) (bumpN 2 c)
        Just 0x0A -> goBlockComment d (newline c)
        Just _    -> goBlockComment d (bump c)

    bump (Cursor p l col) = Cursor (p + 1) l (col + 1)
    bumpN n c = iterate bump c !! n
    newline (Cursor p l _) = Cursor (p + 1) (l + 1) 1

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
            -- String/char literals first: quotes aren't operator chars.
            | b == 0x22        -> lexString c                          -- '"'
            | b == 0x27        -> lexChar   c                          -- '\''
            -- Structural punctuation (non-op chars): parens, brackets,
            -- braces, comma, semicolon, backtick, backslash.
            | b == 0x28, Just 0x23 <- peekByte s (cPos c + 1)
                               ->                                         -- '(#' unboxed-tuple open
                                  let c' = step (step c)
                                  in (mkTok TkLUnbox c c', c')
            | b == 0x28        -> (mkTok TkLParen   c (step c), step c)  -- '('
            | b == 0x29        -> (mkTok TkRParen   c (step c), step c)  -- ')'
            | b == 0x7B        -> (mkTok TkLBrace   c (step c), step c)  -- '{'
            | b == 0x7D        -> (mkTok TkRBrace   c (step c), step c)  -- '}'
            | b == 0x5B        -> (mkTok TkLBracket c (step c), step c)  -- '['
            | b == 0x5D        -> (mkTok TkRBracket c (step c), step c)  -- ']'
            | b == 0x2C        -> (mkTok TkComma    c (step c), step c)  -- ','
            | b == 0x3B        -> (mkTok TkSemi     c (step c), step c)  -- ';'
            | b == 0x60        -> (mkTok TkBacktick c (step c), step c)  -- '`'
            | b == 0x5C        -> (mkTok TkBackslash c (step c), step c) -- '\\'
            -- '@' is an operator-ish char in Haskell reports, but in practice
            -- it's used for as-patterns and type applications; treat as its
            -- own single-char token (no TkSymOp fall-through).
            | b == 0x40        -> (mkTok TkAt       c (step c), step c)  -- '@'
            -- Bar: matched as a single-char token unless it starts '||'.
            -- '||' is handled in lexSymOp as a recognised SymOp form, but
            -- our existing parser expects TkOr — so keep the explicit check.
            | b == 0x7C, Just 0x7C <- peekByte s (cPos c + 1)
                               -> let c' = step (step c) in (mkTok TkOr c c', c')
            | b == 0x7C, not (isOpChar (peekByte s (cPos c + 1)))
                               -> (mkTok TkBar c (step c), step c)       -- '|'
            -- '?' followed by a lowercase letter or '_' is an implicit
            -- parameter reference: ?name -> TkImplicitRef name.
            | b == 0x3F
            , Just b2 <- peekByte s (cPos c + 1)
            , isLowerStart b2
                -> lexImplicitRef c
            -- Everything else that starts with an operator char goes through
            -- the longest-match symbolic operator lexer. It returns known
            -- tokens for recognised shapes (=, ==, ->, :, ::, +, ++, ...)
            -- and TkSymOp for user-defined operators.
            | isOpChar (Just b) -> lexSymOp c
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

    -- | Integer literal. Recognises decimal, hex (@0x@/@0X@), octal
    -- (@0o@/@0O@). No exponent/float support yet.
    lexInt start = case peekByte s (cPos start) of
        Just 0x30                                 -- leading '0'?
            | Just p <- peekByte s (cPos start + 1)
            , p == 0x78 || p == 0x58              -- 'x' or 'X'
                -> lexHex start
            | Just p <- peekByte s (cPos start + 1)
            , p == 0x6F || p == 0x4F              -- 'o' or 'O'
                -> lexOct start
        _ -> lexDec start

    lexDec start = go (cPos start)
      where
        go p = case peekByte s p of
            Just b | isDigit b -> go (p + 1)
            _ ->
                -- Check for float: '.' followed by digit, or 'e'/'E'.
                case peekByte s p of
                    Just 0x2E                                -- '.'
                        | Just b2 <- peekByte s (p + 1)
                        , isDigit b2                        -- not '..' or '.x'
                        -> lexFloat start (p + 1)
                    Just e | e == 0x65 || e == 0x45         -- 'e' or 'E'
                        -> lexFloat start p
                    _ ->
                        let n   = read (BC.unpack (sliceBytes s (cPos start, p))) :: Integer
                            -- Consume trailing '#' or '##' for unboxed int literals
                            -- (e.g. 0#, 1##). Treat them as plain integers.
                            p'  = skipHashes p
                            end = Cursor p' (cLine start) (cCol start + (p' - cPos start))
                        in (mkTok (TkInt n) start end, end)
        skipHashes p = case peekByte s p of
            Just 0x23 -> skipHashes (p + 1)   -- '#'
            _         -> p

    -- | Lex the fractional+exponent part of a float literal.
    -- @start@ is the start of the whole literal; @p@ is positioned
    -- just after the integer digits (either at '.' or at 'e'/'E').
    lexFloat start p0 =
        let p1 = case peekByte s p0 of
                    Just 0x2E -> goDec (p0 + 1)     -- consume '.' then fraction digits
                    _         -> p0                  -- no '.' — start of exponent
            p2 = case peekByte s p1 of
                    Just e | e == 0x65 || e == 0x45  -- 'e' or 'E'
                        -> let p3 = case peekByte s (p1 + 1) of
                                        Just 0x2B -> p1 + 2   -- '+'
                                        Just 0x2D -> p1 + 2   -- '-'
                                        _         -> p1 + 1
                           in goDec p3
                    _ -> p1
            bs  = sliceBytes s (cPos start, p2)
            d   = read (BC.unpack bs) :: Double
            end = Cursor p2 (cLine start) (cCol start + (p2 - cPos start))
        in (mkTok (TkFloat d) start end, end)
      where
        goDec p = case peekByte s p of
            Just b | isDigit b -> goDec (p + 1)
            _                  -> p

    lexHex start = go (cPos start + 2) 0
      where
        go p !acc = case peekByte s p of
            Just b | isHexDigit b -> go (p + 1) (acc * 16 + fromIntegral (hexVal b))
            _ | p == cPos start + 2 ->
                    -- '0x' with no digits: fall back to decimal 0, leave 'x...'
                    -- to the next call. Should not happen in practice.
                    let end = Cursor (cPos start + 1) (cLine start) (cCol start + 1)
                    in (mkTok (TkInt 0) start end, end)
              | otherwise ->
                    let end = Cursor p (cLine start) (cCol start + (p - cPos start))
                    in (mkTok (TkInt acc) start end, end)

    lexOct start = go (cPos start + 2) 0
      where
        go p !acc = case peekByte s p of
            Just b | b >= 0x30 && b <= 0x37 ->
                go (p + 1) (acc * 8 + fromIntegral (b - 0x30))
            _ | p == cPos start + 2 ->
                    let end = Cursor (cPos start + 1) (cLine start) (cCol start + 1)
                    in (mkTok (TkInt 0) start end, end)
              | otherwise ->
                    let end = Cursor p (cLine start) (cCol start + (p - cPos start))
                    in (mkTok (TkInt acc) start end, end)

    lexIdent isCon start = go (cPos start)
      where
        go p = case peekByte s p of
            Just b | isIdentCont b -> go (p + 1)
            _ -> let bs  = sliceBytes s (cPos start, p)
                     -- If the identifier ends with '#' it's a MagicHash
                     -- primop name regardless of case (I#, runRW#, etc.).
                     k   | BC.isSuffixOf "#" bs = TkPrimId bs
                         | isCon               = TkConId bs
                         | otherwise           = keywordOr bs
                     end = Cursor p (cLine start) (cCol start + (p - cPos start))
                 in (mkTok k start end, end)

    -- | Implicit parameter reference: @?name@ → 'TkImplicitRef' name.
    -- The leading @?@ is consumed; the identifier part is lexed the same
    -- way as a normal lowercase identifier (letters, digits, @'@, @#@).
    lexImplicitRef start =
        let nameStart = cPos start + 1   -- skip the '?'
            nameEnd   = go nameStart
            bs        = sliceBytes s (nameStart, nameEnd)
            end       = Cursor nameEnd (cLine start) (cCol start + (nameEnd - cPos start))
        in (mkTok (TkImplicitRef bs) start end, end)
      where
        go p = case peekByte s p of
            Just b | isIdentCont b -> go (p + 1)
            _                      -> p

    -- | Longest-match symbolic operator. Scans the contiguous run of operator
    -- characters, then decides which token kind to emit. Known shapes map to
    -- dedicated tokens; everything else becomes @TkSymOp bs@.
    lexSymOp start = go (cPos start)
      where
        go p = case peekByte s p of
            Just b | isOpChar (Just b) -> go (p + 1)
            _ -> let bs  = sliceBytes s (cPos start, p)
                     k   = classifySymOp bs
                     end = Cursor p (cLine start) (cCol start + (p - cPos start))
                 in (mkTok k start end, end)

    -- Starts at the opening quote. Consume up to the matching @"@, handling
    -- a handful of backslash escapes. On malformed input (unterminated
    -- string, bad escape), we just stop at EOF / keep bytes as-is; the
    -- parser will surface a clearer error if needed.
    lexString openCur =
        let openP = cPos openCur + 1 in      -- past the opening quote
        let (bs, endP) = scanStr openP [] in
        let end = Cursor (endP + 1) (cLine openCur)
                         (cCol openCur + (endP + 1 - cPos openCur))
        in (mkTok (TkStr bs) openCur end, end)
      where
        scanStr p acc = case peekByte s p of
            Nothing   -> (BS.pack (reverse acc), p)          -- EOF; close loosely
            Just 0x22 -> (BS.pack (reverse acc), p)          -- closing quote
            Just 0x5C ->                                      -- backslash
                case peekByte s (p + 1) of
                    Just 0x6E -> scanStr (p + 2) (0x0A : acc)  -- \n
                    Just 0x74 -> scanStr (p + 2) (0x09 : acc)  -- \t
                    Just 0x22 -> scanStr (p + 2) (0x22 : acc)  -- \"
                    Just 0x5C -> scanStr (p + 2) (0x5C : acc)  -- \\
                    Just 0x30 -> scanStr (p + 2) (0x00 : acc)  -- \0
                    Just ch   -> scanStr (p + 2) (ch   : acc)  -- unknown: pass through
                    Nothing   -> (BS.pack (reverse acc), p + 1)
            Just b    -> scanStr (p + 1) (b : acc)

    -- Starts at the opening single-quote. One logical character, possibly
    -- escaped, followed by a closing single-quote. Malformed input degrades
    -- gracefully (we just emit whatever we scanned).
    lexChar openCur =
        let p0 = cPos openCur + 1 in
        let (ch, endP) = case peekByte s p0 of
                Just 0x5C -> case peekByte s (p0 + 1) of           -- backslash
                    Just 0x6E -> ('\n', p0 + 2)
                    Just 0x74 -> ('\t', p0 + 2)
                    Just 0x5C -> ('\\', p0 + 2)
                    Just 0x27 -> ('\'', p0 + 2)
                    Just 0x22 -> ('"',  p0 + 2)
                    Just 0x30 -> ('\0', p0 + 2)
                    Just cc   -> (chr (fromIntegral cc), p0 + 2)
                    Nothing   -> ('?',  p0 + 1)
                Just b    -> (chr (fromIntegral b), p0 + 1)
                Nothing   -> ('?', p0)
        in
        -- skip closing '\''
        let endP' = case peekByte s endP of
                Just 0x27 -> endP + 1
                _         -> endP
            end = Cursor endP' (cLine openCur)
                         (cCol openCur + (endP' - cPos openCur))
        in (mkTok (TkChar ch) openCur end, end)

    keywordOr bs = case bs of
        "if"        -> TkIf
        "then"      -> TkThen
        "else"      -> TkElse
        "do"        -> TkDo
        "let"       -> TkLet
        "in"        -> TkIn
        "where"     -> TkWhere
        "case"      -> TkCase
        "of"        -> TkOf
        "data"      -> TkData
        "module"    -> TkModule
        "import"    -> TkImport
        "qualified" -> TkQualified
        "as"        -> TkAs
        "hiding"    -> TkHiding
        "newtype"   -> TkNewtype
        "type"      -> TkTypeKw
        "class"     -> TkClass
        "instance"  -> TkInstance
        "deriving"  -> TkDeriving
        "infixl"    -> TkInfixL
        "infixr"    -> TkInfixR
        "infix"     -> TkInfix
        "_"         -> TkUnderscore
        _           -> TkIdent bs

-- | Map a lexed run of operator characters to a token kind. Known shapes
-- get dedicated tokens (preserves the old interface); unknown shapes become
-- 'TkSymOp' so the parser can decide what to do with user-defined operators
-- like @<>@, @>>=@, @<$>@, @.&.@, @:|@.
classifySymOp :: ByteString -> TokenKind
classifySymOp bs = case bs of
    "="  -> TkEq
    "=="  -> TkEqEq
    "=>" -> TkDArrow
    "+"  -> TkPlus
    "++" -> TkPlusPlus
    "-"  -> TkMinus
    "->" -> TkArrow
    "<-" -> TkLArrow
    "<"  -> TkLt
    "<=" -> TkLe
    ">"  -> TkGt
    ">=" -> TkGe
    "/=" -> TkNeq
    "&&" -> TkAnd
    "||" -> TkOr
    "*"  -> TkStar
    ":"  -> TkColon
    "::" -> TkDColon
    "."  -> TkDot
    ".." -> TkDotDot
    "!"  -> TkBang
    "$"  -> TkDollar
    _    -> TkSymOp bs

-- | Haskell operator characters (except the structural @(@/@)@ etc). Used to
-- delimit longest-match symbolic-operator tokens. Backtick and backslash are
-- excluded: they have dedicated single-char tokens (TkBacktick, TkBackslash).
isOpChar :: Maybe Word8 -> Bool
isOpChar (Just b) =
       b == 0x21   -- '!'
    || b == 0x23   -- '#'
    || b == 0x24   -- '$'
    || b == 0x25   -- '%'
    || b == 0x26   -- '&'
    || b == 0x2A   -- '*'
    || b == 0x2B   -- '+'
    || b == 0x2D   -- '-'
    || b == 0x2E   -- '.'
    || b == 0x2F   -- '/'
    || b == 0x3A   -- ':'
    || b == 0x3C   -- '<'
    || b == 0x3D   -- '='
    || b == 0x3E   -- '>'
    || b == 0x3F   -- '?'
    || b == 0x5E   -- '^'
    || b == 0x7C   -- '|'
    || b == 0x7E   -- '~'
isOpChar Nothing = False

isDigit :: Word8 -> Bool
isDigit b = b >= 0x30 && b <= 0x39

isHexDigit :: Word8 -> Bool
isHexDigit b = isDigit b
    || (b >= 0x41 && b <= 0x46)    -- 'A'..'F'
    || (b >= 0x61 && b <= 0x66)    -- 'a'..'f'

hexVal :: Word8 -> Int
hexVal b
    | b >= 0x30 && b <= 0x39 = fromIntegral (b - 0x30)
    | b >= 0x41 && b <= 0x46 = fromIntegral (b - 0x41) + 10
    | b >= 0x61 && b <= 0x66 = fromIntegral (b - 0x61) + 10
    | otherwise              = 0

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
    || b == 0x23                                           -- '#' (MagicHash: I#, runRW#, ByteArray#)

showHex :: Word8 -> String
showHex b = [hex (fromIntegral b `div` 16), hex (fromIntegral b `mod` 16)]
  where
    hex n | n < 10    = chr (0x30 + n)
          | otherwise = chr (0x61 + n - 10)
