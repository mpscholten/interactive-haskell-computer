-- | On-demand symbol finder. The core of demand-driven laziness:
-- given a target name, advance the lexer through top-level tokens
-- until we find @name [param] = ...@, returning the parameter list
-- plus body span. Bindings skimmed past are registered as 'SpanOnly'
-- so future lookups hit instantly.
--
-- Never parses a body. Never materializes a token list. Stops as soon
-- as the requested name is found.
--
-- Phase 1.3 LHS grammar:
--
-- @
-- top ::= ident ident? '=' rhs
-- rhs ::= <everything up to end-of-line>
-- @
module IHC.Scan
    ( KnownSymbols
    , emptyKnownSymbols
    , SymbolInfo(..)
    , BindingLhs(..)
    , findBinding
    , lookupSymbol
    , markCompiled
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef
import Foreign.Ptr (Ptr)

import IHC.Lexer
import IHC.Source

-- | What we know about a top-level name.
data SymbolInfo
    = SpanOnly !BindingLhs      -- ^ skimmed past; body location known
    | Compiled !(Ptr ())        -- ^ already JITted; entry pointer
    deriving (Eq, Show)

data BindingLhs = BindingLhs
    { lhsParams :: ![ByteString] -- ^ zero or one in Phase 1.3
    , lhsBody   :: !Span
    } deriving (Eq, Show)

type KnownSymbols = IORef (Map ByteString SymbolInfo, Cursor)

emptyKnownSymbols :: IO KnownSymbols
emptyKnownSymbols = newIORef (Map.empty, startCursor)

lookupSymbol :: KnownSymbols -> ByteString -> IO (Maybe SymbolInfo)
lookupSymbol ref name = do
    (m, _) <- readIORef ref
    pure (Map.lookup name m)

markCompiled :: KnownSymbols -> ByteString -> Ptr () -> IO ()
markCompiled ref name ptr =
    modifyIORef' ref (\(m, c) -> (Map.insert name (Compiled ptr) m, c))

-- | Advance looking for a top-level binding named @target@. Returns
-- the binding's LHS (param list + body span) if found.
findBinding :: Source -> KnownSymbols -> ByteString -> IO (Maybe BindingLhs)
findBinding src ref target = do
    existing <- lookupSymbol ref target
    case existing of
        Just (SpanOnly lhs) -> pure (Just lhs)
        Just (Compiled _)   -> pure Nothing
        Nothing -> do
            (m0, c0) <- readIORef ref
            go m0 c0
  where
    go acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> do
                writeIORef ref (acc, cur')
                pure Nothing
            TkNewline -> go acc cur'
            TkIdent name
                | tkCol tok == 1 -> handleTopIdent acc name tok cur'
                | otherwise      -> go acc cur'
            _ -> go acc cur'

    handleTopIdent acc name startTok cur = do
        -- Read zero or more parameter idents, then expect '='.
        (params, curAfter) <- collectParams [] cur
        let (t, curN) = nextToken src curAfter
        case tkKind t of
            TkEq -> finish acc name params curN startTok
            _    -> skipBadBinding acc startTok cur

    collectParams :: [ByteString] -> Cursor -> IO ([ByteString], Cursor)
    collectParams acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkIdent p -> collectParams (p : acc) cur'
            _         -> pure (reverse acc, cur)

    finish acc name params cur startTok = do
        let bodyStart = cPos (skipTrivia src cur)
            bodyEnd   = findBodyEnd src bodyStart
            lhs       = BindingLhs params (bodyStart, bodyEnd)
            acc'      = Map.insert name (SpanOnly lhs) acc
            curAfter  = Cursor bodyEnd (tkLine startTok) 1
        if name == target
            then do
                writeIORef ref (acc', curAfter)
                pure (Just lhs)
            else go acc' curAfter

    skipBadBinding acc startTok cur =
        let eolPos = findLineEnd src (cPos cur)
            cur'   = Cursor eolPos (tkLine startTok) 1
        in go acc cur'

-- | Scan until the end of the current line (or EOF). Used only for
-- recovering after a malformed LHS; the real body-extent logic is
-- 'findBodyEnd'.
findLineEnd :: Source -> Pos -> Pos
findLineEnd s p = case peekByte s p of
    Nothing   -> p
    Just 0x0A -> p
    Just 0x0D -> p
    Just _    -> findLineEnd s (p + 1)

-- | A body extends until either EOF or the next column-1 non-whitespace
-- byte (the start of the next top-level binding). Indented continuation
-- lines and blank lines are part of the body.
findBodyEnd :: Source -> Pos -> Pos
findBodyEnd s = scanBody
  where
    -- Keep scanning bytes; at each newline, decide whether the body
    -- continues (next line is indented or blank) or ends.
    scanBody p = case peekByte s p of
        Nothing   -> p
        Just 0x0A -> checkNext (p + 1) p
        Just 0x0D -> checkNext (p + 1) p
        Just _    -> scanBody (p + 1)

    checkNext q lastNl = case peekByte s q of
        Nothing   -> lastNl        -- EOF terminates body.
        Just 0x20 -> scanBody q    -- space: indented continuation, keep going.
        Just 0x09 -> scanBody q    -- tab: same.
        Just 0x0A -> checkNext (q + 1) q   -- blank line, check after.
        Just 0x0D -> checkNext (q + 1) q
        Just _    -> lastNl        -- col-1 content: body ends at last newline.
