-- | On-demand symbol finder. This is the core of the plan's
-- "demand-driven laziness": given a target binding name, advance the
-- lexer through top-level tokens until we find @name = …@, returning
-- the byte span of the body. Bindings we skim past are registered as
-- 'SpanOnly' so future lookups find them instantly.
--
-- Never parses a body. Never materializes a token list. Stops scanning
-- as soon as the requested name is found.
--
-- Limitations for Phase 1.0 (adequate to run @main = 42@):
--   * assumes each binding occupies a single physical line
--     (no where-clauses, no multi-line RHS)
--   * recognises only @name = rhs@; no signatures, no patterns
--   * layout handling is trivially "top-level token at column 1"
module IHC.Scan
    ( KnownSymbols
    , emptyKnownSymbols
    , SymbolInfo(..)
    , findBinding
    , lookupSymbol
    ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef

import IHC.Lexer
import IHC.Source

-- | What we know about a top-level name.
data SymbolInfo
    = SpanOnly !Span           -- ^ we've skimmed past it; body lives in this span
    | Compiled !Int            -- ^ already JITted; this is the code-pointer addr
    deriving (Eq, Show)

-- | Shared mutable table. IORef for Phase 1.0; a concurrent map in Phase 6.
type KnownSymbols = IORef (Map ByteString SymbolInfo, Cursor)

emptyKnownSymbols :: IO KnownSymbols
emptyKnownSymbols = newIORef (Map.empty, startCursor)

lookupSymbol :: KnownSymbols -> ByteString -> IO (Maybe SymbolInfo)
lookupSymbol ref name = do
    (m, _) <- readIORef ref
    pure (Map.lookup name m)

-- | Advance the lexer looking for a top-level binding named @target@.
-- Every other top-level binding we pass along the way is registered as
-- 'SpanOnly'. Returns 'Just' with the body span once found.
--
-- Resumes from the saved cursor in 'KnownSymbols' so repeated calls
-- don't re-scan the prefix of the file.
findBinding :: Source -> KnownSymbols -> ByteString -> IO (Maybe Span)
findBinding src ref target = do
    -- Fast path: already registered.
    existing <- lookupSymbol ref target
    case existing of
        Just (SpanOnly s) -> pure (Just s)
        Just (Compiled _) -> pure Nothing  -- (phase 1.0: shouldn't happen for target)
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
                | otherwise      -> go acc cur'  -- indented — not a binding start
            _ -> go acc cur'

    handleTopIdent acc name tok cur = do
        -- Expect: `=` follows, then the RHS extends to end-of-line.
        let (eqTok, cur1) = nextToken src cur
        case tkKind eqTok of
            TkEq -> do
                -- Body starts after '=' (skipping whitespace) and ends at EOL.
                let bodyStart = cPos (skipTrivia src cur1)
                    bodyEnd   = findLineEnd src bodyStart
                    bodySpan  = (bodyStart, bodyEnd)
                    acc'      = Map.insert name (SpanOnly bodySpan) acc
                    -- Move cursor to the newline after the body.
                    cur2      = Cursor bodyEnd (tkLine tok) 1
                if name == target
                    then do
                        writeIORef ref (acc', cur2)
                        pure (Just bodySpan)
                    else go acc' cur2
            _ ->
                -- Unexpected; skip this binding by eating to EOL and continuing.
                let eolPos = findLineEnd src (tkEnd tok)
                    cur2   = Cursor eolPos (tkLine tok) 1
                in go acc cur2

findLineEnd :: Source -> Pos -> Pos
findLineEnd s p = case peekByte s p of
    Nothing   -> p
    Just 0x0A -> p
    Just 0x0D -> p
    Just _    -> findLineEnd s (p + 1)
