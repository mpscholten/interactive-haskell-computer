-- | HSX / QuasiQuoter diagnostic probe.
--
-- Given a @.hs@ file on argv, this program:
--
--   1. Reads the file via 'IHC.Source.readSourceFile'.
--   2. Streams the lexer and reports every QuasiQuoter- / TH-bracket-style
--      token it sees (kind + line\/col, plus the QQ name for @[foo|@).
--   3. Scans top-level bindings and parses each one, walking the resulting
--      'Expr' tree looking for AST nodes that came from a QQ — currently
--      @EApp (EVar "error") (stringToConsList "unexpanded QuasiQuoter …")@
--      placeholders, plus any 'ESplice' / 'EQuote' TH brackets.
--
-- Exits 0 on success, even when no QQ tokens/nodes are present (an empty
-- report is a valid result).
module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import IHC.AST
    ( Expr(..)
    , Stmt(..)
    , Alt(..)
    , Lit(..)
    )
import IHC.Lexer
    ( Cursor
    , Token(..)
    , TokenKind(..)
    , nextToken
    , startCursor
    )
import IHC.Parser
    ( FixityTable
    , defaultFixityTable
    , parseBodyExprWithFixity
    , scanFixityDecls
    )
import IHC.Scan
    ( KnownSymbols
    , emptyKnownSymbols
    , findBinding
    , lhsClauses
    , scanAllTopLevelNames
    )
import IHC.Source (Source, readSourceFile)

--------------------------------------------------------------------------------
-- Lexer pass: dump every QQ / TH bracket token
--------------------------------------------------------------------------------

-- | Tokens we consider "QQ / TH-bracket related" for this probe.
isQQToken :: TokenKind -> Bool
isQQToken = \case
    TkQQOpen{}     -> True
    TkOQuote       -> True
    TkOQuoteD      -> True
    TkOQuoteT      -> True
    TkOQuoteP      -> True
    TkOQuoteTy     -> True
    TkCQuote       -> True
    TkCQuoteTy     -> True
    TkSpliceLParen -> True
    _              -> False

-- | Short label for a QQ/TH token kind, suitable for one-line output.
qqLabel :: TokenKind -> String
qqLabel = \case
    TkQQOpen n     -> "TkQQOpen " ++ show (BC.unpack n)
    TkOQuote       -> "TkOQuote"
    TkOQuoteD      -> "TkOQuoteD"
    TkOQuoteT      -> "TkOQuoteT"
    TkOQuoteP      -> "TkOQuoteP"
    TkOQuoteTy     -> "TkOQuoteTy"
    TkCQuote       -> "TkCQuote"
    TkCQuoteTy     -> "TkCQuoteTy"
    TkSpliceLParen -> "TkSpliceLParen"
    other          -> show other

dumpQQTokens :: Source -> IO ()
dumpQQTokens src = go startCursor
  where
    go :: Cursor -> IO ()
    go cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure ()
            k     -> do
                if isQQToken k
                    then putStrLn $
                        "[lexer] " <> qqLabel k
                                 <> " at line " <> show (tkLine tok)
                                 <> ", col " <> show (tkCol tok)
                    else pure ()
                go cur'

--------------------------------------------------------------------------------
-- Parser pass: walk the AST for QQ placeholder nodes
--------------------------------------------------------------------------------

-- | If an 'Expr' is the current "unexpanded QQ" placeholder
-- (@EApp (EVar "error") (stringToConsList "unexpanded QuasiQuoter …")@),
-- extract the message. Returns 'Nothing' otherwise.
qqPlaceholderMsg :: Expr -> Maybe String
qqPlaceholderMsg (EApp (EVar "error") arg)
    | Just s <- consListToString arg
    , "unexpanded QuasiQuoter" `isPrefixOf` s
    = Just s
qqPlaceholderMsg _ = Nothing

-- | Match the exact shape produced by 'stringToConsList' in IHC.Parser:
-- @(:) 'c' ((:) 'h' ... (:) 'x' [])@.  Anything else gives 'Nothing'.
consListToString :: Expr -> Maybe String
consListToString (EVar "[]") = Just ""
consListToString (EApp (EApp (EVar ":") (ELit (LChar c))) rest) =
    (c :) <$> consListToString rest
consListToString _ = Nothing

-- | Walk every 'Expr' subtree (including inside Stmt / Alt / Bind bodies).
exprSubtrees :: Expr -> [Expr]
exprSubtrees e = e : case e of
    EVar{}                    -> []
    ELit{}                    -> []
    EApp f x                  -> exprSubtrees f ++ exprSubtrees x
    ELam _ b                  -> exprSubtrees b
    ELet bs b                 -> concatMap (exprSubtrees . snd) bs ++ exprSubtrees b
    ECase scr alts            -> exprSubtrees scr
                                 ++ concatMap (\(Alt _ body) -> exprSubtrees body) alts
    EIf c t f                 -> exprSubtrees c ++ exprSubtrees t ++ exprSubtrees f
    EDo ss                    -> concatMap stmtSubtrees ss
    ENeg x                    -> exprSubtrees x
    ETuple xs                 -> concatMap exprSubtrees xs
    EImplicitRef{}            -> []
    EImplicitLet bs b         -> concatMap (exprSubtrees . snd) bs ++ exprSubtrees b
    ERecordCon _ fs           -> concatMap (exprSubtrees . snd) fs
    ERecordWild{}             -> []
    ERecordUpdate x fs        -> exprSubtrees x ++ concatMap (exprSubtrees . snd) fs
    ESplice x                 -> exprSubtrees x
    EQuote x                  -> exprSubtrees x
    EQuasiQuote{}             -> []
    ELabel{}                  -> []
    ETyApp x _                -> exprSubtrees x
    ETypedMethod{}            -> []
    EGuardFail                -> []
  where
    stmtSubtrees (SExpr x)          = exprSubtrees x
    stmtSubtrees (SBind _ x)        = exprSubtrees x
    stmtSubtrees (SBangBind _ x)    = exprSubtrees x
    stmtSubtrees (SLet bs)          = concatMap (exprSubtrees . snd) bs
    stmtSubtrees (SImplicitLet bs)  = concatMap (exprSubtrees . snd) bs

-- | Report QQ placeholder nodes (and 'ESplice' / 'EQuote' nodes for good
-- measure — those are TH brackets, which the lexer side already covers
-- but we mirror on the parser side so the two reports align).
dumpQQExprNodes :: Source -> IO ()
dumpQQExprNodes src = do
    names <- scanAllTopLevelNames src
    fx    <- scanFixityDecls src defaultFixityTable
    ks    <- emptyKnownSymbols
    mapM_ (reportBinding src fx ks) names

reportBinding
    :: Source
    -> FixityTable
    -> KnownSymbols
    -> BC.ByteString
    -> IO ()
reportBinding src fx ks name = do
    mLhs <- findBinding src ks name
    case mLhs of
        Nothing  -> pure ()
        Just lhs -> do
            eExpr <- try (parseBodyExprWithFixity src fx lhs)
                        :: IO (Either SomeException Expr)
            case eExpr of
                Left ex -> hPutStrLn stderr $
                    "  (skip " <> BC.unpack name <> ": parse failed — "
                               <> show ex <> ")"
                Right expr ->
                    mapM_ (reportNode name) (exprSubtrees expr)

reportNode :: BC.ByteString -> Expr -> IO ()
reportNode name e = case qqPlaceholderMsg e of
    Just msg ->
        putStrLn $ "[parser] binding=" <> BC.unpack name
                <> " EApp (EVar \"error\") (stringToConsList "
                <> show msg <> ")"
    Nothing -> case e of
        ESplice _ -> putStrLn $
            "[parser] binding=" <> BC.unpack name <> " ESplice"
        EQuote _  -> putStrLn $
            "[parser] binding=" <> BC.unpack name <> " EQuote"
        _         -> pure ()

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            src <- readSourceFile path
            putStrLn $ "== HsxProbe: " <> path <> " =="
            dumpQQTokens src
            dumpQQExprNodes src
        _ -> do
            hPutStrLn stderr "usage: probe-hsx <path-to-hs-file>"
            exitFailure
