-- | Dependency-light parser for type schemes shared by scanning and elaboration.
module IHC.TypeSchemeParser
    ( TTok(..), tokenKindToTT, parseScheme, parseType, parseTypeAtoms
    , parseTypeKinds, parseSchemeBytes ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import IHC.AST (Name)
import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Source (mkSource)
import IHC.TypeAST (Type(..), Pred(..), Scheme(..))

data TTok = TTCon !ByteString | TTVar !ByteString | TTArrow | TTDArrow
    | TTLParen | TTRParen | TTLBracket | TTRBracket | TTComma | TTDot
    | TTForall | TTUnderscore | TTSymOp !ByteString
    deriving (Eq, Show)

tokenKindToTT :: TokenKind -> Maybe TTok
tokenKindToTT kind = case kind of
    TkConId n -> Just (TTCon n); TkIdent "forall" -> Just TTForall
    TkIdent n -> Just (TTVar n); TkArrow -> Just TTArrow; TkDArrow -> Just TTDArrow
    TkLParen -> Just TTLParen; TkRParen -> Just TTRParen
    TkLBracket -> Just TTLBracket; TkRBracket -> Just TTRBracket
    TkComma -> Just TTComma; TkDot -> Just TTDot; TkUnderscore -> Just TTUnderscore
    TkSymOp n -> Just (TTSymOp n); _ -> Nothing

parseSchemeBytes :: ByteString -> Maybe Scheme
parseSchemeBytes bytes = go startCursor >>= parseScheme
  where
    src = mkSource "<type-scheme>" bytes
    go cur = case nextToken src cur of
        (Token { tkKind = TkEof }, _) -> Just []
        (Token { tkKind = TkNewline }, cur') -> go cur'
        (tok, cur') -> (:) <$> tokenKindToTT (tkKind tok) <*> go cur'

parseTypeKinds :: [TokenKind] -> Maybe Type
parseTypeKinds kinds = traverse tokenKindToTT kinds >>= parseType

parseScheme :: [TTok] -> Maybe Scheme
parseScheme toks0 = do
    let (vars, toks1) = consumeOptionalForall toks0
        (preds, toks2) = consumeOptionalContext toks1
    body <- parseType toks2
    pure (Scheme (if null vars then collectTypeVars body preds else vars) preds body)

consumeOptionalForall :: [TTok] -> ([Name], [TTok])
consumeOptionalForall (TTForall : rest) = takeForallVars [] rest
consumeOptionalForall toks = ([], toks)

takeForallVars :: [Name] -> [TTok] -> ([Name], [TTok])
takeForallVars acc (TTVar n : rest) = takeForallVars (n : acc) rest
takeForallVars acc (TTDot : rest) = (reverse acc, rest)
takeForallVars acc rest = (reverse acc, rest)

consumeOptionalContext :: [TTok] -> ([Pred], [TTok])
consumeOptionalContext toks = case splitDArrowDepth0 toks of
    Nothing -> ([], toks)
    Just (ctx, rest) -> (parseContext ctx, rest)

parseContext :: [TTok] -> [Pred]
parseContext [] = []
parseContext toks@(TTLParen : _) = parseParenContext toks
parseContext toks = maybe [] (:[]) (parsePred toks)

parseParenContext :: [TTok] -> [Pred]
parseParenContext (TTLParen : inner) =
    let (body, _) = splitRParenDepth0 inner
    in [p | seg <- splitCommaDepth0 body, Just p <- [parsePred seg]]
parseParenContext _ = []

parsePred :: [TTok] -> Maybe Pred
parsePred (TTCon cls : rest) = do
    args <- parseTypeAtoms rest
    if null args then Nothing else Just (Pred cls args)
parsePred _ = Nothing

parseType :: [TTok] -> Maybe Type
parseType (TTForall : rest) = do
    let (vs, afterDot) = takeForallVars [] rest
        (preds, afterCtx) = consumeOptionalContext afterDot
    TyForall vs preds <$> parseType afterCtx
parseType toks = case splitArrowDepth0 toks of
    Just (lhs, rhs) -> TyArrow <$> parseTypeApp lhs <*> parseType rhs
    Nothing -> parseTypeApp toks

parseTypeApp :: [TTok] -> Maybe Type
parseTypeApp toks = do
    atoms <- parseTypeAtoms toks
    case atoms of [] -> Nothing; a : as -> Just (foldl TyApp a as)

parseTypeAtoms :: [TTok] -> Maybe [Type]
parseTypeAtoms [] = Just []
parseTypeAtoms toks = do
    (atom, rest) <- parseAtom toks
    (atom :) <$> parseTypeAtoms rest

parseAtom :: [TTok] -> Maybe (Type, [TTok])
parseAtom toks = case toks of
    TTCon q : TTDot : TTCon n : rest -> qualifiedTyCon q n rest
    TTVar q : TTDot : TTCon n : rest -> qualifiedTyCon q n rest
    TTCon n : rest -> Just (TyCon n, rest)
    TTVar n : rest -> Just (TyVar n, rest)
    TTUnderscore : rest -> Just (TyVar "_", rest)
    TTLParen : inner -> parseParenAtom inner
    TTLBracket : inner -> parseListAtom inner
    _ -> Nothing
  where
    qualifiedTyCon prefix segment rest =
        let name = prefix <> "." <> segment in case rest of
            TTDot : TTCon next : more -> qualifiedTyCon name next more
            _ -> Just (TyCon name, rest)

parseParenAtom :: [TTok] -> Maybe (Type, [TTok])
parseParenAtom (TTRParen : rest) = Just (TyCon "()", rest)
parseParenAtom inner = do
    let (body, afterRP) = splitRParenDepth0 inner; segments = splitCommaDepth0 body
    case segments of
        [seg] -> (, afterRP) <$> parseType seg
        many -> do
            ts <- mapM parseType many
            let tupleCon = TyCon (BC.pack ('(' : replicate (length ts - 1) ',' ++ ")"))
            pure (foldl TyApp tupleCon ts, afterRP)

parseListAtom :: [TTok] -> Maybe (Type, [TTok])
parseListAtom inner = do
    let (body, afterRB) = splitRBracketDepth0 inner
    t <- parseType body
    pure (TyApp (TyCon "[]") t, afterRB)

splitArrowDepth0, splitDArrowDepth0 :: [TTok] -> Maybe ([TTok], [TTok])
splitArrowDepth0 = splitAtDepth TTArrow
splitDArrowDepth0 = splitAtDepth TTDArrow

splitAtDepth :: TTok -> [TTok] -> Maybe ([TTok], [TTok])
splitAtDepth needle = go [] 0 where
    go _ _ [] = Nothing
    go acc 0 (t : rest) | t == needle = Just (reverse acc, rest)
    go acc d (t : rest) = go (t : acc) (depthDelta t d) rest

depthDelta :: TTok -> Int -> Int
depthDelta t d = case t of
    TTLParen -> d + 1; TTLBracket -> d + 1
    TTRParen -> d - 1; TTRBracket -> d - 1; _ -> d

splitRParenDepth0, splitRBracketDepth0 :: [TTok] -> ([TTok], [TTok])
splitRParenDepth0 = splitClosing TTRParen TTLParen
splitRBracketDepth0 = splitClosing TTRBracket TTLBracket

splitClosing :: TTok -> TTok -> [TTok] -> ([TTok], [TTok])
splitClosing close open = go [] 1 where
    go :: [TTok] -> Int -> [TTok] -> ([TTok], [TTok])
    go acc _ [] = (reverse acc, [])
    go acc 1 (t : rest) | t == close = (reverse acc, rest)
    go acc d (t : rest)
        | t == open = go (t : acc) (d + 1) rest
        | t == close = go (t : acc) (d - 1) rest
        | otherwise = go (t : acc) d rest

splitCommaDepth0 :: [TTok] -> [[TTok]]
splitCommaDepth0 = go [] 0 [] where
    go cur _ acc [] = reverse (reverse cur : acc)
    go cur 0 acc (TTComma : rest) = go [] 0 (reverse cur : acc) rest
    go cur d acc (t : rest) = go (t : cur) (depthDelta t d) acc rest

collectTypeVars :: Type -> [Pred] -> [Name]
collectTypeVars body preds = uniq (collect body ++ concatMap collectPred preds)
  where
    collect t = case t of
        TyVar n -> [n]; TyCon _ -> []; TyApp a b -> collect a ++ collect b
        TyArrow a b -> collect a ++ collect b; TyForall _ _ _ -> []
    collectPred (Pred _ as) = concatMap collect as
    collectPred (QPred vs ctx pbody) =
        filter (`notElem` vs) (concatMap collectPred ctx ++ collectPred pbody)
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
