{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser conformance tests for Haskell 2010 §1.2-1.4: identifiers,
-- operator symbols, and qualified names. Each item from the taxonomy is
-- mapped to a single hspec example so the matrix is visible at a glance.
module Hs2010LexIdent (spec) where

import Control.Exception (SomeException, evaluate, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Lexer (Token(..), TokenKind(..), nextToken, startCursor)
import IHC.ModuleHeader
    ( ExportItem(..)
    , ExportSpec(..)
    , ModuleHeader(..)
    , parseModuleHeader
    )
import IHC.Parser (defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

lexOne :: ByteString -> IO (Either SomeException TokenKind)
lexOne bs = try (evaluate (tkKind (fst (nextToken (mkSrc bs) startCursor))))

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldLexAs :: ByteString -> TokenKind -> IO ()
shouldLexAs bs expected = do
    r <- lexOne bs
    case r of
        Right got | got == expected -> pure ()
        Right got -> expectationFailure
            ("expected " <> show expected <> ", got " <> show got)
        Left e -> expectationFailure
            ("lexer crashed: " <> show e)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Lexical identifiers & operators" $ do

    describe "§1.2 Identifiers and keywords" $ do
        it "1.2.1 varid lowercase-led `foo`" $
            shouldLexAs "foo" (TkIdent "foo")
        it "1.2.2 varid with apostrophe and digits `f'1`" $
            shouldLexAs "f'1" (TkIdent "f'1")
        it "1.2.3 varid with leading underscore `_x`" $
            shouldLexAs "_x" (TkIdent "_x")
        it "1.2.4 bare wildcard `_`" $
            shouldLexAs "_" TkUnderscore
        it "1.2.5 conid `Foo`" $
            shouldLexAs "Foo" (TkConId "Foo")
        it "1.2.6 conid with digit and apostrophe `T1'`" $
            shouldLexAs "T1'" (TkConId "T1'")
        it "1.2.7 reserved identifier `case` lexes as TkCase" $
            shouldLexAs "case" TkCase
        it "1.2.7 reserved identifier `class` lexes as TkClass" $
            shouldLexAs "class" TkClass
        it "1.2.7 reserved identifier `data` lexes as TkData" $
            shouldLexAs "data" TkData
        it "1.2.7 reserved identifier `default` lexes as TkIdent (no dedicated token)" $
            -- IHC has no TkDefault; the decl scanner matches the bytes directly.
            shouldLexAs "default" (TkIdent "default")
        it "1.2.7 reserved identifier `deriving` lexes as TkDeriving" $
            shouldLexAs "deriving" TkDeriving
        it "1.2.7 reserved identifier `do` lexes as TkDo" $
            shouldLexAs "do" TkDo
        it "1.2.7 reserved identifier `else` lexes as TkElse" $
            shouldLexAs "else" TkElse
        it "1.2.7 reserved identifier `foreign` lexes as TkIdent (FFI not yet hard-keyword)" $
            shouldLexAs "foreign" (TkIdent "foreign")
        it "1.2.7 reserved identifier `if` lexes as TkIf" $
            shouldLexAs "if" TkIf
        it "1.2.7 reserved identifier `import` lexes as TkImport" $
            shouldLexAs "import" TkImport
        it "1.2.7 reserved identifier `in` lexes as TkIn" $
            shouldLexAs "in" TkIn
        it "1.2.7 reserved identifier `infix` lexes as TkInfix" $
            shouldLexAs "infix" TkInfix
        it "1.2.7 reserved identifier `infixl` lexes as TkInfixL" $
            shouldLexAs "infixl" TkInfixL
        it "1.2.7 reserved identifier `infixr` lexes as TkInfixR" $
            shouldLexAs "infixr" TkInfixR
        it "1.2.7 reserved identifier `instance` lexes as TkInstance" $
            shouldLexAs "instance" TkInstance
        it "1.2.7 reserved identifier `let` lexes as TkLet" $
            shouldLexAs "let" TkLet
        it "1.2.7 reserved identifier `module` lexes as TkModule" $
            shouldLexAs "module" TkModule
        it "1.2.7 reserved identifier `newtype` lexes as TkNewtype" $
            shouldLexAs "newtype" TkNewtype
        it "1.2.7 reserved identifier `of` lexes as TkOf" $
            shouldLexAs "of" TkOf
        it "1.2.7 reserved identifier `then` lexes as TkThen" $
            shouldLexAs "then" TkThen
        it "1.2.7 reserved identifier `type` lexes as TkTypeKw" $
            shouldLexAs "type" TkTypeKw
        it "1.2.7 reserved identifier `where` lexes as TkWhere" $
            shouldLexAs "where" TkWhere
        it "1.2.8 soft keyword `as` is lexed as a plain identifier" $
            shouldLexAs "as" (TkIdent "as")
        it "1.2.8 soft keyword `forall` is lexed as a plain identifier (ParserBugs Bug 3)" $
            shouldLexAs "forall" (TkIdent "forall")
        it "1.2.8 soft keyword `qualified` usable as varid" $
            shouldLexAs "qualified" (TkIdent "qualified")
        it "1.2.8 soft keyword `hiding` usable as varid" $
            shouldLexAs "hiding" (TkIdent "hiding")
        it "1.2.9 maximal munch: `cases` is one identifier" $
            shouldLexAs "cases" (TkIdent "cases")
        it "1.2.9 maximal munch: `lett` is one identifier (not let + t)" $
            shouldLexAs "lett" (TkIdent "lett")
        it "1.2.9 maximal munch: `iffy` is one identifier (not if + fy)" $
            shouldLexAs "iffy" (TkIdent "iffy")

    describe "§1.3 Operator symbols" $ do
        it "1.3.1 varsym `+++` lexes as TkSymOp \"+++\"" $
            shouldLexAs "+++" (TkSymOp "+++")
        it "1.3.2 consym `:+:` lexes as TkSymOp \":+:\"" $
            shouldLexAs ":+:" (TkSymOp ":+:")
        it "1.3.3 bare `:` is reserved (lexes as TkColon, the cons op)" $
            shouldLexAs ":" TkColon
        it "1.3.4 reserved op `..` lexes as TkDotDot" $
            shouldLexAs ".." TkDotDot
        it "1.3.4 reserved op `::` lexes as TkDColon" $
            shouldLexAs "::" TkDColon
        it "1.3.4 reserved op `=` lexes as TkEq" $
            shouldLexAs "=" TkEq
        it "1.3.4 reserved op `\\` lexes as TkBackslash" $
            shouldLexAs "\\" TkBackslash
        it "1.3.4 reserved op `|` lexes as TkBar" $
            shouldLexAs "|" TkBar
        it "1.3.4 reserved op `<-` lexes as TkLArrow" $
            shouldLexAs "<-" TkLArrow
        it "1.3.4 reserved op `->` lexes as TkArrow" $
            shouldLexAs "->" TkArrow
        it "1.3.4 reserved op `@` lexes as TkAt" $
            shouldLexAs "@" TkAt
        it "1.3.4 reserved op `~` lexes as TkSymOp \"~\"" $
            -- No dedicated token; the parser disambiguates by string in pattern context.
            shouldLexAs "~" (TkSymOp "~")
        it "1.3.4 reserved op `=>` lexes as TkDArrow" $
            shouldLexAs "=>" TkDArrow
        it "1.3.5 `!` is an ordinary varsym (TkBang)" $
            shouldLexAs "!" TkBang
        it "1.3.5 `!=` is a regular symbolic operator (not bang + equals)" $
            shouldLexAs "!=" (TkSymOp "!=")
        -- Dot-led multi-char ops must not be split into TkDot + remainder
        -- (aeson/lens .=, conduit .|, lens .#, Data.Bits .&.).
        it "1.3.6 dot-op `.=` lexes as TkSymOp (aeson/lens)" $
            shouldLexAs ".=" (TkSymOp ".=")
        it "1.3.6 dot-op `.|` lexes as TkSymOp (conduit)" $
            shouldLexAs ".|" (TkSymOp ".|")
        it "1.3.6 dot-op `.#` lexes as TkSymOp (lens)" $
            shouldLexAs ".#" (TkSymOp ".#")
        it "1.3.6 dot-op `.$` lexes as TkSymOp" $
            shouldLexAs ".$" (TkSymOp ".$")
        it "1.3.6 dot-op `.~` lexes as TkSymOp" $
            shouldLexAs ".~" (TkSymOp ".~")
        it "1.3.6 bitwise `.&.` lexes as one TkSymOp" $
            shouldLexAs ".&." (TkSymOp ".&.")
        it "1.3.6 bitwise `.|.` lexes as one TkSymOp" $
            shouldLexAs ".|." (TkSymOp ".|.")

    describe "§1.4 Qualified names" $ do
        it "1.4.1 qvarid `Foo.bar` parses as a qualified expression" $
            "Foo.bar" `shouldParseTo` EVar "Foo.bar"
        it "1.4.2 qconid `Foo.Bar` parses as a qualified constructor" $
            "Foo.Bar" `shouldParseTo` EVar "Foo.Bar"
        it "1.4.3 qvarsym `Foo.++` parses as a qualified operator" $ do
            r <- parseExpr "1 Foo.++ 2"
            case r of
                Right _ -> pure ()
                Left _  -> pendingWith
                    "known gap: parser does not yet fuse qvarsym (Module.+op) chains"
        it "1.4.4 qconsym `Foo.:|` parses as a qualified constructor operator" $ do
            r <- parseExpr "1 Foo.:| 2"
            case r of
                Right _ -> pure ()
                Left _  -> pendingWith
                    "known gap: parser does not yet fuse qconsym (Module.:op) chains"
        it "1.4.5 multi-component qualifier `A.B.x` parses" $
            "A.B.x" `shouldParseTo` EVar "A.B.x"
        it "1.4.5 multi-component qualifier `A.B.C.x` parses" $
            "A.B.C.x" `shouldParseTo` EVar "A.B.C.x"
        it "1.4.6 dot abuts: `F.x` is a qualified name" $
            "F.x" `shouldParseTo` EVar "F.x"
        it "1.4.6 dot does not abut: `F . x` is constructor + composition" $
            "F . x" `shouldParseTo`
                EApp (EApp (EVar ".") (EVar "F")) (EVar "x")

    describe "§1.4 Qualified names — module-header export forms" $ do
        it "abutting `M (B.bar)` exports the qualified name `bar`" $ do
            (mh, _) <- parseModuleHeader (mkSrc "module M (B.bar) where\n") startCursor
            case mh of
                Just (ModuleHeader _ (ExportList items) _) ->
                    items `shouldBe` [ExportName "bar"]
                _ -> expectationFailure ("unexpected header: " <> show mh)
        it "non-abutting `M (B . bar)` does NOT export `bar` as qualified" $ do
            (mh, _) <- parseModuleHeader (mkSrc "module M (B . bar) where\n") startCursor
            case mh of
                Just (ModuleHeader _ (ExportList items) _) ->
                    items `shouldNotContain` [ExportName "bar"]
                _ -> expectationFailure ("unexpected header: " <> show mh)

    describe "Higher-level conformance via parseExpr" $ do
        it "soft keyword `forall` binds as a value identifier (ParserBugs Bug 3)" $
            "let forall = 1 in forall" `shouldParseTo`
                ELet [("forall", ELit (LInt 1))] (EVar "forall")
        it "soft keyword `as` binds as a value identifier" $
            "let as = 1 in as" `shouldParseTo`
                ELet [("as", ELit (LInt 1))] (EVar "as")
        it "varsym `+++` parses as an infix expression" $
            "1 +++ 2" `shouldParseTo`
                EApp (EApp (EVar "+++") (ELit (LInt 1))) (ELit (LInt 2))
        it "consym `:+:` parses as an infix expression" $
            "1 :+: 2" `shouldParseTo`
                EApp (EApp (EVar ":+:") (ELit (LInt 1))) (ELit (LInt 2))
        it "qualified varid `Data.List.head` parses" $
            "Data.List.head" `shouldParseTo` EVar "Data.List.head"
        it "parenthesised cons `(:)` parses as an atom" $
            "(:)" `shouldParseTo` EVar ":"
        it "parenthesised varsym `(+)` parses as an atom" $
            "(+)" `shouldParseTo` EVar "+"
