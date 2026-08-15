{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsExtTypeApps (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST (Expr(..), Lit(..), Name, Stmt(..))
import IHC.Parser
    ( ParseError
    , defaultFixityTable
    , parseExprAtEof
    )
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

isParseError :: SomeException -> Bool
isParseError e = case fromException e of
    Just (_ :: ParseError) -> True
    Nothing                -> False

-- | Assert that an expression parses cleanly. Surfaces any thrown
-- exception (including 'ParseError') as a failed expectation.
parseExprOk :: ByteString -> Expectation
parseExprOk bs = do
    r <- try (parseExprAtEof (mkSrc bs) defaultFixityTable)
    case r of
        Right _                  -> pure ()
        Left (e :: SomeException) -> expectationFailure
            ("expected success, got " <> show e)

-- | Parse-only success for top-level declaration fixtures.
-- @loadProgramFromSource@ runs parse, scan, elaborate, and link;
-- a parse-only test accepts either @Right _@ (full pipeline succeeded)
-- or a @Left e@ where @e@ is NOT a 'ParseError' (parse succeeded but
-- a later phase legitimately failed on a stub program).
parseProgram :: ByteString -> Expectation
parseProgram bs = do
    r <- try (loadProgramFromSource [] (mkSrc bs))
    case r of
        Right _                        -> pure ()
        Left (e :: SomeException)
            | isParseError e -> expectationFailure
                ("expected parse success, got ParseError: " <> show e)
            | otherwise      -> pure ()

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- try (parseExprAtEof (mkSrc bs) defaultFixityTable)
    case r of
        Right got -> got `shouldBe` expected
        Left (e :: SomeException) -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

hasReturnPayload :: Expr -> Bool
hasReturnPayload (ETyApp (EVar "return") "Payload") = True
hasReturnPayload (ETyApp e _) = hasReturnPayload e
hasReturnPayload (EApp f a) = hasReturnPayload f || hasReturnPayload a
hasReturnPayload _ = False

hasChainedTyApp :: Name -> [Name] -> Expr -> Bool
hasChainedTyApp headName tags e = go (reverse tags) e
  where
    go [] (EVar n) = n == headName
    go (t:ts) (ETyApp inner ty) = ty == t && go ts inner
    go ts (EApp f _) = go ts f
    go _ _ = False

spec :: Spec
spec = describe "HsExt — Type applications & operators" $ do

    describe "TypeApplications" $ do
        it "TypeApplications: f @Int x parses" $
            parseExprOk "f @Int x"

        it "TypeApplications: read @Int \"1\" parses" $
            parseExprOk "read @Int \"1\""

        it "TypeApplications: id @(Maybe a) Nothing parses" $
            parseExprOk "id @(Maybe a) Nothing"

        it "TypeApplications: bare type variable f @a x parses" $
            parseExprOk "f @a x"

        it "TypeApplications: chained f @Int @Bool x parses" $
            parseExprOk "f @Int @Bool x"

        it "leftover: return @Payload is ETyApp return Payload" $ do
            r <- try (parseExprAtEof (mkSrc "return @Payload") defaultFixityTable)
            case r of
                Right e -> e `shouldBe` ETyApp (EVar "return") "Payload"
                Left (e :: SomeException) -> expectationFailure
                    ("expected success, got " <> show e)

        it "leftover: return @Payload $! n == 0 keeps @Payload on return" $ do
            r <- try (parseExprAtEof (mkSrc "return @Payload $! n == 0")
                        defaultFixityTable)
            case r of
                Left (e :: SomeException) -> expectationFailure
                    ("expected success, got " <> show e)
                Right e ->
                    unless (hasReturnPayload e) $
                        expectationFailure
                            ("return @Payload dropped from " <> show e)

        it "TypeApplications: (+) @Int is ETyApp on (+)" $
            "(+) @Int" `shouldParseTo` ETyApp (EVar "+") "Int"

        it "TypeApplications: (+ @Int) is ETyApp on (+) inside parens" $
            "(+ @Int)" `shouldParseTo` ETyApp (EVar "+") "Int"

        it "TypeApplications: (+) @Int 1 2 applies after type app" $
            "(+) @Int 1 2" `shouldParseTo`
                EApp (EApp (ETyApp (EVar "+") "Int") (ELit (LInt 1)))
                     (ELit (LInt 2))

        it "TypeApplications: (+ @Int) 1 2 applies the typed operator" $
            "(+ @Int) 1 2" `shouldParseTo`
                EApp (EApp (ETyApp (EVar "+") "Int") (ELit (LInt 1)))
                     (ELit (LInt 2))

        it "TypeApplications: (+ @Int 1) is a right section of (+) @Int" $
            "(+ @Int 1)" `shouldParseTo`
                ELam "$s"
                    (EApp (EApp (ETyApp (EVar "+") "Int") (EVar "$s"))
                          (ELit (LInt 1)))

        it "TypeApplications: (<|>) @Parser is ETyApp on <|>" $
            "(<|>) @Parser" `shouldParseTo` ETyApp (EVar "<|>") "Parser"

        it "TypeApplications: infix backtick 1 `const` @Int @Bool True" $
            "1 `const` @Int @Bool True" `shouldParseTo`
                EApp (EApp (ETyApp (ETyApp (EVar "const") "Int") "Bool")
                           (ELit (LInt 1)))
                     (EVar "True")

        it "TypeApplications: do { return @Int 1 } keeps @Int on return" $
            "do { return @Int 1 }" `shouldParseTo`
                EDo [ SExpr (EApp (ETyApp (EVar "return") "Int")
                                  (ELit (LInt 1))) ]

        it "TypeApplications: do { empty @Parser } keeps @Parser" $
            "do { empty @Parser }" `shouldParseTo`
                EDo [ SExpr (ETyApp (EVar "empty") "Parser") ]

        it "TypeApplications: empty @Parser is ETyApp on empty" $
            "empty @Parser" `shouldParseTo` ETyApp (EVar "empty") "Parser"

        it "TypeApplications: leftover HSX quote `[| toHtmlRaw @Text \"x\" |]`" $
            "[| toHtmlRaw @Text \"x\" |]" `shouldParseTo`
                EQuote (EApp (ETyApp (EVar "toHtmlRaw") "Text")
                             (EApp (EApp (EVar ":") (ELit (LChar 'x')))
                                   (EVar "[]")))

        it "TypeApplications: leftover `toHtmlRaw @Text` outside a quote" $
            "toHtmlRaw @Text \"x\"" `shouldParseTo`
                EApp (ETyApp (EVar "toHtmlRaw") "Text")
                     (EApp (EApp (EVar ":") (ELit (LChar 'x'))) (EVar "[]"))

        it "TypeApplications: parse @Void @Text is chained ETyApp" $
            "parse @Void @Text" `shouldParseTo`
                ETyApp (ETyApp (EVar "parse") "Void") "Text"

        it "TypeApplications: leftover parse @Void @Text keeps both tags" $ do
            r <- try (parseExprAtEof (mkSrc "parse @Void @Text input")
                        defaultFixityTable)
            case r of
                Left (e :: SomeException) -> expectationFailure
                    ("expected success, got " <> show e)
                Right e ->
                    unless (hasChainedTyApp "parse" ["Void", "Text"] e) $
                        expectationFailure
                            ("parse @Void @Text dropped from " <> show e)

        -- Warp serveConnection / HSX parseHsx: TypeApplications inside
        -- do and on operators must stay on the callee, not migrate onto
        -- the application or the next statement.
        it "leftover: TypeApp in do-bind `x <- parse @Void @Text input`" $
            "do { x <- parse @Void @Text input; return x }" `shouldParseTo`
                EDo [ SBind "x" (EApp (ETyApp (ETyApp (EVar "parse") "Void")
                                              "Text")
                                     (EVar "input"))
                    , SExpr (EApp (EVar "return") (EVar "x"))
                    ]

        it "leftover: TypeApp on Alternative in do `empty @Parser <|> q`" $
            "do { empty @Parser <|> q }" `shouldParseTo`
                EDo [ SExpr (EApp (EApp (EVar "<|>")
                                        (ETyApp (EVar "empty") "Parser"))
                                  (EVar "q")) ]

        it "leftover: TypeApp on `(>>=) @IO`" $
            "(>>=) @IO" `shouldParseTo` ETyApp (EVar ">>=") "IO"

        it "leftover: TypeApp on infix-used operator `(<|>) @Parser p q`" $
            "(<|>) @Parser p q" `shouldParseTo`
                EApp (EApp (ETyApp (EVar "<|>") "Parser") (EVar "p"))
                     (EVar "q")

        it "leftover: TypeApp in quoted do `[| do { return @Int 1 } |]`" $
            "[| do { return @Int 1 } |]" `shouldParseTo`
                EQuote (EDo [ SExpr (EApp (ETyApp (EVar "return") "Int")
                                          (ELit (LInt 1))) ])

    describe "TypeOperators" $ do
        it "TypeOperators: type a + b = Either a b parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \type a + b = Either a b\n\
                \main = pure ()\n"

        it "TypeOperators: data a :*: b = a :*: b parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \infixl 7 :*:\n\
                \data a :*: b = a :*: b\n\
                \main = pure ()\n"

        it "TypeOperators: class C (a :*: b) parses" $
            parseProgram
                "{-# LANGUAGE TypeOperators #-}\n\
                \module M where\n\
                \infixl 7 :*:\n\
                \data a :*: b = a :*: b\n\
                \class C t where\n\
                \    method :: t -> Int\n\
                \instance C (a :*: b) where\n\
                \    method _ = 0\n\
                \main = pure ()\n"

    describe "AllowAmbiguousTypes" $ do
        it "AllowAmbiguousTypes: forall a. Int -> Int parses" $
            parseProgram
                "{-# LANGUAGE AllowAmbiguousTypes #-}\n\
                \{-# LANGUAGE ExplicitForAll #-}\n\
                \module M where\n\
                \f :: forall a. Int -> Int\n\
                \f x = x\n\
                \main = pure ()\n"

    describe "PartialTypeSignatures" $ do
        it "PartialTypeSignatures: f :: _ -> Int" $
            parseProgram
                "{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: _ -> Int\n\
                \f x = 0\n\
                \main = pure ()\n"

        it "PartialTypeSignatures: f :: a -> _" $
            parseProgram
                "{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: a -> _\n\
                \f x = 0\n\
                \main = pure ()\n"

    describe "NamedWildcards" $ do
        it "NamedWildcards: f :: _a -> _a" $
            parseProgram
                "{-# LANGUAGE NamedWildcards #-}\n\
                \{-# LANGUAGE PartialTypeSignatures #-}\n\
                \module M where\n\
                \f :: _a -> _a\n\
                \f x = x\n\
                \main = pure ()\n"
