{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs2010ExprCtl (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec

import IHC.AST
import IHC.Parser (defaultFixityTable, parseExprAtEof)
import IHC.Source (Source, mkSource)

mkSrc :: ByteString -> Source
mkSrc = mkSource "<test>"

parseExpr :: ByteString -> IO (Either SomeException Expr)
parseExpr bs = try (parseExprAtEof (mkSrc bs) defaultFixityTable)

shouldParse :: ByteString -> Expectation
shouldParse bs = do
    r <- parseExpr bs
    case r of
        Right _ -> pure ()
        Left e  -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

shouldParseTo :: ByteString -> Expr -> Expectation
shouldParseTo bs expected = do
    r <- parseExpr bs
    case r of
        Right got -> got `shouldBe` expected
        Left e    -> expectationFailure
            ("expected parse success on " <> show bs <> ", got " <> show e)

spec :: Spec
spec = describe "Hs2010 — Expression control flow" $ do

    describe "5.1 atomic expressions" $ do
        it "5.1.1 variable `x`" $
            "x" `shouldParseTo` EVar "x"
        it "5.1.2 qualified variable `M.x`" $
            "M.x" `shouldParseTo` EVar "M.x"
        it "5.1.3 operator-as-varid `(+)`" $
            "(+)" `shouldParseTo` EVar "+"
        it "5.1.4 constructor `Just`" $
            "Just" `shouldParseTo` EVar "Just"
        it "5.1.5 constructor operator in parens `(:)`" $
            "(:)" `shouldParseTo` EVar ":"
        it "5.1.6 unit constructor `()`" $ shouldParse "()"
        it "5.1.7 nil-list constructor `[]`" $ shouldParse "[]"
        it "5.1.8a tuple constructor `(,)`" $
            shouldParse "(,)"
        it "5.1.8b tuple constructor `(,,)`" $
            shouldParse "(,,)"
        it "5.1.9 integer literal `1`" $
            "1" `shouldParseTo` ELit (LInt 1)
        it "5.1.10 float literal `1.5`" $
            "1.5" `shouldParseTo` ELit (LFloat 1.5)
        it "5.1.11 char literal `'a'`" $
            "'a'" `shouldParseTo` ELit (LChar 'a')
        it "5.1.12 string literal `\"x\"` (parser desugars to cons-list of chars)" $
            "\"x\"" `shouldParseTo`
                EApp (EApp (EVar ":") (ELit (LChar 'x'))) (EVar "[]")
        it "5.1.13 parenthesised expression `(e)`" $
            "(x)" `shouldParseTo` EVar "x"

    describe "5.2 application" $ do
        it "5.2.1 function application `f x y`" $
            "f x y" `shouldParseTo`
                EApp (EApp (EVar "f") (EVar "x")) (EVar "y")
        it "5.2.2 constructor application `Just 1`" $
            "Just 1" `shouldParseTo`
                EApp (EVar "Just") (ELit (LInt 1))

    describe "5.3 lambda abstractions" $ do
        it "5.3.1 single-arg lambda `\\x -> x`" $
            "\\x -> x" `shouldParseTo` ELam "x" (EVar "x")
        it "5.3.2 multi-arg lambda `\\x y -> y`" $
            "\\x y -> y" `shouldParseTo` ELam "x" (ELam "y" (EVar "y"))
        it "5.3.3 lambda with constructor pattern `\\(x:xs) -> x`" $
            shouldParse "\\(x:xs) -> x"
        it "5.3.4 lambda with as-pattern `\\a@b -> a`" $
            shouldParse "\\a@b -> a"

    describe "5.4 let bindings" $ do
        it "5.4.1 let with single binding" $
            "let x = 1 in x" `shouldParseTo`
                ELet [("x", ELit (LInt 1))] (EVar "x")
        it "5.4.2a let with multi-binding (explicit braces)" $
            shouldParse "let { x = 1; y = 2 } in x"
        it "5.4.2b let with multi-binding (semicolon separators, same line)" $
            shouldParse "let x = 1; y = 2 in x"
        it "5.4.3 let with type signature" $
            shouldParse "let x :: Int\n    x = 1\n in x"

    describe "5.5 conditional" $ do
        it "5.5.1 `if c then a else b`" $
            "if c then a else b" `shouldParseTo`
                EIf (EVar "c") (EVar "a") (EVar "b")
        it "5.5.2 `if` with optional semicolons" $
            pendingWith "known gap: optional `;` between `if`/`then`/`else`"

    describe "5.6 case expressions" $ do
        it "5.6.1 plain case alt `case x of A -> 1`" $
            shouldParse "case x of A -> 1"
        it "5.6.2 multi-clause case `case x of A -> 1; B -> 2`" $
            shouldParse "case x of { A -> 1; B -> 2 }"
        it "5.6.3 case alt with guards `_ | g -> e`" $
            shouldParse "case x of _ | g -> e"
        it "5.6.3b braced case alt with boolean guard and fallback" $
            shouldParse "case x of { p | cond -> e; _ -> z }"
        it "5.6.4 case alt with where clause" $
            shouldParse "case x of _ -> e where y = 1"
        it "5.6.5 empty case alternative list" $
            shouldParse "case x of {}"
        it "5.6.6 pattern guard inside case alt" $
            shouldParse "case m of { _ | Just x <- m -> x; _ -> z }"
        it "5.6.7 local-decl guard inside case alt" $
            shouldParse "case x of { _ | let y = x -> y }"

    describe "5.7 do expressions" $ do
        it "5.7.1 sequenced `do e1 ; e2`" $
            shouldParse "do { e1 ; e2 }"
        it "5.7.2 bind `do x <- m ; f x`" $
            shouldParse "do { x <- m ; f x }"
        it "5.7.3 let statement `do let x = 1 ; e`" $
            shouldParse "do { let x = 1 ; e }"
        it "5.7.4 empty statement `do ; e`" $
            shouldParse "do { ; e }"
        it "5.7.5 final must be expression — invalid `do x <- m`" $
            pendingWith "known gap: parser doesn't enforce do-block final-expression rule"
        it "leftover: PatternSignatures do-bind `n :: CInt <- peek p` binds n" $
            "do { n :: CInt <- peek p; return n }" `shouldParseTo`
                EDo [ SBind "n" (ETyApp (EApp (EVar "peek") (EVar "p")) "CInt")
                    , SExpr (EApp (EVar "return") (EVar "n"))
                    ]
        it "leftover: constructor-pattern do-bind `CInt n <- peek p` is not SExpr" $ do
            r <- parseExpr "do { CInt n <- peek p; return n }"
            let bindsN (SBind v _)     = v == "n"
                bindsN (SBangBind v _) = v == "n"
                bindsN (SLet bs)       = any ((== "n") . fst) bs
                bindsN _               = False
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "CInt n <- collapsed to SExpr (n never bound)"
                Right (EDo stmts)
                    | any bindsN stmts -> pure ()
                    | otherwise -> expectationFailure
                        ("n not bound in " <> show stmts)
                Right other -> expectationFailure
                    ("expected EDo, got " <> show other)

        -- Warp composeHeader / createAndTrim fill callbacks start with a
        -- function application (`poke` / `copyBytes`), not a bind.
        it "leftover: first statement in do is function application (not bind)" $ do
            r <- parseExpr "do { poke p x; return n }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SBind _ _ : _)) -> expectationFailure
                    "first-stmt application collapsed to SBind"
                Right (EDo (SBangBind _ _ : _)) -> expectationFailure
                    "first-stmt application collapsed to SBangBind"
                Right (EDo (SExpr e : rest))
                    | isApp e, lastReturnOrPure rest -> pure ()
                Right other -> expectationFailure
                    ("expected EDo starting with SExpr application, got "
                     <> show other)

        it "leftover: last statement is `return`" $
            "do { e1; return x }" `shouldParseTo`
                EDo [ SExpr (EVar "e1")
                    , SExpr (EApp (EVar "return") (EVar "x"))
                    ]

        it "leftover: last statement is `pure`" $
            "do { e1; pure x }" `shouldParseTo`
                EDo [ SExpr (EVar "e1")
                    , SExpr (EApp (EVar "pure") (EVar "x"))
                    ]

        it "leftover: nested do" $
            "do { x <- do { y <- m; return y }; return x }" `shouldParseTo`
                EDo [ SBind "x" (EDo
                        [ SBind "y" (EVar "m")
                        , SExpr (EApp (EVar "return") (EVar "y"))
                        ])
                    , SExpr (EApp (EVar "return") (EVar "x"))
                    ]

        it "leftover: let in do pins SLet" $
            "do { let x = 1; return x }" `shouldParseTo`
                EDo [ SLet [("x", ELit (LInt 1))]
                    , SExpr (EApp (EVar "return") (EVar "x"))
                    ]

        it "leftover: PatternSignatures `(x :: Int) <- m` binds x and tags Int" $ do
            r <- parseExpr "do { (x :: Int) <- m; return x }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "(x :: Int) <- collapsed to SExpr (x never bound)"
                Right (EDo (SBind "x" action : rest))
                    | hasTyApp "Int" action
                    , any (mentionsBinder "x") rest -> pure ()
                Right other -> expectationFailure
                    ("expected SBind x (ETyApp … Int), got " <> show other)

        it "leftover: `CInt n <- peek p` pins ETyApp action CInt" $ do
            r <- parseExpr "do { CInt n <- peek p; return n }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo stmts)
                    | any isCIntPinnedPeek stmts
                    , any (bindsName "n") stmts -> pure ()
                Right other -> expectationFailure
                    ("expected ETyApp peek CInt + binder n, got "
                     <> show other)

        -- network getSockOpt: `alloca $ \ptr -> do { … ; n :: CInt <- peek ptr }`
        it "leftover: PatternSignatures do-bind nested in alloca lambda" $
            "alloca $ \\p -> do { n :: CInt <- peek p; return n }"
                `shouldParseTo`
                    EApp (EVar "alloca")
                        (ELam "p" (EDo
                            [ SBind "n" (ETyApp (EApp (EVar "peek") (EVar "p"))
                                                "CInt")
                            , SExpr (EApp (EVar "return") (EVar "n"))
                            ]))

        -- IHP.HSX.Parser hsxComment: `body :: String <- manyTill …`
        it "leftover: HSX `body :: String <- manyTill` do-bind" $
            "do { body :: String <- manyTill anySingle end; pure body }"
                `shouldParseTo`
                    EDo [ SBind "body"
                            (ETyApp (EApp (EApp (EVar "manyTill")
                                                (EVar "anySingle"))
                                          (EVar "end"))
                                    "String")
                        , SExpr (EApp (EVar "pure") (EVar "body"))
                        ]

        -- Warp.Run recv4: `if S.null bs1 then return bs0 else do …`
        it "leftover: Warp empty-else-do `if c then return x else do`" $
            "if S.null bs1 then return bs0 else do { let bs2 = bs0 <> bs1; return bs2 }"
                `shouldParseTo`
                    EIf (EApp (EVar "S.null") (EVar "bs1"))
                        (EApp (EVar "return") (EVar "bs0"))
                        (EDo [ SLet [("bs2", EApp (EApp (EVar "<>")
                                                        (EVar "bs0"))
                                                  (EVar "bs1"))]
                             , SExpr (EApp (EVar "return") (EVar "bs2"))
                             ])

        it "leftover: Warp both-branch do `if c then do else do`" $
            "if h2 then do { return True } else do { bs0 <- recv4; return bs0 }"
                `shouldParseTo`
                    EIf (EVar "h2")
                        (EDo [SExpr (EApp (EVar "return") (EVar "True"))])
                        (EDo [ SBind "bs0" (EVar "recv4")
                             , SExpr (EApp (EVar "return") (EVar "bs0"))
                             ])

        -- Warp.Run runSettingsSocket: as-pattern Settings binder inside do
        it "leftover: Warp record as-pattern in do-bind" $ do
            r <- parseExpr
                    "do { set@Settings{settingsAccept = a} <- m; return a }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "set@Settings{…} <- collapsed to SExpr (set/a never bound)"
                Right (EDo stmts)
                    | any (bindsName "set") stmts
                      || any (bindsName "a") stmts -> pure ()
                Right other -> expectationFailure
                    ("expected as-pattern Settings do-bind, got "
                     <> show other)

        -- Warp File/Response/Date: `composeHeader ver s hs >>= connSendAll conn`
        it "leftover: Warp infix `(>>=)` is bare bind" $
            "composeHeader ver s hs >>= connSendAll conn" `shouldParseTo`
                EApp (EApp (EVar ">>=")
                           (EApp (EApp (EApp (EVar "composeHeader")
                                             (EVar "ver"))
                                       (EVar "s"))
                                 (EVar "hs")))
                     (EApp (EVar "connSendAll") (EVar "conn"))

        it "leftover: Warp `initialize >>= action` is bare bind" $
            "initialize >>= action" `shouldParseTo`
                EApp (EApp (EVar ">>=") (EVar "initialize")) (EVar "action")

        it "leftover: infix `(=<<)` is bare bind-flip" $
            "k =<< m" `shouldParseTo`
                EApp (EApp (EVar "=<<") (EVar "k")) (EVar "m")

        it "leftover: `(>>=)` is EVar \">>=\" " $
            "(>>=)" `shouldParseTo` EVar ">>="

        it "leftover: `(=<<)` is EVar \"=<<\" " $
            "(=<<)" `shouldParseTo` EVar "=<<"

    -- Warp / HSX leftover: parenthesised operators and sections must
    -- parse as the *bare* operator, not a last-writer FQN (Category
    -- `(.)`, Semigroup `(<>)`, Alternative `(<|>)`, TH `$`).  Pin the
    -- desugared AST so eval leftovers cannot be blamed on parse.
    describe "leftover Parser: Warp/HSX operator sections and infix ops" $ do
        it "leftover: `(.)` is EVar \".\", not a record-dot or FQN" $
            "(.)" `shouldParseTo` EVar "."
        it "leftover: `(<>)` is EVar \"<>\"" $
            "(<>)" `shouldParseTo` EVar "<>"
        it "leftover: `(<|>)` is EVar \"<|>\"" $
            "(<|>)" `shouldParseTo` EVar "<|>"
        it "leftover: `($)` is EVar \"$\", not a TH splice" $
            "($)" `shouldParseTo` EVar "$"
        it "leftover: left section `(f .)` desugars around bare `.`" $
            "(f .)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar ".") (EVar "f")) (EVar "$s"))
        it "leftover: right section `(. g)` desugars around bare `.`" $
            "(. g)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar ".") (EVar "$s")) (EVar "g"))
        it "leftover: left section `(bs <>)` (warp Request prepend)" $
            "(bs <>)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "<>") (EVar "bs")) (EVar "$s"))
        it "leftover: right section `(<> x)` desugars around bare `<>`" $
            "(<> x)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "<>") (EVar "$s")) (EVar "x"))
        it "leftover: left section `(p <|>)` desugars around bare `<|>`" $
            "(p <|>)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "<|>") (EVar "p")) (EVar "$s"))
        it "leftover: right section `(<|> q)` desugars around bare `<|>`" $
            "(<|> q)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "<|>") (EVar "$s")) (EVar "q"))
        it "leftover: left section `(f $)` desugars around bare `$`" $
            "(f $)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "$") (EVar "f")) (EVar "$s"))
        it "leftover: right section `($ x)` desugars around bare `$`" $
            "($ x)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "$") (EVar "$s")) (EVar "x"))
        it "leftover: infix `f . g` is `(.)` f g with bare `.`" $
            "f . g" `shouldParseTo`
                EApp (EApp (EVar ".") (EVar "f")) (EVar "g")
        it "leftover: infix `a <> b` is `(<>)` a b" $
            "a <> b" `shouldParseTo`
                EApp (EApp (EVar "<>") (EVar "a")) (EVar "b")
        it "leftover: infix `p <|> q` is `(<|>)` p q" $
            "p <|> q" `shouldParseTo`
                EApp (EApp (EVar "<|>") (EVar "p")) (EVar "q")
        it "leftover: infix `$` is application (not leftover `$` FQN)" $
            "f $ x" `shouldParseTo` EApp (EVar "f") (EVar "x")
        it "leftover: infix backtick `ptr `plusPtr` off`" $
            "ptr `plusPtr` off" `shouldParseTo`
                EApp (EApp (EVar "plusPtr") (EVar "ptr")) (EVar "off")
        it "leftover: qualified infix backtick `` `BS.isPrefixOf` ``" $
            "\"h2\" `BS.isPrefixOf` proto" `shouldParseTo`
                EApp (EApp (EVar "BS.isPrefixOf")
                        (EApp (EApp (EVar ":") (ELit (LChar 'h')))
                            (EApp (EApp (EVar ":") (ELit (LChar '2')))
                                (EVar "[]"))))
                    (EVar "proto")
        it "leftover: left backtick section `(10 `div`)`" $
            "(10 `div`)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "div") (ELit (LInt 10))) (EVar "$s"))
        it "leftover: right backtick section `(`mod` 5)`" $
            "(`mod` 5)" `shouldParseTo`
                ELam "$s" (EApp (EApp (EVar "mod") (EVar "$s")) (ELit (LInt 5)))

    describe "leftover Parser: Warp/HSX do-bind + else-do AST pins" $ do
        it "leftover: constructor-pattern do-bind `Just x <- m` is not SExpr" $ do
            r <- parseExpr "do { Just x <- m; return x }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "Just x <- collapsed to SExpr (x never bound)"
                Right (EDo stmts)
                    | any (bindsName "x") stmts -> pure ()
                    | otherwise -> expectationFailure
                        ("x not bound in " <> show stmts)
                Right other -> expectationFailure
                    ("expected EDo, got " <> show other)

        it "leftover: `Just x <- m` pins PCon Just in the lowered bind" $ do
            r <- parseExpr "do { Just x <- m; return x }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo stmts)
                    | any (hasConPatBind "Just" "x") stmts -> pure ()
                Right other -> expectationFailure
                    ("expected PCon Just [PVar x], got " <> show other)

        it "leftover: record as-pattern `s@Settings{..} <- getSettings` binds s" $ do
            r <- parseExpr "do { s@Settings{..} <- getSettings; return s }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "s@Settings{..} <- collapsed to SExpr (s never bound)"
                Right (EDo stmts)
                    | any (bindsName "s") stmts -> pure ()
                    | otherwise -> expectationFailure
                        ("s not bound in " <> show stmts)
                Right other -> expectationFailure
                    ("expected EDo, got " <> show other)

        it "leftover: `s@Settings{..} <-` pins PAs s (PRecordWild Settings)" $ do
            r <- parseExpr "do { s@Settings{..} <- getSettings; return s }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo stmts)
                    | any (hasAsRecordWild "s" "Settings") stmts -> pure ()
                Right other -> expectationFailure
                    ("expected PAs s (PRecordWild Settings), got "
                     <> show other)

        it "leftover: view-pattern do-bind `(fromIntegral -> n) <- peek p` binds n" $ do
            r <- parseExpr
                    "do { (fromIntegral -> n) <- peek p; return n }"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right (EDo (SExpr _ : _)) -> expectationFailure
                    "(fromIntegral -> n) <- collapsed to SExpr"
                Right (EDo stmts)
                    | any (bindsName "n") stmts
                    , any (hasViewPatBind (EVar "fromIntegral") "n") stmts
                        -> pure ()
                Right other -> expectationFailure
                    ("expected PView fromIntegral (PVar n), got "
                     <> show other)

        it "leftover: bang-bind `!n <- peek p` is SBangBind n" $
            "do { !n <- peek p; return n }" `shouldParseTo`
                EDo [ SBangBind "n" (EApp (EVar "peek") (EVar "p"))
                    , SExpr (EApp (EVar "return") (EVar "n"))
                    ]

        it "leftover: type application in do `return @Payload x`" $
            "do { return @Payload x }" `shouldParseTo`
                EDo [ SExpr (EApp (ETyApp (EVar "return") "Payload")
                                  (EVar "x")) ]

        it "leftover: implicit-param let in do `let ?callStack = stk`" $
            "do { let ?callStack = stk; return ?callStack }" `shouldParseTo`
                EDo [ SImplicitLet [("callStack", EVar "stk")]
                    , SExpr (EApp (EVar "return")
                                  (EImplicitRef "callStack"))
                    ]

        it "leftover: `else do` is EDo (not a dropped do)" $
            "if c then a else do x" `shouldParseTo`
                EIf (EVar "c") (EVar "a") (EDo [SExpr (EVar "x")])

        it "leftover: layout after `else` then `do`" $
            "if c then a else do\n  x" `shouldParseTo`
                EIf (EVar "c") (EVar "a") (EDo [SExpr (EVar "x")])

        it "leftover: both branches are do `if c then do a else do b`" $
            "if c then do a else do b" `shouldParseTo`
                EIf (EVar "c")
                    (EDo [SExpr (EVar "a")])
                    (EDo [SExpr (EVar "b")])

        it "leftover: megaparsec `\\\\case { Tokens ts -> ts; _ -> z }`" $
            "\\case { Tokens ts -> ts; _ -> z }" `shouldParseTo`
                ELam "$lc"
                    (ECase (EVar "$lc")
                        [ Alt (PCon "Tokens" [PVar "ts"]) (EVar "ts")
                        , Alt PWild (EVar "z")
                        ])

        it "leftover: multiway-if keeps `return @Payload` on both branches" $ do
            r <- parseExpr
                    "if | n == 0 -> return @Payload True | otherwise -> return @Payload False"
            case r of
                Left e -> expectationFailure
                    ("expected parse success, got " <> show e)
                Right e
                    | returnPayloadCount e == 2 -> pure ()
                    | otherwise -> expectationFailure
                        ("expected two return @Payload, got " <> show e)

        it "leftover: `f $! x` desugars to seq x (f x)" $
            "f $! x" `shouldParseTo`
                EApp (EApp (EVar "seq") (EVar "x"))
                     (EApp (EVar "f") (EVar "x"))

    describe "EOF strictness — silent-skip protection" $ do
        it "rejects trailing tokens after a complete expression (e.g. `1 in 2`)" $ do
            r <- parseExpr "1 in 2"
            case r of
                Left _  -> pure ()
                Right e -> expectationFailure
                    ("expected ParseError on trailing tokens, got " <> show e)
        it "accepts a complete expression with no trailing tokens" $
            "1" `shouldParseTo` ELit (LInt 1)

isApp :: Expr -> Bool
isApp (EApp _ _) = True
isApp _          = False

lastReturnOrPure :: [Stmt] -> Bool
lastReturnOrPure [] = False
lastReturnOrPure [SExpr e] = isReturnOrPure e
lastReturnOrPure (_:xs)    = lastReturnOrPure xs

isReturnOrPure :: Expr -> Bool
isReturnOrPure (EApp (EVar n) _) = n == "return" || n == "pure"
isReturnOrPure (ETyApp e _)      = isReturnOrPure e
isReturnOrPure _                 = False

hasTyApp :: ByteString -> Expr -> Bool
hasTyApp ty (ETyApp _ t) = t == ty
hasTyApp ty (EApp f _)   = hasTyApp ty f
hasTyApp _ _             = False

mentionsBinder :: ByteString -> Stmt -> Bool
mentionsBinder n (SExpr e)         = exprMentions n e
mentionsBinder n (SBind _ e)       = exprMentions n e
mentionsBinder n (SBangBind _ e)   = exprMentions n e
mentionsBinder n (SLet bs)         = any (exprMentions n . snd) bs
mentionsBinder n (SImplicitLet bs) = any (exprMentions n . snd) bs

exprMentions :: ByteString -> Expr -> Bool
exprMentions n (EVar v)     = v == n
exprMentions n (EApp f a)   = exprMentions n f || exprMentions n a
exprMentions n (ETyApp e _) = exprMentions n e
exprMentions _ _            = False

bindsName :: ByteString -> Stmt -> Bool
bindsName n (SBind v _)     = v == n
bindsName n (SBangBind v _) = v == n
bindsName n (SLet bs)       = any ((== n) . fst) bs
bindsName _ _               = False

hasConPatBind :: ByteString -> ByteString -> Stmt -> Bool
hasConPatBind con var (SLet bs) = any (caseHasCon con var . snd) bs
hasConPatBind _ _ _             = False

caseHasCon :: ByteString -> ByteString -> Expr -> Bool
caseHasCon con var (ECase _ alts) =
    any (\(Alt p _) -> p == PCon con [PVar var]) alts
caseHasCon _ _ _ = False

hasAsRecordWild :: ByteString -> ByteString -> Stmt -> Bool
hasAsRecordWild asName con (SLet bs) =
    any (caseHasAsRecordWild asName con . snd) bs
hasAsRecordWild _ _ _ = False

caseHasAsRecordWild :: ByteString -> ByteString -> Expr -> Bool
caseHasAsRecordWild asName con (ECase _ alts) =
    any (\(Alt p _) -> p == PAs asName (PRecordWild con)) alts
caseHasAsRecordWild _ _ _ = False

hasViewPatBind :: Expr -> ByteString -> Stmt -> Bool
hasViewPatBind viewFn var (SLet bs) =
    any (caseHasView viewFn var . snd) bs
hasViewPatBind _ _ _ = False

caseHasView :: Expr -> ByteString -> Expr -> Bool
caseHasView viewFn var (ECase _ alts) =
    any (\(Alt p _) -> p == PView viewFn (PVar var)) alts
caseHasView _ _ _ = False

returnPayloadCount :: Expr -> Int
returnPayloadCount (ETyApp (EVar "return") "Payload") = 1
returnPayloadCount (ETyApp e _) = returnPayloadCount e
returnPayloadCount (EApp f a)   = returnPayloadCount f + returnPayloadCount a
returnPayloadCount (EIf c t e)  =
    returnPayloadCount c + returnPayloadCount t + returnPayloadCount e
returnPayloadCount (EDo ss)     = sum (map stmtReturnPayload ss)
returnPayloadCount _            = 0

stmtReturnPayload :: Stmt -> Int
stmtReturnPayload (SExpr e)       = returnPayloadCount e
stmtReturnPayload (SBind _ e)     = returnPayloadCount e
stmtReturnPayload (SBangBind _ e) = returnPayloadCount e
stmtReturnPayload (SLet bs)       = sum (map (returnPayloadCount . snd) bs)
stmtReturnPayload (SImplicitLet bs) =
    sum (map (returnPayloadCount . snd) bs)

isCIntPinnedPeek :: Stmt -> Bool
isCIntPinnedPeek (SBind _ (ETyApp action "CInt")) = isPeekApp action
isCIntPinnedPeek _                                = False

isPeekApp :: Expr -> Bool
isPeekApp (EApp (EVar "peek") _) = True
isPeekApp (EApp f _)             = isPeekApp f
isPeekApp (ETyApp e _)           = isPeekApp e
isPeekApp _                      = False
