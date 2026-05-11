{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property: source-level string literals at /expression/
-- position desugar to a cons-chain of 'LChar's.
--
-- 'ELit (LStr ...)' is intentionally absent from
-- 'Properties.RoundTrip' at expression position because the
-- parser does not produce it from a source-level string literal
-- — it desugars @"hello"@ into
--
--     EApp (EApp (EVar \":\") (ELit (LChar 'h')))
--          (EApp (EApp (EVar \":\") (ELit (LChar 'e')))
--                (... (EVar \"[]\")))
--
-- via 'IHC.Parser.stringToConsList' at @src/IHC/Parser.hs:3771@.
-- @LStr@ only survives at /pattern/ position (parser line 2560,
-- where it is exercised by 'Properties.RoundTrip'\'s 'PLit'
-- arm).
--
-- This module fills the third carve-out from the original Phase 2
-- plan — alongside Properties.SectionDesugar (sections desugar to
-- lambdas) and Properties.DoDesugar (do-blocks desugar to @>>=@\/
-- @>>@ chains) — with a /spec/ property that asserts the actual
-- desugaring matches the documented shape.
--
-- Generator scope: ASCII bytes only (0x00..0x7F).  The lexer
-- decodes @\\<n>@ escapes for codepoints above 0x7F into the
-- 'Char' with that codepoint and then UTF-8-encodes into the
-- TkStr 'ByteString', so a generated raw byte > 0x7F would not
-- match its parsed bit pattern.  Restricting to ASCII keeps the
-- byte == codepoint invariant.  Lifting the restriction would
-- require a Char-list -> UTF-8 ByteString round-trip and is a
-- follow-up.
module Properties.StringDesugar (spec) where

import Control.Exception (SomeException, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr)
import Data.Word (Word8)

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , forAll
    , frequency
    , ioProperty
    , property
    , vectorOf
    , (===)
    )

import IHC.AST (Expr(..), Lit(..))
import IHC.Parser (ParseError, defaultFixityTable, parseExprAtEof)
import IHC.Source (mkSource)


--------------------------------------------------------------------------------
-- Spec — mirror of IHC.Parser.stringToConsList
--------------------------------------------------------------------------------

-- | The AST shape the parser is expected to produce for a
-- source-level string literal.  Mirror of
-- 'IHC.Parser.stringToConsList'.  Each byte of the lexer's
-- 'TkStr' payload becomes one 'LChar' (via @chr . fromIntegral@,
-- which is the body of 'BC.unpack').  Generator stays inside
-- ASCII so byte == codepoint.
stringToConsList :: ByteString -> Expr
stringToConsList bs
    | BS.null bs = EVar "[]"
    | otherwise  =
        let h = BS.head bs
            c = chr (fromIntegral h)
            t = BS.tail bs
        in EApp (EApp (EVar ":") (ELit (LChar c)))
                (stringToConsList t)


--------------------------------------------------------------------------------
-- Generator + pretty-printer
--------------------------------------------------------------------------------

-- | A 0–16 byte ASCII string.  Empty strings are reachable —
-- @parseExprAtEof "\\\"\\\"" defaultFixityTable@ should produce
-- @EVar "[]"@.
genAsciiString :: Gen ByteString
genAsciiString = do
    n <- choose (0, 16)
    BS.pack <$> vectorOf n genAsciiByte


-- | Bias toward printable bytes so generated literals look
-- reasonable; include a small fraction of control bytes so the
-- escape paths are exercised.
genAsciiByte :: Gen Word8
genAsciiByte = frequency
    [ (40, choose (0x20, 0x7E))   -- printable ASCII
    , ( 5, choose (0x00, 0x1F))   -- C0 control bytes (need escape)
    , ( 1, pure 0x7F)             -- DEL
    ]


-- | Render an ASCII 'ByteString' as a Haskell source string
-- literal, escaping control bytes, @\"@, and @\\@.  Numeric
-- escapes always carry a trailing @\\&@ so adjacent digits in
-- the next byte cannot extend the escape.
prettyStringLit :: ByteString -> ByteString
prettyStringLit bs = "\"" <> BS.concatMap escapeStrByte bs <> "\""


escapeStrByte :: Word8 -> ByteString
escapeStrByte b
    | b == 0x5C                 = "\\\\"             -- '\\'
    | b == 0x22                 = "\\\""             -- '\"'
    | b == 0x0A                 = "\\n"
    | b == 0x09                 = "\\t"
    | b == 0x0D                 = "\\r"
    | b >= 0x20 && b < 0x7F     = BS.singleton b
    | otherwise                 = BC.pack ("\\" <> show b <> "\\&")


--------------------------------------------------------------------------------
-- Property
--------------------------------------------------------------------------------

prop_string_desugar :: Property
prop_string_desugar = forAll genAsciiString $ \bs -> ioProperty $ do
    let src      = prettyStringLit bs
        expected = stringToConsList bs
    r <- try @SomeException
        (parseExprAtEof (mkSource "<str>" src) defaultFixityTable)
    pure $ case r of
        Right actual ->
            counterexample
                ( "string-literal desugaring mismatch:\n  bytes    = "
                  <> show bs
                  <> "\n  src      = " <> show src
                  <> "\n  expected = " <> show expected
                  <> "\n  actual   = " <> show actual )
                (actual === expected)
        Left ex ->
            counterexample
                ( "parser rejected string literal:\n  bytes = "
                  <> show bs
                  <> "\n  src   = " <> show src
                  <> "\n  error = " <> formatExn ex )
                (property False)


-- | Render an exception with the parser's @file:line:col@ form
-- when it is a 'ParseError', otherwise via @show@.
formatExn :: SomeException -> String
formatExn e = case fromException e of
    Just (pe :: ParseError) -> show pe
    Nothing                 -> show e


--------------------------------------------------------------------------------
-- Spec wiring
--------------------------------------------------------------------------------

spec :: Spec
spec =
    describe "Property — string-literal desugaring (Phase 2.O)" $
        modifyMaxSuccess (const 500) $
            prop "\"...\" at expression position desugars to the documented LChar cons-chain"
                prop_string_desugar
