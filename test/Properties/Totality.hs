{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications   #-}

-- | Property-based parser totality fuzz suite (Phase 1 of the
-- \"random Haskell programs\" plan).
--
-- Asserts that the parser is total: every input either parses
-- cleanly ('Right' 'Expr') or fails with the project's documented
-- error type ('IHC.Parser.ParseError', or the lexer-level
-- 'IHC.Lexer.LexError' bridged via @liftLex@).  Anything else —
-- @head []@, @undefined@, a 'PatternMatchFail', a stray
-- 'ErrorCall' from @Data.Char.chr@ — is a parser bug.
--
-- Two strategies feed the parser:
--
--   * 'prop_random_bytes_no_crash' — uniform-random 'ByteString'
--     biased toward printable ASCII.  Coverage is broad but most
--     inputs are rejected immediately, so signal-to-noise is low.
--
--   * 'prop_fixture_mutation_no_crash' — load a Coverage fixture
--     and apply a single byte-level mutation (drop, duplicate,
--     swap, truncate, insert).  Mutating near-valid programs
--     explores the part of the input space where bugs cluster
--     (typo-and-error paths) much more efficiently than uniform
--     random bytes.
--
-- Reproduce → fixture → fix → verify: when QuickCheck shrinks a
-- counterexample, drop the resulting bytes into
-- @test/Fixtures/Coverage/@ as a permanent regression canary.  See
-- the parser bug pattern in @test/ParserBugs.hs@.
module Properties.Totality (spec) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import Data.Word (Word8)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Test.Hspec (Spec, describe, runIO)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , frequency
    , ioProperty
    , oneof
    , property
    , sized
    , vectorOf
    )

import IHC.Lexer (LexError)
import IHC.Parser (ParseError, defaultFixityTable, parseExprOnly)
import IHC.Source (mkSource)


--------------------------------------------------------------------------------
-- Totality oracle
--------------------------------------------------------------------------------

-- | An exception is "expected" — i.e. NOT a totality bug — if it is
-- the project's documented parser-error type or the lexer-level
-- error that 'parseExprOnly' bridges via @liftLex@.  Everything
-- else (e.g. an @ErrorCall@ from @head []@, a 'PatternMatchFail',
-- an arithmetic overflow) means the parser crashed and the test
-- must fail.
expectedException :: SomeException -> Bool
expectedException e
    | Just (_ :: ParseError) <- fromException e = True
    | Just (_ :: LexError)   <- fromException e = True
    | otherwise                                  = False


-- | Drive 'parseExprOnly' on a single 'ByteString' and decide
-- whether the result is totality-clean.  Forces the result to
-- WHNF via 'evaluate' so a thunked exception in the returned
-- 'Expr' is still caught here rather than escaping to the
-- harness.
checkParseTotality :: ByteString -> IO Property
checkParseTotality bs = do
    r <- try @SomeException
        (parseExprOnly (mkSource "<fuzz>" bs) defaultFixityTable >>= evaluate)
    pure $ case r of
        Right _                     -> property True
        Left e | expectedException e -> property True
        Left e                       -> counterexample
            ( "uncaught exception (not ParseError / LexError):\n  "
              <> show e
              <> "\n  on input bytes: "
              <> show bs )
            (property False)


--------------------------------------------------------------------------------
-- Strategy 1: uniform-random bytes, biased toward printable ASCII
--------------------------------------------------------------------------------

-- | A byte generator weighted toward printable ASCII.  Pure-uniform
-- 'Word8' wastes most cases on UTF-8 garbage that the lexer rejects
-- in the first byte; biasing toward 0x20–0x7E means generated
-- inputs are more likely to look like (broken) Haskell.
arbitraryByte :: Gen Word8
arbitraryByte = frequency
    [ (50, choose (32, 126))    -- printable ASCII
    , ( 5, choose ( 0,  31))    -- controls (incl. \t \n \r)
    , ( 5, choose (127, 255))   -- high bytes (UTF-8 leader bytes)
    ]


-- | Generate a 'ByteString' whose length scales with QuickCheck's
-- size parameter.  Empty inputs are reachable.
genBytes :: Gen ByteString
genBytes = sized $ \n -> do
    len <- choose (0, max 0 n)
    BS.pack <$> vectorOf len arbitraryByte


prop_random_bytes_no_crash :: Property
prop_random_bytes_no_crash =
    forAll genBytes $ \bs -> ioProperty (checkParseTotality bs)


--------------------------------------------------------------------------------
-- Strategy 2: corpus-seeded byte mutation
--------------------------------------------------------------------------------

-- | One byte-level mutation operator.  Each is a no-op for
-- out-of-range indices; the totality check still runs on the
-- unchanged bytes, so we don't need to filter degenerate cases
-- out of the generator.
data Mutation
    = DropByte    !Int
    | DupByte     !Int
    | SwapBytes   !Int
    | TruncateAt  !Int
    | InsertByte  !Int !Word8
    deriving (Show)


applyMutation :: ByteString -> Mutation -> ByteString
applyMutation bs m = case m of
    DropByte i
        | inRange i        -> BS.take i bs <> BS.drop (i + 1) bs
        | otherwise        -> bs
    DupByte i
        | inRange i        -> BS.take (i + 1) bs <> BS.drop i bs
        | otherwise        -> bs
    SwapBytes i
        | i >= 0, i + 1 < n ->
            BS.take i bs
              <> BS.singleton (BS.index bs (i + 1))
              <> BS.singleton (BS.index bs i)
              <> BS.drop (i + 2) bs
        | otherwise        -> bs
    TruncateAt i
        | i >= 0 && i <= n -> BS.take i bs
        | otherwise        -> bs
    InsertByte i w
        | i >= 0 && i <= n -> BS.take i bs <> BS.singleton w <> BS.drop i bs
        | otherwise        -> bs
  where
    n         = BS.length bs
    inRange j = j >= 0 && j < n


-- | Pick one fixture from the corpus, then one mutation site.
-- Empty corpora collapse to a single trivial probe so the property
-- can still run on a fresh checkout where the fixtures haven't
-- been generated yet.
genFixtureMutation :: [ByteString] -> Gen (ByteString, Mutation)
genFixtureMutation [] =
    pure (BS.empty, TruncateAt 0)
genFixtureMutation corpus = do
    bs <- elements corpus
    let n     = BS.length bs
        clamp = max 0 (n - 1)
    m <- oneof
        [ DropByte    <$> choose (0, clamp)
        , DupByte     <$> choose (0, clamp)
        , SwapBytes   <$> choose (0, max 0 (n - 2))
        , TruncateAt  <$> choose (0, n)
        , InsertByte  <$> choose (0, n) <*> arbitraryByte
        ]
    pure (applyMutation bs m, m)


prop_fixture_mutation_no_crash :: [ByteString] -> Property
prop_fixture_mutation_no_crash corpus =
    forAll (genFixtureMutation corpus) $ \(bs, m) ->
        counterexample ("mutation: " <> show m) $
        ioProperty (checkParseTotality bs)


--------------------------------------------------------------------------------
-- Spec wiring
--------------------------------------------------------------------------------

spec :: Spec
spec = do
    -- Load the fixture corpus once at spec-construction time so
    -- every QuickCheck case picks from the same in-memory list.
    -- Re-reading from disk per case would dominate the runtime.
    corpus <- runIO loadCorpus
    describe "Property — parser totality (Phase 1)" $
        modifyMaxSuccess (const 200) $ do
            prop "random ByteString — Right or Left ParseError"
                prop_random_bytes_no_crash
            prop "fixture mutation — Right or Left ParseError"
                (prop_fixture_mutation_no_crash corpus)


-- | Pre-load every @.hs@ fixture under @test/Fixtures/Coverage/@.
-- Returns @[]@ if the directory is missing — keeps the suite
-- runnable from a fresh checkout where the fixtures haven't been
-- materialised yet.
loadCorpus :: IO [ByteString]
loadCorpus = do
    let dir = "test/Fixtures/Coverage"
    present <- doesDirectoryExist dir
    if not present
        then pure []
        else do
            names <- sort . filter (".hs" `isSuffixOf`) <$> listDirectory dir
            traverse (BS.readFile . (dir </>)) names
