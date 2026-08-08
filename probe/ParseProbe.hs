-- | IHP parser gap analysis probe.
--
-- For each .hs file given as arguments (or on stdin), this program:
--   1. CPP-preprocesses the source (best-effort)
--   2. Scans all top-level binding names via 'scanAllTopLevelNames'
--   3. Attempts to parse each binding's RHS via 'parseBodyExpr'
--   4. ParseError -> records as parse failure
--   5. Other exceptions -> records as "parses/scans OK, fails later"
--
-- Output: tab-separated records + summary table.
module Main (main) where

import Control.Exception (SomeException, fromException, try, evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (group, sort, sortBy, isInfixOf)
import Data.Ord (Down(..), comparing)
import System.IO (hPutStrLn, stderr, hSetBuffering, BufferMode(..), stdout)

import IHC.AST (Expr)
import IHC.Cpp (cppPreprocessWithIncludes, defaultCppContext)
import IHC.Parser (FixityTable, ParseError(..), parseBodyExprWithFixity, defaultFixityTable, scanFixityDecls)
import IHC.Scan
    ( emptyKnownSymbols
    , scanAllTopLevelNames
    , findBinding
    , lhsClauses
    )
import IHC.Source (Source(..), mkSource)

--------------------------------------------------------------------------------
-- Result types
--------------------------------------------------------------------------------

data FileResult = FileResult
    { frFile        :: !FilePath
    , frTotalNames  :: !Int
    , frParseErrors :: ![(ByteString, ParseError)]
    , frOtherErrors :: ![(ByteString, String)]
    , frOK          :: !Int
    }

--------------------------------------------------------------------------------
-- Per-file probe
--------------------------------------------------------------------------------

probeFile :: FilePath -> IO FileResult
probeFile path = do
    raw <- BC.readFile path
    let src0 = mkSource path raw
    -- CPP preprocess (best-effort; on error keep raw source)
    ebs <- try (cppPreprocessWithIncludes [] defaultCppContext path raw)
                :: IO (Either SomeException ByteString)
    let src = case ebs of
                Left _  -> src0
                Right b -> mkSource path b

    -- Scan top-level names
    names <- scanAllTopLevelNames src

    parseErrs <- newIORef []
    otherErrs <- newIORef []
    okCount   <- newIORef (0 :: Int)

    -- Get per-file fixity table
    fx <- scanFixityDecls src defaultFixityTable

    mapM_ (probeBinding src fx parseErrs otherErrs okCount) names

    pe <- readIORef parseErrs
    oe <- readIORef otherErrs
    ok <- readIORef okCount

    pure FileResult
        { frFile        = path
        , frTotalNames  = length names
        , frParseErrors = pe
        , frOtherErrors = oe
        , frOK          = ok
        }

probeBinding
    :: Source
    -> FixityTable
    -> IORef [(ByteString, ParseError)]
    -> IORef [(ByteString, String)]
    -> IORef Int
    -> ByteString
    -> IO ()
probeBinding src fx parseErrs otherErrs okCount name = do
    ks <- emptyKnownSymbols
    mLhs <- findBinding src ks name
    case mLhs of
        Nothing  -> pure ()
        Just lhs -> do
            eResult <- try (evaluate =<< parseBodyExprWithFixity src fx lhs)
                          :: IO (Either SomeException Expr)
            case eResult of
                Right _ -> modifyIORef' okCount (+1)
                Left ex ->
                    case (fromException ex :: Maybe ParseError) of
                        Just pe -> modifyIORef' parseErrs ((name, pe) :)
                        Nothing -> modifyIORef' otherErrs
                                       ((name, showFirstLine (show ex)) :)
  where
    showFirstLine s = take 120 (takeWhile (/= '\n') s)

--------------------------------------------------------------------------------
-- Bucketing
--------------------------------------------------------------------------------

bucketMsg :: String -> String
bucketMsg msg
    | "expected `=` or `|`"       `isInfixOf` msg = "expected = or | at RHS start"
    | "expected `=` after guard"   `isInfixOf` msg = "expected = after guard"
    | "expected identifier"        `isInfixOf` msg = "expected identifier in binding"
    | "expected `=` in binding"    `isInfixOf` msg = "expected = in binding"
    | "empty clause list"          `isInfixOf` msg = "empty clause list"
    | "clauses have differing"     `isInfixOf` msg = "differing arities"
    | "expected `)` or `,`"        `isInfixOf` msg = "tuple/section syntax"
    | "expected `]`"               `isInfixOf` msg = "list expression syntax"
    | "expected `in`"              `isInfixOf` msg = "let-in expression"
    | "expected `of`"              `isInfixOf` msg = "case-of expression"
    | "expected `->`"              `isInfixOf` msg = "lambda/case arrow"
    | "expected `}` or `;`"        `isInfixOf` msg = "record/brace layout"
    | "unexpected"                 `isInfixOf` msg = "unexpected token"
    | otherwise                                    = "other: " <> take 50 msg

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- files passed as CLI args
    files0 <- fmap words getContents
    let files = files0

    hPutStrLn stderr ("ihc-parse-probe: probing " <> show (length files) <> " files")

    results <- mapM (\f -> do
        hPutStrLn stderr ("  " <> f)
        r <- try (probeFile f) :: IO (Either SomeException FileResult)
        pure $ case r of
            Left ex -> FileResult
                { frFile = f, frTotalNames = 0
                , frParseErrors = []
                , frOtherErrors = [("__file__", showFirstLine (show ex))]
                , frOK = 0 }
            Right r' -> r'
        ) files

    let totalFiles   = length results
        filesWithPE  = length (filter (not . null . frParseErrors) results)
        filesClean   = length (filter (\r -> null (frParseErrors r) && frTotalNames r > 0) results)
        totalNames   = sum (map frTotalNames results)
        totalOK      = sum (map frOK results)
        allParseErrs = concatMap (\r ->
            map (\(n,pe) -> (frFile r, n, pe)) (frParseErrors r)) results

    -- Bucket by message
    let buckets = map (\g -> (head g, length g))
                    . group . sort
                    . map (\(_,_,pe) -> bucketMsg (peMsg pe))
                    $ allParseErrs
        sortedBuckets = sortBy (comparing (Down . snd)) buckets

    let filesForBucket bucket =
            length . filter (\r -> any (\(_,pe) -> bucketMsg (peMsg pe) == bucket)
                                       (frParseErrors r)) $ results

    -- Print summary
    putStrLn "=== IHP Parse Probe Results ==="
    putStrLn ""
    putStrLn $ "Files probed      : " <> show totalFiles
    putStrLn $ "Files with errors : " <> show filesWithPE
    putStrLn $ "Files clean       : " <> show filesClean
    putStrLn $ "Total bindings    : " <> show totalNames
    putStrLn $ "Parsed OK         : " <> show totalOK
    putStrLn $ "Parse errors      : " <> show (length allParseErrs)
    putStrLn ""

    putStrLn "=== Error buckets (sorted by frequency) ==="
    putStrLn ""
    let header = padTo 45 "bucket" <> " | " <> padTo 5 "count" <> " | " <> padTo 5 "files" <> " | example"
    putStrLn header
    putStrLn (replicate 100 '-')
    mapM_ (\(bucket, cnt) -> do
        let fa = filesForBucket bucket
        let exs = [ (frFile r, pe)
                  | r <- results
                  , (_, pe) <- frParseErrors r
                  , bucketMsg (peMsg pe) == bucket ]
        let ex = case exs of
                    []          -> ""
                    ((f,pe):_)  -> f <> ":" <> show (peLine pe) <> ":" <> show (peCol pe)
        putStrLn $ padTo 45 bucket <> " | " <> padTo 5 (show cnt) <> " | "
                 <> padTo 5 (show fa) <> " | " <> ex
        ) sortedBuckets

    putStrLn ""
    putStrLn "=== Detailed parse errors (all) ==="
    mapM_ (\(file, name, pe) ->
        putStrLn $ "PARSE_ERR\t" <> file <> "\t" <> BC.unpack name
                 <> "\t" <> show (peLine pe) <> ":" <> show (peCol pe)
                 <> "\t" <> peMsg pe
        ) allParseErrs

    putStrLn ""
    putStrLn "=== Files with most parse errors ==="
    let byErrs = sortBy (comparing (Down . length . frParseErrors)) results
    mapM_ (\r -> do
        let pes = frParseErrors r
        if null pes
            then pure ()
            else do
                putStrLn $ frFile r <> "  (" <> show (length pes) <> " errors)"
                mapM_ (\(nm, pe) ->
                    putStrLn $ "  binding=" <> BC.unpack nm
                             <> " line=" <> show (peLine pe)
                             <> " msg=" <> take 80 (peMsg pe)
                    ) (take 8 pes)
                if length pes > 8
                    then putStrLn ("  ... +" <> show (length pes - 8) <> " more")
                    else pure ()
        ) (take 30 byErrs)

  where
    padTo n s = s <> replicate (max 0 (n - length s)) ' '
    showFirstLine s = take 120 (takeWhile (/= '\n') s)
