module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment
    (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO
    ( BufferMode(..), hFlush, hGetLine, hIsEOF, hPutStrLn, hSetBuffering
    , stderr, stdout
    )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, getPid
    , getProcessExitCode, proc, waitForProcess
    )
import Test.Hspec
import Text.Read (readMaybe)

import qualified BuiltinsAudit
import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified FreshProcessTest
import qualified HsExtTypeFams
import qualified HsExtMisc
import qualified HsExtRecords
import qualified Hs2010LexNum
import qualified HsExtTypeApps
import qualified HsExtForall
import qualified Hs2010Fixity
import qualified Hs2010Deriving
import qualified HsExtDeriving
import qualified HsExtSyntax
import qualified HsExtClasses
import qualified HsExtKinds
import qualified HsExtPatterns
import qualified Hs2010ClassInst
import qualified Hs2010Types
import qualified HsExtGADTs
import qualified Hs2010Patterns
import qualified Hs2010LexIdent
import qualified Hs2010Bindings
import qualified Hs2010LexComments
import qualified HsExtLiterals
import qualified Hs2010ExprCtl
import qualified Hs2010Modules
import qualified Hs2010LexStr
import qualified Hs2010ExprData
import qualified Hs2010LexLayout
import qualified Hs2010DataDecl
import qualified LexerIhp
import qualified NorthStarTest
import qualified ParserBugs
import qualified PreludeIOOwnerTest
import qualified Properties.DoDesugar
import qualified Properties.RoundTrip
import qualified Properties.SectionDesugar
import qualified Properties.StringDesugar
import qualified Properties.Totality
import qualified Properties.TupleSectionDesugar
import qualified ReplTest
import qualified RunFile
import qualified Unsupported
import qualified NetworkSocketAddrInfoRecordUpdateTest
import qualified TopLevelIOBindingTest
import qualified TopLevelWarpAliasTest
import qualified TypeSchemeParserTest
import qualified WarpHelloTest
import qualified WarpRunStartupTest

-- The ~600-example suite runs in ONE process.
-- 'IHC.Builtins.reapSpawnedThreads' (wired into 'resetPerRunGlobals'
-- and the end of 'runWithSearchPath') is the master-CI OOM fix: it
-- bounds the cross-fixture interpreter-thread STACK growth that
-- heap-exhausted the run.  Independently, a fixture leaves one
-- interpreter thread that never terminates and runs with async
-- exceptions masked, so 'killThread' cannot reap it; at shutdown it
-- busy-spins holding the stdout Handle lock and wedges GHC's RTS
-- shutdown.  The suite prints "603 examples, 0 failures" and then the
-- process hangs forever — a CI *timeout*.  Every in-process escape is
-- itself RTS-scheduled behind the wedge (verified the hard way: -N2,
-- exitImmediately, and a C SIGALRM watchdog were all defeated or
-- unreachable).
--
-- So the binary re-execs itself.  A tiny PARENT process (runs no
-- interpreter code, so nothing can wedge it) runs the suite as a
-- CHILD, forwards the child's output so CI still sees the full report,
-- and reads hspec's own summary line — always printed before the
-- wedge — to learn pass/fail.  It gives the child a short grace to
-- exit on its own (the normal, non-wedged path) then SIGKILLs it, and
-- exits with the correct code.
childEnvVar :: String
childEnvVar = "IHC_TEST_CHILD"

main :: IO ()
main = do
    child <- lookupEnv childEnvVar
    case child of
        Just _  -> hspec allSpecs   -- CHILD: ordinary runner; exits itself unless wedged
        Nothing -> parentMain

parentMain :: IO ()
parentMain = do
    self <- getExecutablePath
    args <- getArgs
    env0 <- getEnvironment
    (_, Just hOut, _, ph) <- createProcess (proc self args)
        { std_out = CreatePipe       -- captured: we tee it + scan the summary
        , std_err = Inherit          -- child stderr -> ours (visible in CI)
        , env     = Just ((childEnvVar, "1") : env0)
        }
    hSetBuffering stdout LineBuffering
    fref  <- newIORef Nothing        -- parsed failure count, once the summary is seen
    armed <- newIORef False          -- grace-then-SIGKILL armed?
    timedOut <- newIORef False
    let killChild = getPid ph >>= maybe (pure ()) (signalProcess sigKILL)
        pump = do
            eof <- hIsEOF hOut       -- blocks until a line or the pipe closes
            if eof then pure () else do
                line <- hGetLine hOut
                putStrLn line
                hFlush stdout
                case parseFailures line of
                    Nothing -> pure ()
                    Just n  -> do
                        writeIORef fref (Just n)
                        a <- readIORef armed
                        when (not a) $ do
                            writeIORef armed True
                            _ <- forkIO $ do
                                threadDelay (15 * 1000 * 1000)
                                alive <- getProcessExitCode ph
                                when (alive == Nothing) killChild
                            pure ()
                pump
    timeoutSeconds <- maybe 1200 id . (>>= readMaybe)
        <$> lookupEnv "IHC_TEST_TIMEOUT_SECONDS"
    _ <- forkIO $ do
        threadDelay (timeoutSeconds * 1000 * 1000)
        alive <- getProcessExitCode ph
        when (alive == Nothing) $ do
            writeIORef timedOut True
            hPutStrLn stderr
                ("ihc-test: suite exceeded " <> show timeoutSeconds
                    <> " seconds; terminating wedged child")
            killChild
    pump                             -- until the child's stdout closes (clean exit or SIGKILL)
    ec <- waitForProcess ph
    mf <- readIORef fref
    didTimeOut <- readIORef timedOut
    exitWith $ if didTimeOut
        then ExitFailure 124
        else case mf of
            Just 0  -> ExitSuccess   -- hspec summary is authoritative
            Just _  -> ExitFailure 1
            Nothing -> case ec of    -- no summary => child crashed; trust its code
                ExitSuccess -> ExitSuccess
                _           -> ExitFailure 1

-- hspec prints e.g. @603 examples, 0 failures, 72 pending@; pull the
-- integer immediately preceding the @failure(s)@ token.
parseFailures :: String -> Maybe Int
parseFailures line =
    case [ ds | (a, b) <- zip ws (drop 1 ws), isFailWord b
              , let ds = takeWhile isDigit a, not (null ds) ] of
        (ds : _) -> Just (read ds)
        _        -> Nothing
  where
    ws = words line
    isFailWord w = w `elem` ["failure", "failures", "failure,", "failures,"]

allSpecs :: Spec
allSpecs = do
    BuiltinsAudit.spec
    ParserBugs.spec
    PreludeIOOwnerTest.spec
    FreshProcessTest.spec
    Properties.Totality.spec
    Properties.RoundTrip.spec
    Properties.SectionDesugar.spec
    Properties.DoDesugar.spec
    Properties.StringDesugar.spec
    Properties.TupleSectionDesugar.spec
    HsExtMisc.spec
    Hs2010LexNum.spec
    Hs2010Fixity.spec
    Hs2010Deriving.spec
    HsExtDeriving.spec
    HsExtSyntax.spec
    HsExtClasses.spec
    HsExtKinds.spec
    HsExtPatterns.spec
    Hs2010ClassInst.spec
    Hs2010Types.spec
    TypeSchemeParserTest.spec
    HsExtGADTs.spec
    Hs2010Patterns.spec
    Hs2010LexIdent.spec
    Hs2010Bindings.spec
    Hs2010LexComments.spec
    HsExtLiterals.spec
    Hs2010ExprCtl.spec
    Hs2010Modules.spec
    Hs2010LexStr.spec
    Hs2010ExprData.spec
    Hs2010LexLayout.spec
    Hs2010DataDecl.spec
    CoreLowerTest.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    HsExtTypeFams.spec
    HsExtTypeApps.spec
    Unsupported.spec
    ReplTest.spec
    NorthStarTest.spec
    LexerIhp.spec
    HsExtRecords.spec
    HsExtForall.spec
    TopLevelIOBindingTest.spec
    TopLevelWarpAliasTest.spec
    WarpRunStartupTest.spec
    NetworkSocketAddrInfoRecordUpdateTest.spec
    WarpHelloTest.spec
