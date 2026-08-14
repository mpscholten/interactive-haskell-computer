-- | Long-lived @ihc daemon@ — keeps the instance manifest and scanned
-- module skeletons warm across @ihc run@ invocations.
--
-- Each CLI @ihc run FILE@ is a new OS process.  Without a daemon that
-- means re-tokenising every library file the program touches.  The
-- daemon is a single process that:
--
--   * forces 'enableKeepModuleCache' so 'LoadedModule' skeletons
--     (headers, data/class/instance scans, type sigs) survive
--     'resetPerRunGlobals';
--   * warms 'manifestIndex' once;
--   * serves one 'runFile' at a time over a Unix socket (IHC's
--     per-run globals are not concurrency-safe).
--
-- @ihc run@ connects if the socket is up, otherwise starts a
-- background daemon and retries.  @IHC_NO_DAEMON=1@ forces the
-- in-process path.
module Daemon
    ( runDaemonForeground
    , runViaDaemonOrLocal
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, fromException, try)
import Control.Monad (unless, void)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Network.Socket
    ( Family(AF_UNIX)
    , SockAddr(SockAddrUnix)
    , Socket
    , SocketType(Stream)
    , accept
    , bind
    , close
    , connect
    , defaultProtocol
    , listen
    , socket
    , socketToHandle
    )
import System.Directory
    ( createDirectoryIfMissing
    , getHomeDirectory
    , removeFile
    )
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO
    ( Handle
    , IOMode(ReadWriteMode)
    , hClose
    , hFlush
    , hPutStrLn
    , stderr
    , stdout
    )
import System.IO.Error (isAlreadyInUseError)
import System.Posix.Files (stdFileMode)
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(WriteOnly)
    , closeFd
    , defaultFileFlags
    , dup
    , dupTo
    , fdToHandle
    , openFd
    , stdError
    , stdOutput
    )
import System.Process
    ( CreateProcess(..)
    , StdStream(NoStream, UseHandle)
    , createProcess
    , proc
    )

import IHC.Driver (runFile)
import IHC.InstanceManifest (manifestIndex, miClassProviders)
import IHC.Scheduler (enableKeepModuleCache)

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

defaultSocketPath :: IO FilePath
defaultSocketPath = do
    m <- lookupEnv "IHC_DAEMON_SOCKET"
    case m of
        Just p | not (null p) -> pure p
        _ -> do
            home <- getHomeDirectory
            let dir = home </> ".cache" </> "ihc"
            createDirectoryIfMissing True dir
            pure (dir </> "daemon.sock")

daemonLogPath :: IO FilePath
daemonLogPath = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "ihc"
    createDirectoryIfMissing True dir
    pure (dir </> "daemon.log")

daemonDisabled :: IO Bool
daemonDisabled = do
    m <- lookupEnv "IHC_NO_DAEMON"
    pure $ case m of
        Just s | not (null s) && s /= "0" -> True
        _ -> False

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- | Bind the Unix socket and serve forever.  If another daemon already
-- owns the socket, exit successfully.
runDaemonForeground :: IO ()
runDaemonForeground = do
    enableKeepModuleCache
    -- Force the instance-manifest CAF (and its on-disk cache) now so
    -- the first client does not pay the boot-library scan.
    let !_warm = Map.size (miClassProviders manifestIndex)
    path <- defaultSocketPath
    createDirectoryIfMissing True (takeDirectory path)
    sock <- socket AF_UNIX Stream defaultProtocol
    bindResult <- try (bind sock (SockAddrUnix path))
                    :: IO (Either SomeException ())
    case bindResult of
        Left err
            | isAddrInUse err -> do
                close sock
                hPutStrLn stderr ("ihc daemon: already running at " <> path)
            | otherwise -> do
                -- Stale socket from a dead daemon: unlink and retry once.
                _ <- try (removeFile path) :: IO (Either SomeException ())
                retry <- try (bind sock (SockAddrUnix path))
                            :: IO (Either SomeException ())
                case retry of
                    Left err2 -> do
                        close sock
                        ioError (userError ("ihc daemon: bind failed: " <> show err2))
                    Right () -> listenAndServe sock path
        Right () -> listenAndServe sock path

listenAndServe :: Socket -> FilePath -> IO ()
listenAndServe sock path = do
    listen sock 16
    hPutStrLn stderr ("ihc daemon: listening on " <> path)
    hFlush stderr
    serveLoop sock `finally` do
        close sock
        void (try (removeFile path) :: IO (Either SomeException ()))

serveLoop :: Socket -> IO ()
serveLoop sock = do
    (conn, _) <- accept sock
    -- One client at a time: per-run IORefs in the scheduler are not
    -- safe to share across concurrent runFile calls.  'socketToHandle'
    -- owns 'conn'; 'handleClient' always closes the Handle.
    _ <- try (handleClient conn) :: IO (Either SomeException ())
    serveLoop sock

handleClient :: Socket -> IO ()
handleClient conn = do
    h <- socketToHandle conn ReadWriteMode
    let go = do
            cmd <- BC.hGetLine h
            if cmd /= BC.pack "RUN"
                then sendResponse h 2 BS.empty (BC.pack "ihc daemon: unknown command")
                else do
                    pathBs <- BC.hGetLine h
                    let path = BC.unpack pathBs
                    (code, out, err) <- captureStdio (runFile path)
                    sendResponse h code out err
    go `finally` hClose h

sendResponse :: Handle -> Int -> ByteString -> ByteString -> IO ()
sendResponse h code out err = do
    BS.hPut h (putWord32 (fromIntegral code))
    BS.hPut h (putWord32 (fromIntegral (BS.length out)))
    BS.hPut h out
    BS.hPut h (putWord32 (fromIntegral (BS.length err)))
    BS.hPut h err
    hFlush h

--------------------------------------------------------------------------------
-- Client
--------------------------------------------------------------------------------

-- | Prefer a running daemon; auto-start one if needed; fall back to
-- in-process 'runFile' when the socket never comes up.
runViaDaemonOrLocal :: FilePath -> IO Int
runViaDaemonOrLocal path = do
    off <- daemonDisabled
    if off
        then runFile path
        else do
            sockPath <- defaultSocketPath
            connected <- tryConnect sockPath
            case connected of
                Just h -> talk h path
                Nothing -> do
                    startBackgroundDaemon
                    waited <- waitForConnect sockPath 50
                    case waited of
                        Just h  -> talk h path
                        Nothing -> runFile path

tryConnect :: FilePath -> IO (Maybe Handle)
tryConnect sockPath = do
    sock <- socket AF_UNIX Stream defaultProtocol
    r <- try (connect sock (SockAddrUnix sockPath))
            :: IO (Either SomeException ())
    case r of
        Left _ -> do
            close sock
            pure Nothing
        Right () -> Just <$> socketToHandle sock ReadWriteMode

waitForConnect :: FilePath -> Int -> IO (Maybe Handle)
waitForConnect _ 0 = pure Nothing
waitForConnect sockPath n = do
    threadDelay 20000  -- 20ms
    m <- tryConnect sockPath
    case m of
        Just h  -> pure (Just h)
        Nothing -> waitForConnect sockPath (n - 1)

talk :: Handle -> FilePath -> IO Int
talk h path =
    (do
        BS.hPut h (BC.pack "RUN\n")
        BS.hPut h (BC.pack path <> BC.pack "\n")
        hFlush h
        code <- fromIntegral <$> hGetWord32 h
        outLen <- fromIntegral <$> hGetWord32 h
        out <- hGetExact h outLen
        errLen <- fromIntegral <$> hGetWord32 h
        err <- hGetExact h errLen
        unless (BS.null out) $ BS.hPut stdout out
        unless (BS.null err) $ BS.hPut stderr err
        hFlush stdout
        hFlush stderr
        pure code)
    `finally` hClose h

startBackgroundDaemon :: IO ()
startBackgroundDaemon = do
    exe <- getExecutablePath
    logPath <- daemonLogPath
    -- Create/append the log so a background child is not silenced.
    let flags = defaultFileFlags
            { append = True
            , creat  = Just stdFileMode
            }
    logFd <- openFd logPath WriteOnly flags
    logH <- fdToHandle logFd
    _ <- createProcess (proc exe ["daemon"])
        { std_in  = NoStream
        , std_out = UseHandle logH
        , std_err = UseHandle logH
        , new_session = True
        }
    -- logH is now owned by the child; do not close it here.
    pure ()

--------------------------------------------------------------------------------
-- stdio capture (server side)
--------------------------------------------------------------------------------

-- | Redirect fd 1/2 to temp files for the duration of 'action', then
-- restore and read the captured bytes.  Interpreted @putStrLn@ writes
-- through the host Handles, so we flush before swapping and after.
captureStdio :: IO Int -> IO (Int, ByteString, ByteString)
captureStdio action = do
    cacheDir <- (</> ".cache" </> "ihc") <$> getHomeDirectory
    createDirectoryIfMissing True cacheDir
    let outPath = cacheDir </> "daemon-last.stdout"
        errPath = cacheDir </> "daemon-last.stderr"
    BS.writeFile outPath BS.empty
    BS.writeFile errPath BS.empty
    hFlush stdout
    hFlush stderr
    savedOut <- dup stdOutput
    savedErr <- dup stdError
    let writeFlags = defaultFileFlags { trunc = True }
    outFd <- openFd outPath WriteOnly writeFlags
    errFd <- openFd errPath WriteOnly writeFlags
    let restore = do
            hFlush stdout
            hFlush stderr
            _ <- dupTo savedOut stdOutput
            _ <- dupTo savedErr stdError
            closeFd outFd
            closeFd errFd
            closeFd savedOut
            closeFd savedErr
    result <- (do
            _ <- dupTo outFd stdOutput
            _ <- dupTo errFd stdError
            action)
        `finally` restore
    out <- BS.readFile outPath
    err <- BS.readFile errPath
    pure (result, out, err)

--------------------------------------------------------------------------------
-- bytes
--------------------------------------------------------------------------------

putWord32 :: Word32 -> ByteString
putWord32 w = BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

hGetWord32 :: Handle -> IO Word32
hGetWord32 h = do
    bs <- hGetExact h 4
    let w =  (fromIntegral (BS.index bs 0) :: Word32) `shiftL` 24
         .|. (fromIntegral (BS.index bs 1) :: Word32) `shiftL` 16
         .|. (fromIntegral (BS.index bs 2) :: Word32) `shiftL` 8
         .|.  fromIntegral (BS.index bs 3)
    pure w

hGetExact :: Handle -> Int -> IO ByteString
hGetExact _ 0 = pure BS.empty
hGetExact h n = do
    chunk <- BS.hGet h n
    if BS.length chunk == n
        then pure chunk
        else if BS.null chunk
            then ioError (userError "ihc daemon: short read")
            else do
                rest <- hGetExact h (n - BS.length chunk)
                pure (chunk <> rest)

isAddrInUse :: SomeException -> Bool
isAddrInUse e =
    case fromException e of
        Just ioe -> isAlreadyInUseError ioe
        Nothing  -> False
