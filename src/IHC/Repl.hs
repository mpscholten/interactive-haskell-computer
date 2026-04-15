-- | Interactive REPL for the ihc interpreter.
--
-- Reads one line at a time (with haskeline for editing + history),
-- dispatches meta-commands (:q, :l, :t, :?) or evaluates the input as
-- a Haskell expression in the current environment.
--
-- The environment accumulates across lines: `let x = ...` bindings
-- persist in subsequent inputs.
module IHC.Repl (runRepl) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

import System.Console.Haskeline
    ( InputT
    , Settings(..)
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )

import IHC.AST (Expr)
import IHC.Builtins (showValWith)
import IHC.Classes (ClassRegistry)
import IHC.Driver (resolveSearchPathFor)
import IHC.Eval (eval)
import IHC.ModuleHeader (ImportDecl(..), parseSingleImport)
import IHC.Parser (defaultFixityTable, parseExprOnly)
import IHC.Scheduler (buildBaseEnv, loadImportIntoEnv, loadProgramFromSource)
import IHC.Source (mkSource, readSourceFile, Source(..), srcBytes)
import IHC.TypeDescribe (describeType)
import IHC.Val

-- | Entry point. Boots the base environment, then runs the REPL.
runRepl :: IO ()
runRepl = do
    (baseEnv, classReg) <- buildBaseEnv
    envRef  <- newIORef baseEnv
    histDir <- historyDir
    let settings = defaultSettings
            { historyFile = Just (histDir </> "repl_history") }
    runInputT settings (loop envRef classReg)

historyDir :: IO FilePath
historyDir = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "ihc"
    createDirectoryIfMissing True dir
    pure dir

--------------------------------------------------------------------------------
-- Main REPL loop
--------------------------------------------------------------------------------

loop :: IORef Env -> ClassRegistry -> InputT IO ()
loop envRef classReg = go
  where
    go = do
        mLine <- getInputLine "ihc> "
        case mLine of
            Nothing   -> pure ()
            Just ""   -> go
            Just line -> do
                continue <- dispatch envRef classReg line
                if continue then go else pure ()

dispatch :: IORef Env -> ClassRegistry -> String -> InputT IO Bool
dispatch envRef classReg line =
    case words line of
        ((':':cmd):rest) -> metaCmd envRef classReg cmd rest
        ("import":_)     -> doImport envRef line >> pure True
        _                -> evalLine envRef classReg line >> pure True

--------------------------------------------------------------------------------
-- Meta-commands
--------------------------------------------------------------------------------

metaCmd :: IORef Env -> ClassRegistry -> String -> [String] -> InputT IO Bool
metaCmd _      _        "q"    _    = pure False
metaCmd _      _        "quit" _    = pure False
metaCmd _      _        "?"    _    = printHelp >> pure True
metaCmd _      _        "help" _    = printHelp >> pure True
metaCmd envRef classReg "l"    args = doLoad envRef classReg (unwords args) >> pure True
metaCmd envRef classReg "load" args = doLoad envRef classReg (unwords args) >> pure True
metaCmd envRef _        "t"    args = doTypeOf envRef (unwords args) >> pure True
metaCmd _      _        cmd    _    = do
    outputStrLn ("Unknown command :" <> cmd <> "  (try :?)")
    pure True

printHelp :: InputT IO ()
printHelp = mapM_ outputStrLn
    [ "ihc REPL commands:"
    , "  :l FILE   (:load)   load a Haskell file into the session"
    , "  :t EXPR   (:type)   show the runtime structural type of an expression"
    , "  :?        (:help)   show this message"
    , "  :q        (:quit)   exit"
    , "  Ctrl-D              exit"
    ]

--------------------------------------------------------------------------------
-- :t EXPR  —  runtime structural type
--------------------------------------------------------------------------------

doTypeOf :: IORef Env -> String -> InputT IO ()
doTypeOf _      ""   = outputStrLn "Usage: :t EXPR"
doTypeOf envRef expr = do
    env <- liftIO (readIORef envRef)
    let src = mkSource "<repl:t>" (BC.pack expr)
    r <- liftIO (try (parseExprOnly src defaultFixityTable)
                    :: IO (Either SomeException Expr))
    case r of
        Left err -> outputStrLn ("Parse error: " <> show err)
        Right ast -> do
            r2 <- liftIO (try (typeOfExpr env ast)
                              :: IO (Either SomeException String))
            case r2 of
                Left  e   -> outputStrLn (expr <> " :: <error: " <> show e <> ">")
                Right ty  -> outputStrLn (expr <> " :: " <> ty)

-- | Evaluate an expression to WHNF and describe its runtime type.
-- IO actions are NOT executed — we stop at the first 'VIO' and report "IO a".
typeOfExpr :: Env -> Expr -> IO String
typeOfExpr env ast = do
    v  <- eval env ast
    ty <- describeType v
    pure (BC.unpack ty)

--------------------------------------------------------------------------------
-- :load FILE
--------------------------------------------------------------------------------

doLoad :: IORef Env -> ClassRegistry -> FilePath -> InputT IO ()
doLoad _      _        ""   = outputStrLn "Usage: :load FILE"
doLoad envRef _classReg path = do
    r <- liftIO (tryLoad path)
    case r of
        Left err      -> outputStrLn ("Load error: " <> err)
        Right newEnv  -> do
            liftIO $ modifyIORef' envRef (\e -> Map.union newEnv e)
            outputStrLn ("Loaded " <> path)

-- | Attempt to load @path@, returning the resulting environment.
-- We append a trivial @main = ()@ if the file has no @main@, so
-- the scheduler doesn't throw.
tryLoad :: FilePath -> IO (Either String Env)
tryLoad path = do
    r <- try (loadFile path) :: IO (Either SomeException Env)
    case r of
        Right env -> pure (Right env)
        Left  e   -> pure (Left (show e))

loadFile :: FilePath -> IO Env
loadFile path = do
    src        <- readSourceFile path
    searchPath <- resolveSearchPathFor path
    r <- try (loadProgramFromSource searchPath src)
            :: IO (Either SomeException (Env, Thunk))
    case r of
        Right (env, _) -> pure env
        Left  _        -> do
            let patched = src { srcBytes = srcBytes src
                                           <> BC.pack "\nmain = ()\n" }
            (env, _) <- loadProgramFromSource searchPath patched
            pure env

--------------------------------------------------------------------------------
-- import MODULE
--------------------------------------------------------------------------------

-- | Handle a line that starts with @import@ typed at the REPL prompt.
-- Parses the import declaration, loads the named module (discovering all
-- its exported bindings), and merges the result into the session env.
doImport :: IORef Env -> String -> InputT IO ()
doImport envRef line = do
    let src = mkSource "<repl>" (BC.pack line)
    mDecl <- liftIO (parseSingleImport src)
    case mDecl of
        Nothing -> outputStrLn ("Import parse error: " <> show line)
        Just imp -> do
            env <- liftIO (readIORef envRef)
            -- Use the current directory as the default search path for
            -- REPL-level imports. The user can :load a file first to
            -- widen the search to that file's directory.
            let searchPath = ["."]
            r <- liftIO (tryImportModule searchPath imp env)
            case r of
                Left err           ->
                    outputStrLn ("Import error: " <> err)
                Right (newEnv, n)  -> do
                    liftIO (writeIORef envRef newEnv)
                    outputStrLn ( "imported "
                                <> BC.unpack (impModule imp)
                                <> " ("
                                <> show n
                                <> " names)" )

tryImportModule
    :: [FilePath]
    -> ImportDecl
    -> Env
    -> IO (Either String (Env, Int))
tryImportModule searchPath imp env = do
    r <- try (loadImportIntoEnv searchPath imp env)
            :: IO (Either SomeException (Env, Int))
    case r of
        Right pair -> pure (Right pair)
        Left  e    -> pure (Left (show e))

--------------------------------------------------------------------------------
-- Expression evaluation
--------------------------------------------------------------------------------

evalLine :: IORef Env -> ClassRegistry -> String -> InputT IO ()
evalLine envRef classReg input = do
    env <- liftIO (readIORef envRef)
    case parseLet input of
        Just (name, rhs) -> do
            r <- liftIO (tryEvalLet env name rhs)
            case r of
                Left  err     -> outputStrLn ("Error: " <> err)
                Right newEnv  -> liftIO (writeIORef envRef newEnv)
        Nothing -> do
            r <- liftIO (tryEvalExpr env classReg input)
            case r of
                Left  err -> outputStrLn ("Error: " <> err)
                Right ()  -> pure ()

-- | Detect `let name = expr` (a standalone let with no `in`).
parseLet :: String -> Maybe (String, String)
parseLet s =
    case words s of
        ("let" : _) ->
            let body = dropPrefix "let" (dropWhile (== ' ') s)
            in case break (== '=') body of
                (lhs, '=' : rhs)
                    | not (null (trim lhs))
                    , '>' `notElem` take 1 (trim rhs)
                    -> Just (trim lhs, trim rhs)
                _ -> Nothing
        _ -> Nothing
  where
    trim       = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    dropPrefix pfx str =
        let n = length pfx
        in if take n str == pfx then drop n str else str

tryEvalLet :: Env -> String -> String -> IO (Either String Env)
tryEvalLet env name rhs = do
    let src = mkSource "<repl>" (BC.pack rhs)
    r <- try (parseExprOnly src defaultFixityTable)
            :: IO (Either SomeException Expr)
    case r of
        Left  err  -> pure (Left (show err))
        Right expr -> do
            slot <- newIORef BlackHole
            let env' = Map.insert (BC.pack name) slot env
            writeIORef slot (Unevaluated (Closure env' expr))
            pure (Right env')

tryEvalExpr :: Env -> ClassRegistry -> String -> IO (Either String ())
tryEvalExpr env classReg input = do
    let src = mkSource "<repl>" (BC.pack input)
    r <- try (parseExprOnly src defaultFixityTable)
            :: IO (Either SomeException Expr)
    case r of
        Left  err  -> pure (Left (show err))
        Right expr -> do
            r2 <- try (evalAndPrint env classReg expr)
                    :: IO (Either SomeException ())
            case r2 of
                Left  e  -> pure (Left (show e))
                Right () -> pure (Right ())

evalAndPrint :: Env -> ClassRegistry -> Expr -> IO ()
evalAndPrint env classReg expr = do
    v <- eval env expr
    case v of
        VIO _ -> do
            v2 <- runIO v
            case v2 of
                VUnit -> pure ()
                other -> printVal classReg other
        VUnit -> pure ()
        other -> printVal classReg other
  where
    runIO (VIO io) = io >>= runIO
    runIO x        = pure x

printVal :: ClassRegistry -> Val -> IO ()
printVal classReg v = do
    s <- showValWith classReg v
    putStrLn s
    hFlush stdout
