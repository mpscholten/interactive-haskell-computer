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
import IHC.Builtins (showValWith, buildConEnv, buildFieldEnv)
import IHC.Classes (ClassRegistry, registerInstance)
import IHC.Driver (resolveSearchPathFor)
import IHC.Eval (eval, force)
import IHC.Lexer (nextToken, startCursor, Token(..), TokenKind(..))
import IHC.ModuleHeader (ImportDecl(..), parseSingleImport)
import IHC.Parser (defaultFixityTable, parseExprOnly, parseBodyExprWithFixity)
import IHC.Scan (scanDataDecls, scanInstanceDecls, InstanceDecl(..), BindingLhs(..))
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
        ("data":_)       -> doDataDecl envRef line >> pure True
        ("newtype":_)    -> doDataDecl envRef line >> pure True
        ("type":_)       -> doTypeDecl line >> pure True
        ("class":_)      -> doClassDecl line >> pure True
        ("instance":_)   -> doInstanceDecl envRef classReg line >> pure True
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
-- Top-level declaration handlers
--------------------------------------------------------------------------------

-- | Synthetic-module wrapper used to reuse existing scan machinery on a
-- single declaration line typed at the REPL prompt.
syntheticModule :: String -> BC.ByteString
syntheticModule decl = BC.pack ("module IhcRepl where\n" <> decl <> "\n")

-- | Handle @data T = A | B Int@ and @newtype T = T Int@ at the REPL.
-- We wrap the declaration in a synthetic module, run 'scanDataDecls' to
-- extract (constructor, arity) pairs, build VFun/VCon thunks via
-- 'buildConEnv', and merge the result into the REPL env.
doDataDecl :: IORef Env -> String -> InputT IO ()
doDataDecl envRef line = do
    let src = mkSource "<repl>" (syntheticModule line)
    r <- liftIO (tryDataDecl envRef src line)
    case r of
        Left err -> outputStrLn ("Error: " <> err)
        Right msg -> outputStrLn msg

tryDataDecl :: IORef Env -> Source -> String -> IO (Either String String)
tryDataDecl envRef src _line = do
    r <- (try (scanDataDecls src) :: IO (Either SomeException (Map.Map BC.ByteString Int, Map.Map BC.ByteString [(BC.ByteString, Int)])))
    case r of
        Left err  -> pure (Left (show err))
        Right (reg, fldReg) ->
            if Map.null reg
            then pure (Left "no constructors found (parse error?)")
            else do
                conEnv   <- buildConEnv  reg
                fieldEnv <- buildFieldEnv fldReg
                modifyIORef' envRef (Map.union fieldEnv . Map.union conEnv)
                let ctorCount = Map.size reg
                    ctors     = Map.keys reg
                    msg = "data decl: " <> show ctorCount <>
                          " constructor(s): " <>
                          unwords (map BC.unpack ctors)
                pure (Right msg)

-- | Handle @type Foo = Int@ at the REPL.  ihc has no type checker so we
-- parse-and-discard type synonyms, printing a short note.
doTypeDecl :: String -> InputT IO ()
doTypeDecl line =
    -- Extract the type name (second word after "type")
    case words line of
        ("type" : name : _) ->
            outputStrLn ("type " <> name <> " (type synonyms are not checked in ihc)")
        _ ->
            outputStrLn "type synonym (not checked in ihc)"

-- | Handle @class C a where foo :: a -> a@ at the REPL.
-- We do a lightweight scan to count the method signatures/defaults declared
-- in the @where@ block, then register the class name in the output.
-- No ClassRegistry entry is created — dispatch is resolved at instance time.
doClassDecl :: String -> InputT IO ()
doClassDecl line = do
    let src     = mkSource "<repl>" (syntheticModule line)
        methods = countClassMethods src
        name    = extractClassName line
        msg     = "class " <> name <> " with " <> show methods <> " method(s)"
    outputStrLn msg

-- | Count method declarations in a @class … where@ block by scanning
-- for indented (col > 1) identifier tokens that are followed eventually
-- by @::@ or @=@ on the same or continuation line.
-- Simplified heuristic: count distinct indented idents before @::@.
countClassMethods :: Source -> Int
countClassMethods src = go 0 False startCursor
  where
    go !n inWhere cur =
        let (tok, cur') = nextToken src cur
        in case tkKind tok of
            TkEof    -> n
            TkWhere  -> go n True cur'
            TkNewline -> go n inWhere cur'
            TkIdent _
                | inWhere && tkCol tok > 1 ->
                    -- Count this as a method name; skip to end of this line.
                    go (n + 1) inWhere (skipToLineEnd src cur')
            _ -> go n inWhere cur'

    skipToLineEnd s c =
        let (tok, c') = nextToken s c
        in case tkKind tok of
            TkEof     -> c
            TkNewline -> c'
            _         -> skipToLineEnd s c'

-- | Extract the class name (first uppercase identifier after @class@).
extractClassName :: String -> String
extractClassName line =
    case dropWhile (/= ' ') line of
        []   -> "?"
        rest -> case dropWhile (== ' ') rest of
            []   -> "?"
            rest2 -> takeWhile (\c -> c /= ' ' && c /= '\n') rest2

-- | Handle @instance C Int where foo x = x + 1@ at the REPL.
-- Wraps the declaration in a synthetic module, scans with
-- 'scanInstanceDecls', evaluates each method body in the current env,
-- and registers the dictionary in the ClassRegistry.
doInstanceDecl :: IORef Env -> ClassRegistry -> String -> InputT IO ()
doInstanceDecl envRef classReg line = do
    r <- liftIO (tryInstanceDecl envRef classReg line)
    case r of
        Left err -> outputStrLn ("Error: " <> err)
        Right msg -> outputStrLn msg

tryInstanceDecl :: IORef Env -> ClassRegistry -> String -> IO (Either String String)
tryInstanceDecl envRef classReg line = do
    let src = mkSource "<repl>" (syntheticModule line)
    r <- (try (scanInstanceDecls src) :: IO (Either SomeException [InstanceDecl]))
    case r of
        Left err -> pure (Left (show err))
        Right [] -> pure (Left "no instance declaration found (parse error?)")
        Right (InstanceDecl cls typ methods : _) -> do
            env <- readIORef envRef
            r2 <- (try (evalInstanceMethods src env methods)
                    :: IO (Either SomeException [Val]))
            case r2 of
                Left err     -> pure (Left (show err))
                Right vals   -> do
                    registerInstance classReg cls typ vals
                    let n   = length methods
                        msg = "instance " <> BC.unpack cls <>
                              " " <> BC.unpack typ <>
                              " with " <> show n <> " method(s)"
                    pure (Right msg)

-- | Parse and evaluate each method body in the given env.
evalInstanceMethods
    :: Source
    -> Env
    -> [(BC.ByteString, BindingLhs)]
    -> IO [Val]
evalInstanceMethods src env methods =
    mapM evalOne methods
  where
    evalOne (_, lhs) = do
        expr <- parseBodyExprWithFixity src defaultFixityTable (lhsClauses lhs)
        t    <- newThunk env expr
        force t

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
