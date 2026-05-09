-- | Interactive REPL for the ihc interpreter.
--
-- Reads one line at a time (with haskeline for editing + history),
-- dispatches meta-commands (:q, :l, :t, :?) or evaluates the input as
-- a Haskell expression in the current environment.
--
-- The environment accumulates across lines: `let x = ...` bindings
-- persist in subsequent inputs.
module IHC.Repl (runRepl) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.List (nub)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>), takeDirectory)
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
import IHC.Classes (ClassRegistry, registerInstance, registerInstanceMulti)
import IHC.Classes (legacyHooks)
import IHC.Driver (resolveSearchPathFor)
import IHC.Eval (eval, force, runIOVal)
import IHC.Lexer (nextToken, startCursor, Token(..), TokenKind(..))
import IHC.ModuleHeader (ImportDecl(..), ImportSpec(..), parseSingleImport)
import IHC.Parser (defaultFixityTable, parseExprOnly, parseBodyExprWithFixity)
import IHC.Scan (scanDataDecls, scanInstanceDecls, scanClassDecls, InstanceDecl(..), ClassDecl(..), BindingLhs(..), emptyKnownSymbols, findBinding, FieldRegistry)
import IHC.Scheduler
    ( buildBaseEnv
    , freeVars
    , loadImportIntoEnv
    , loadFileIntoEnv
    , splitQualified
    , classMethodDispatcher
    , defaultTypeTag
    , desugarRecordCons
    , desugarRecordPats
    )
import IHC.Source (mkSource, Source(..))
import IHC.TypeDescribe (describeType)
import IHC.Val

-- | Entry point. Boots the base environment, then runs the REPL.
runRepl :: IO ()
runRepl = do
    (baseEnv, classReg) <- buildBaseEnv
    envRef      <- newIORef baseEnv
    loadedRef   <- newIORef ([] :: [FilePath])
    importsRef  <- newIORef ([] :: [ImportDecl])
    -- Accumulated record-field registry for every `data`/`newtype`
    -- declared at the REPL. Used by 'desugarRecordCons' /
    -- 'desugarRecordPats' so record-construction and record-update
    -- expressions like `x { name = 1 }` evaluate correctly instead of
    -- falling through the ERecordUpdate no-op in Eval.
    fieldRegRef <- newIORef (Map.empty :: FieldRegistry)
    histDir     <- historyDir
    let settings = defaultSettings
            { historyFile = Just (histDir </> "repl_history") }
    runInputT settings (loop envRef classReg loadedRef importsRef fieldRegRef)

historyDir :: IO FilePath
historyDir = do
    home <- getHomeDirectory
    let dir = home </> ".cache" </> "ihc"
    createDirectoryIfMissing True dir
    pure dir

--------------------------------------------------------------------------------
-- Main REPL loop
--------------------------------------------------------------------------------

loop :: IORef Env -> ClassRegistry -> IORef [FilePath] -> IORef [ImportDecl] -> IORef FieldRegistry -> InputT IO ()
loop envRef classReg loadedRef importsRef fieldRegRef = go
  where
    go = do
        mLine <- getInputLine "ihc> "
        case mLine of
            Nothing   -> pure ()
            Just ""   -> go
            Just line -> do
                continue <- dispatch envRef classReg loadedRef importsRef fieldRegRef line
                if continue then go else pure ()

dispatch :: IORef Env -> ClassRegistry -> IORef [FilePath] -> IORef [ImportDecl] -> IORef FieldRegistry -> String -> InputT IO Bool
dispatch envRef classReg loadedRef importsRef fieldRegRef line =
    case words line of
        ((':':cmd):rest) -> metaCmd envRef classReg loadedRef importsRef cmd rest
        ("import":_)     -> doImport envRef loadedRef importsRef line >> pure True
        ("data":_)       -> doDataDecl envRef fieldRegRef line >> pure True
        ("newtype":_)    -> doDataDecl envRef fieldRegRef line >> pure True
        ("type":_)       -> doTypeDecl line >> pure True
        ("class":_)      -> doClassDecl envRef classReg line >> pure True
        ("instance":_)   -> doInstanceDecl envRef classReg line >> pure True
        _                -> evalLine envRef classReg loadedRef importsRef fieldRegRef line >> pure True

--------------------------------------------------------------------------------
-- Meta-commands
--------------------------------------------------------------------------------

metaCmd :: IORef Env -> ClassRegistry -> IORef [FilePath] -> IORef [ImportDecl] -> String -> [String] -> InputT IO Bool
metaCmd _      _        _         _          "q"      _    = pure False
metaCmd _      _        _         _          "quit"   _    = pure False
metaCmd _      _        _         _          "?"      _    = printHelp >> pure True
metaCmd _      _        _         _          "help"   _    = printHelp >> pure True
metaCmd envRef classReg loadedRef _          "l"      args = doLoad envRef classReg loadedRef (unwords args) >> pure True
metaCmd envRef classReg loadedRef _          "load"   args = doLoad envRef classReg loadedRef (unwords args) >> pure True
metaCmd envRef classReg loadedRef _          "r"      _    = doReload envRef classReg loadedRef >> pure True
metaCmd envRef classReg loadedRef _          "reload" _    = doReload envRef classReg loadedRef >> pure True
metaCmd envRef _        loadedRef importsRef "t"      args = doTypeOf envRef loadedRef importsRef (unwords args) >> pure True
metaCmd _      _        _         _          cmd      _    = do
    outputStrLn ("Unknown command :" <> cmd <> "  (try :?)")
    pure True

printHelp :: InputT IO ()
printHelp = mapM_ outputStrLn
    [ "ihc REPL commands:"
    , "  :l FILE   (:load)   load a Haskell file into the session"
    , "  :r        (:reload) re-load all previously loaded files"
    , "  :t EXPR   (:type)   show the runtime structural type of an expression"
    , "  :?        (:help)   show this message"
    , "  :q        (:quit)   exit"
    , "  Ctrl-D              exit"
    ]

--------------------------------------------------------------------------------
-- :t EXPR  —  runtime structural type
--------------------------------------------------------------------------------

doTypeOf :: IORef Env -> IORef [FilePath] -> IORef [ImportDecl] -> String -> InputT IO ()
doTypeOf _      _         _          ""   = outputStrLn "Usage: :t EXPR"
doTypeOf envRef loadedRef importsRef expr = do
    env <- liftIO (readIORef envRef)
    let src = mkSource "<repl:t>" (BC.pack expr)
    r <- liftIO (try (parseExprOnly src defaultFixityTable)
                    :: IO (Either SomeException Expr))
    case r of
        Left err -> outputStrLn ("Parse error: " <> show err)
        Right ast -> do
            env' <- liftIO (ensureQualifiedNamesLoaded loadedRef importsRef env ast)
            r2 <- liftIO (try (typeOfExpr env' ast)
                              :: IO (Either SomeException String))
            case r2 of
                Left  e   -> outputStrLn (expr <> " :: <error: " <> show e <> ">")
                Right ty  -> outputStrLn (expr <> " :: " <> ty)

-- | Evaluate an expression to WHNF and describe its runtime type.
-- IO actions are NOT executed — we stop at the first 'VIO' and report "IO a".
typeOfExpr :: Env -> Expr -> IO String
typeOfExpr env ast = do
    v  <- eval legacyHooks env emptyIPMap ast
    ty <- describeType legacyHooks v
    pure (BC.unpack ty)

--------------------------------------------------------------------------------
-- :load FILE
--------------------------------------------------------------------------------

doLoad :: IORef Env -> ClassRegistry -> IORef [FilePath] -> FilePath -> InputT IO ()
doLoad _      _        _         ""   = outputStrLn "Usage: :load FILE"
doLoad envRef _classReg loadedRef path = do
    env <- liftIO (readIORef envRef)
    r <- liftIO (tryLoadFile path env)
    case r of
        Left err              -> outputStrLn ("Load error: " <> err)
        Right (newEnv, count) -> do
            liftIO $ writeIORef envRef newEnv
            liftIO $ modifyIORef' loadedRef (addUnique path)
            outputStrLn ("Loaded " <> path <> " (" <> show count <> " names)")

-- | Add a path to the list only if it isn't already present.
addUnique :: FilePath -> [FilePath] -> [FilePath]
addUnique p ps
    | p `elem` ps = ps
    | otherwise   = ps <> [p]

-- | `:r` / `:reload` — re-run every previously loaded file in order.
-- Bindings from a fresh load shadow any stale definitions in the env
-- (same behaviour as `:l`).  Prints one "Reloading FILE" line per file.
-- If a file has a parse or load error, the error is reported but the
-- REPL stays alive and the remaining files are still processed.
doReload :: IORef Env -> ClassRegistry -> IORef [FilePath] -> InputT IO ()
doReload envRef _classReg loadedRef = do
    paths <- liftIO (readIORef loadedRef)
    case paths of
        [] -> outputStrLn "No files loaded; nothing to reload."
        _  -> mapM_ reloadOne paths
  where
    reloadOne path = do
        outputStrLn ("Reloading " <> path)
        env <- liftIO (readIORef envRef)
        r <- liftIO (tryLoadFile path env)
        case r of
            Left err              -> outputStrLn ("Load error: " <> err)
            Right (newEnv, count) -> do
                liftIO $ writeIORef envRef newEnv
                outputStrLn ("Loaded " <> path <> " (" <> show count <> " names)")

-- | Attempt to load @path@ into the existing env, returning the updated
-- env and the count of newly-exported names.
tryLoadFile :: FilePath -> Env -> IO (Either String (Env, Int))
tryLoadFile path existingEnv = do
    r <- try (doLoadFile path existingEnv) :: IO (Either SomeException (Env, Int))
    case r of
        Right pair -> pure (Right pair)
        Left  e    -> pure (Left (show e))

-- | Load a .hs file using the REPL-oriented loader that exposes ALL
-- exported names unqualified.  This matches ghci's @:l@ semantics.
doLoadFile :: FilePath -> Env -> IO (Env, Int)
doLoadFile path existingEnv = do
    searchPath <- resolveSearchPathFor path
    loadFileIntoEnv searchPath path existingEnv

--------------------------------------------------------------------------------
-- import MODULE
--------------------------------------------------------------------------------

-- | Handle a line that starts with @import@ typed at the REPL prompt.
-- Parses the import declaration, loads the named module (discovering all
-- its exported bindings), and merges the result into the session env.
doImport :: IORef Env -> IORef [FilePath] -> IORef [ImportDecl] -> String -> InputT IO ()
doImport envRef loadedRef importsRef line = do
    let src = mkSource "<repl>" (BC.pack line)
    mDecl <- liftIO (parseSingleImport src)
    case mDecl of
        Nothing -> outputStrLn ("Import parse error: " <> show line)
        Just imp -> do
            env <- liftIO (readIORef envRef)
            searchPath <- liftIO (currentImportSearchPath loadedRef)
            if shouldDeferImport imp
                then do
                    liftIO $ modifyIORef' importsRef (\imports -> imports <> [imp])
                    outputStrLn ("imported " <> BC.unpack (impModule imp) <> " (deferred)")
                else do
                    r <- liftIO (tryImportModule searchPath imp env)
                    case r of
                        Left err           ->
                            outputStrLn ("Import error: " <> err)
                        Right (newEnv, n)  -> do
                            liftIO (writeIORef envRef newEnv)
                            liftIO $ modifyIORef' importsRef (\imports -> imports <> [imp])
                            outputStrLn ( "imported "
                                        <> BC.unpack (impModule imp)
                                        <> " ("
                                        <> show n
                                        <> " names)" )

-- | Most REPL imports are eager so their unqualified names are available
-- immediately, but @import Prelude@ is special: bulk-loading the whole
-- re-export surface is extremely expensive and makes the prompt look hung.
-- We defer it and let 'ensureQualifiedNamesLoaded' pull in only the names
-- that a later expression actually references.
shouldDeferImport :: ImportDecl -> Bool
shouldDeferImport imp =
    impQualified imp
    || impAlias imp /= Nothing
    || impModule imp == BC.pack "Prelude"

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
doDataDecl :: IORef Env -> IORef FieldRegistry -> String -> InputT IO ()
doDataDecl envRef fieldRegRef line = do
    let src = mkSource "<repl>" (syntheticModule line)
    r <- liftIO (tryDataDecl envRef fieldRegRef src line)
    case r of
        Left err -> outputStrLn ("Error: " <> err)
        Right msg -> outputStrLn msg

tryDataDecl :: IORef Env -> IORef FieldRegistry -> Source -> String -> IO (Either String String)
tryDataDecl envRef fieldRegRef src _line = do
    r <- (try (scanDataDecls src) :: IO (Either SomeException (Map.Map BC.ByteString (BC.ByteString, Int, Int), Map.Map BC.ByteString [(BC.ByteString, Int)], Map.Map BC.ByteString [BC.ByteString])))
    case r of
        Left err  -> pure (Left (show err))
        Right (reg, fldReg, _tReg) ->
            if Map.null reg
            then pure (Left "no constructors found (parse error?)")
            else do
                conEnv   <- buildConEnv  reg
                -- Bind field accessors under both the bare name (for
                -- explicit selector use) AND the synthetic $fldProj$
                -- key so @x.field@ at the REPL desugars correctly.
                -- NoFieldSelectors at the REPL is a no-op — every
                -- declaration there is considered to have selectors
                -- enabled since the REPL has no module-level pragma.
                fieldEnv <- buildFieldEnv fldReg
                let projEnv = HashMap.fromList
                        [ (BC.append (BC.pack "$fldProj$") k, v)
                        | (k, v) <- HashMap.toList fieldEnv ]
                modifyIORef' envRef
                    (HashMap.union projEnv . HashMap.union fieldEnv . HashMap.union conEnv)
                -- Accumulate the field registry so subsequent expressions
                -- can desugar record-construction / record-update / wild
                -- patterns against the types declared at this prompt.
                modifyIORef' fieldRegRef (Map.union fldReg)
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
-- Runs the real 'scanClassDecls' pipeline and:
--
--   * Binds each declared method as a top-level dispatcher in the REPL
--     env via 'classMethodDispatcher' so @foo someVal@ at the prompt
--     resolves to the registered instance for the argument's type.
--   * Parses + evaluates any default-method bodies in the current env
--     and stores them in the 'ClassRegistry' under the '<default>'
--     sentinel tag so dispatch can fall back to them when no instance
--     is registered for a given type.
doClassDecl :: IORef Env -> ClassRegistry -> String -> InputT IO ()
doClassDecl envRef classReg line = do
    let src = mkSource "<repl>" (syntheticModule line)
    r <- liftIO (tryClassDecl envRef classReg src)
    case r of
        Left  err -> outputStrLn ("Error: " <> err)
        Right msg -> outputStrLn msg

tryClassDecl :: IORef Env -> ClassRegistry -> Source -> IO (Either String String)
tryClassDecl envRef classReg src = do
    r <- try (scanClassDecls src) :: IO (Either SomeException [ClassDecl])
    case r of
        Left err -> pure (Left (show err))
        Right [] -> pure (Left "no class declaration found (parse error?)")
        Right (decl : _) -> do
            env <- readIORef envRef
            -- 1. Bind each method name as a dispatcher thunk.
            dispatcherPairs <- mapM (mkDispatcher classReg (classClassName decl))
                                    (classMethodNames decl)
            -- 2. Evaluate default method bodies (if any) in the current
            --    env, producing a slot list keyed by method-name order.
            defaults <- HashMap.fromList <$> mapM (\methodName -> do
                            v <- mkDefault env src decl methodName
                            pure (methodName, v))
                        (classMethodNames decl)
            let existing = [ n | (n, _) <- dispatcherPairs, HashMap.member n env ]
            -- Names already bound in the REPL env (builtins like show/==/
            -- compare or earlier user classes) are NOT overwritten — the
            -- existing dispatcher/implementation stays in charge.
            let newPairs = [ p | p@(n, _) <- dispatcherPairs, not (HashMap.member n env) ]
            writeIORef envRef (HashMap.union (HashMap.fromList newPairs) env)
            -- Register the default-method list under the sentinel tag.
            unless (HashMap.null defaults) $
                registerInstance classReg (classClassName decl)
                                 defaultTypeTag defaults
            let skippedNote
                  | null existing = ""
                  | otherwise = " (skipped " <> show (length existing)
                                <> " existing name(s))"
            pure (Right ( "class " <> BC.unpack (classClassName decl)
                       <> " with " <> show (length (classMethodNames decl))
                       <> " method(s)" <> skippedNote ))

mkDispatcher
    :: ClassRegistry
    -> BC.ByteString
    -> BC.ByteString
    -> IO (BC.ByteString, Thunk)
mkDispatcher classReg cls methodName = do
    let v = classMethodDispatcher classReg cls methodName
    t <- newWHNFThunk v
    pure (methodName, t)

-- | If the class declared a default body for this method, parse and
-- evaluate it in @env@. Otherwise return a placeholder 'Val' that errors
-- only if dispatched to for that slot. This mirrors
-- 'IHC.Scheduler.registerClassDefaults'.
mkDefault :: Env -> Source -> ClassDecl -> BC.ByteString -> IO Val
mkDefault env src decl methodName =
    case Map.lookup methodName (classDefaults decl) of
        Just lhs -> do
            r <- try (do
                        expr <- parseBodyExprWithFixity src defaultFixityTable
                                    (lhsClauses lhs)
                        t <- newThunk env expr
                        force legacyHooks t)
                    :: IO (Either SomeException Val)
            case r of
                Right v -> pure v
                Left  _ -> pure (placeholder (classClassName decl) methodName)
        Nothing ->
            pure (placeholder (classClassName decl) methodName)
  where
    placeholder cls n = VFun $ \_ -> error
        ( "class-method `" <> BC.unpack n
       <> "` of class `"   <> BC.unpack cls
       <> "`: no instance and no default implementation" )

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
        Right (InstanceDecl cls typ typs methods : _) -> do
            env <- readIORef envRef
            r2 <- (try (evalInstanceMethods src env methods)
                    :: IO (Either SomeException (HashMap.HashMap BC.ByteString Val)))
            case r2 of
                Left err        -> pure (Left (show err))
                Right methodMap -> do
                    -- Single-tag registration for the head type (preserves
                    -- the existing single-param class behaviour at the
                    -- REPL prompt).
                    registerInstance classReg cls typ methodMap
                    -- Composite-tag registration for MPTC heads so
                    -- TypeApplications dispatch (@setField \@\"name\"
                    -- \@User \@String@) resolves.
                    registerInstanceMulti classReg cls typs methodMap
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
    -> IO (HashMap.HashMap BC.ByteString Val)
evalInstanceMethods src env methods =
    HashMap.fromList <$> mapM evalOne methods
  where
    evalOne (methodName, lhs) = do
        expr <- parseBodyExprWithFixity src defaultFixityTable (lhsClauses lhs)
        t    <- newThunk env expr
        v    <- force legacyHooks t
        pure (methodName, v)

--------------------------------------------------------------------------------
-- Expression evaluation
--------------------------------------------------------------------------------

evalLine :: IORef Env -> ClassRegistry -> IORef [FilePath] -> IORef [ImportDecl] -> IORef FieldRegistry -> String -> InputT IO ()
evalLine envRef classReg loadedRef importsRef fieldRegRef input = do
    env <- liftIO (readIORef envRef)
    fldReg <- liftIO (readIORef fieldRegRef)
    case sessionBindName input of
        Just (name, rhs) -> do
            r <- liftIO (tryEvalSessionBind loadedRef importsRef fldReg env name rhs)
            case r of
                Left  err    -> outputStrLn ("Error: " <> err)
                Right newEnv -> liftIO (writeIORef envRef newEnv)
        Nothing -> case letBindingName input of
            Just name -> do
                r <- liftIO (tryEvalLetDecl loadedRef importsRef fldReg env name input)
                case r of
                    Left  err     -> outputStrLn ("Error: " <> err)
                    Right newEnv  -> liftIO (writeIORef envRef newEnv)
            Nothing -> do
                r <- liftIO (tryEvalExpr loadedRef importsRef fldReg env classReg input)
                case r of
                    Left  err -> outputStrLn ("Error: " <> err)
                    Right ()  -> pure ()

-- | Detect a ghci-style top-level session bind: @name <- ioExpr@.
-- Returns @Just (name, rhs)@ when the line matches, where @name@ is
-- the (lowercase-starting) identifier on the left of the top-level
-- @<-@ and @rhs@ is the expression text to the right.
--
-- We use the lexer to find the first @<-@ that appears at bracket
-- depth 0 (outside any @()@, @[]@, or @{}@), which correctly skips
-- over @<-@ tokens inside nested @do@-blocks, list comprehensions,
-- etc. The LHS must be exactly one identifier token (we do not yet
-- support destructuring patterns at the REPL).
sessionBindName :: String -> Maybe (String, String)
sessionBindName input =
    case findTopLevelLArrow src of
        Nothing -> Nothing
        Just arrowStart ->
            let lhsTxt = take arrowStart input
                rhsTxt = drop (arrowStart + 2) input  -- skip "<-"
            in case words lhsTxt of
                [nm@(c:_)] | isLowerStart c -> Just (nm, rhsTxt)
                _                           -> Nothing
  where
    src = mkSource "<repl:bind>" (BC.pack input)
    isLowerStart c = (c >= 'a' && c <= 'z') || c == '_'

-- | Walk the token stream of @src@ looking for a 'TkLArrow' at bracket
-- depth 0. Returns its byte offset in the source, or 'Nothing' if no
-- such token exists. Used by 'sessionBindName' to distinguish a
-- top-level @pat <- rhs@ from an @<-@ nested inside @(...)@ or a @do@.
findTopLevelLArrow :: Source -> Maybe Int
findTopLevelLArrow src = go (0 :: Int) startCursor
  where
    go !depth cur =
        let (tok, cur') = nextToken src cur
        in case tkKind tok of
            TkEof                     -> Nothing
            TkLArrow | depth == 0     -> Just (tkStart tok)
            TkLParen                  -> go (depth + 1) cur'
            TkLBracket                -> go (depth + 1) cur'
            TkLBrace                  -> go (depth + 1) cur'
            TkRParen                  -> go (max 0 (depth - 1)) cur'
            TkRBracket                -> go (max 0 (depth - 1)) cur'
            TkRBrace                  -> go (max 0 (depth - 1)) cur'
            _                         -> go depth cur'

-- | Parse @rhs@ as an expression, evaluate it, force the result
-- through 'runIOVal' (so IO actions are executed), and bind the
-- resulting value under @name@ in the session env. If anything
-- throws, the env is left unchanged and the exception is returned
-- as a @Left@ — the caller prints it and the REPL keeps going.
tryEvalSessionBind :: IORef [FilePath] -> IORef [ImportDecl] -> FieldRegistry -> Env -> String -> String -> IO (Either String Env)
tryEvalSessionBind loadedRef importsRef fldReg env name rhs = do
    let src = mkSource "<repl>" (BC.pack rhs)
    r <- try (parseExprOnly src defaultFixityTable)
            :: IO (Either SomeException Expr)
    case r of
        Left  err  -> pure (Left (show err))
        Right expr0 -> do
            let expr = desugarRecordPats fldReg (desugarRecordCons fldReg expr0)
            env' <- ensureQualifiedNamesLoaded loadedRef importsRef env expr
            r2 <- try (do
                        v  <- eval legacyHooks env' emptyIPMap expr
                        runIOVal legacyHooks v)
                    :: IO (Either SomeException Val)
            case r2 of
                Left  e   -> pure (Left (show e))
                Right res -> do
                    slot <- newWHNFThunk res
                    pure (Right (HashMap.insert (BC.pack name) slot env'))

-- | Extract the binding name from a REPL-level @let@ declaration.
-- Returns 'Just name' when the input looks like @let f ...@ with an @=@
-- sign somewhere (i.e. it is a binding, not a @let … in …@ expression).
-- The heuristic: the second word after @let@ must start with a lowercase
-- letter or @_@, there must be an @=@ in the input, and the top-level
-- structure must not include an @in@ keyword at the same nesting depth
-- as the @let@ (that would make it a @let … in@ expression, not a
-- standalone binding).
letBindingName :: String -> Maybe String
letBindingName s =
    case words s of
        ("let" : name@(c:_) : _)
            | '=' `elem` s
            , isLower c || c == '_'
            , not (hasTopLevelIn s)
            -> Just name
        _ -> Nothing
  where
    isLower c = c >= 'a' && c <= 'z'
    -- Check whether the keyword "in" appears at the top level of the
    -- string (depth 0 for parens/brackets/braces), which would mean
    -- this is a "let … in …" expression rather than a standalone binding.
    hasTopLevelIn str = go (0 :: Int) (words (stripLetPrefix str))
    stripLetPrefix str = case words str of
        ("let":rest) -> unwords rest
        _            -> str
    go _     []           = False
    go depth (w : ws)
        | w `elem` ["(","[","{"]  = go (depth + 1) ws
        | w `elem` [")","]","}"]  = go (max 0 (depth - 1)) ws
        | w == "in" && depth == 0 = True
        | otherwise               = go depth ws

-- | Parse and bind a REPL @let@ declaration, using the full Scan +
-- Parser pipeline so that function-style LHSs like @f x y = body@ are
-- correctly desugared into lambdas, and the binding is knot-tied so
-- recursive references inside the body (e.g. @map@ calling @map@) work.
tryEvalLetDecl :: IORef [FilePath] -> IORef [ImportDecl] -> FieldRegistry -> Env -> String -> String -> IO (Either String Env)
tryEvalLetDecl loadedRef importsRef fldReg env name input = do
    -- Strip the leading "let " and wrap in a synthetic module so that
    -- findBinding can locate the top-level declaration at column 1.
    let decl = dropLetPrefix input
        src  = mkSource "<repl>" (syntheticModule decl)
        key  = BC.pack name
    r <- try (parseLetBinding src key)
            :: IO (Either SomeException Expr)
    case r of
        Left  err  -> pure (Left (show err))
        Right expr0 -> do
            let expr = desugarRecordPats fldReg (desugarRecordCons fldReg expr0)
            env0 <- ensureQualifiedNamesLoaded loadedRef importsRef env expr
            -- Knot-tying: allocate a slot first, extend the env to
            -- include the binding, then fill the slot with a closure
            -- that captures the extended env.  This makes the binding
            -- visible to itself (recursion) and to later REPL lines.
            slot <- newIORef (BlackHole "<repl-placeholder>")
            let env' = HashMap.insert key slot env0
            writeIORef slot (Unevaluated (Closure env' emptyIPMap expr))
            pure (Right env')

dropLetPrefix :: String -> String
dropLetPrefix s =
    let s1 = dropWhile (== ' ') s
    in if take 3 s1 == "let"
       then dropWhile (== ' ') (drop 3 s1)
       else s1

-- | Use the Scan + Parser pipeline to parse a top-level binding from a
-- synthetic module source.  Returns the desugared 'Expr' (parameters
-- become nested lambdas via 'parseBodyExprWithFixity').
parseLetBinding :: Source -> BC.ByteString -> IO Expr
parseLetBinding src name = do
    known <- emptyKnownSymbols
    mLhs  <- findBinding src known name
    case mLhs of
        Nothing  -> throwIO (userError ("let: cannot parse binding for `"
                                         <> BC.unpack name <> "`"))
        Just lhs -> parseBodyExprWithFixity src defaultFixityTable (lhsClauses lhs)

tryEvalExpr :: IORef [FilePath] -> IORef [ImportDecl] -> FieldRegistry -> Env -> ClassRegistry -> String -> IO (Either String ())
tryEvalExpr loadedRef importsRef fldReg env classReg input = do
    let src = mkSource "<repl>" (BC.pack input)
    r <- try (parseExprOnly src defaultFixityTable)
            :: IO (Either SomeException Expr)
    case r of
        Left  err  -> pure (Left (show err))
        Right expr0 -> do
            let expr = desugarRecordPats fldReg (desugarRecordCons fldReg expr0)
            env' <- ensureQualifiedNamesLoaded loadedRef importsRef env expr
            r2 <- try (evalAndPrint env' classReg expr)
                    :: IO (Either SomeException ())
            case r2 of
                Left  e  -> pure (Left (show e))
                Right () -> pure (Right ())

currentImportSearchPath :: IORef [FilePath] -> IO [FilePath]
currentImportSearchPath loadedRef = do
    loaded <- readIORef loadedRef
    pure (nub ("." : map takeDirectory loaded))

ensureQualifiedNamesLoaded :: IORef [FilePath] -> IORef [ImportDecl] -> Env -> Expr -> IO Env
ensureQualifiedNamesLoaded loadedRef importsRef env expr = do
    imports <- readIORef importsRef
    if null imports
        then pure env
        else do
            searchPath <- currentImportSearchPath loadedRef
            let requested =
                    foldr addRequest []
                        (qualifiedRequests imports ++ unqualifiedRequests imports)
            foldM (loadOne searchPath) env requested
  where
    qualifiedRequests imports =
        [ (imp, bare)
        | fv <- freeVars expr
        , Just (qual, bare) <- [splitQualified fv]
        , imp <- matchingQualifiedImports qual imports
        ]

    unqualifiedRequests imports =
        [ (imp, fv)
        | fv <- freeVars expr
        , splitQualified fv == Nothing
        , not (HashMap.member fv env)
        , imp <- matchingUnqualifiedImports fv imports
        ]

    matchingQualifiedImports qual = filter (\imp ->
        case impAlias imp of
            Just a  -> a == qual
            Nothing -> impQualified imp && impModule imp == qual)

    matchingUnqualifiedImports name = filter (\imp ->
        not (impQualified imp)
        && impAlias imp == Nothing
        && specAllowsImport imp name)

    addRequest (imp, bare) [] = [(imp, [bare])]
    addRequest (imp, bare) ((imp', names) : rest)
        | sameImport imp imp' = (imp', bare : names) : rest
        | otherwise           = (imp', names) : addRequest (imp, bare) rest

    sameImport a b =
        impModule a == impModule b
        && impQualified a == impQualified b
        && impAlias a == impAlias b
        && impSpec a == impSpec b

    specAllowsImport imp = specAllowsName (impSpec imp)

    specAllowsName ImportAll         _    = True
    specAllowsName (ImportOnly ns)   name = name `elem` ns
    specAllowsName (ImportHiding ns) name = name `notElem` ns

    loadOne searchPath accEnv (imp, names) = do
        let imp' = imp { impSpec = ImportOnly (nub names) }
        (env', _) <- loadImportIntoEnv searchPath imp' accEnv
        pure env'

evalAndPrint :: Env -> ClassRegistry -> Expr -> IO ()
evalAndPrint env classReg expr = do
    v <- eval legacyHooks env emptyIPMap expr
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
