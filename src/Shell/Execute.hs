{-# LANGUAGE ForeignFunctionInterface #-}

module Shell.Execute (
    execute,
    executeBackground,
    executePipeline,
) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Shell.Env (Env (..), Shell (..), addHistory, getHistory, getUnsavedHistory, markHistorySaved, nextJobNumber, saveHistory)
import Shell.Parser (Builtin (..), Command (..), CommandBody (..), HistoryAction (..), Redirect (..), RedirectMode (..), builtinName, parseCommand)
import Shell.Path (getExecutablePathFromPaths)
import System.Directory (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, hPutStrLn, openFile, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createPipe, createProcess, proc, waitForProcess)

foreign import ccall "fork" c_fork :: IO CInt
foreign import ccall "execvp" c_execvp :: CString -> Ptr CString -> IO CInt
foreign import ccall "_exit" c_exit :: CInt -> IO ()

execute :: Command -> Shell ()
execute Command{body = cmdBody, stdoutRedirect = stdoutR, stderrRedirect = stderrR} = do
    stdoutHandle <- liftIO $ openRedirect stdout stdoutR
    stderrHandle <- liftIO $ openRedirect stderr stderrR
    executeBody stdoutHandle stderrHandle cmdBody
        `finally'` do
            liftIO $ closeRedirect stdoutR stdoutHandle
            liftIO $ closeRedirect stderrR stderrHandle

executeBackground :: Command -> Shell ()
executeBackground Command{body = External cmd args} = do
    jobNum <- nextJobNumber
    pid <- liftIO $ spawnBackground cmd args
    liftIO $ case pid of
        (-1) -> hPutStrLn stderr $ cmd ++ ": fork failed"
        _ -> do
            putStrLn $ "[" ++ show jobNum ++ "] " ++ show (fromIntegral pid :: Int)
            hFlush stdout
executeBackground cmd = execute cmd

spawnBackground :: String -> [String] -> IO CInt
spawnBackground cmd args = do
    cCmd <- newCString cmd
    cArgs <- mapM newCString (cmd : args)
    withArray0 nullPtr cArgs $ \argsPtr -> do
        pid <- c_fork
        case pid of
            0 -> do
                -- Child process: exec immediately
                _ <- c_execvp cCmd argsPtr
                -- If exec fails, exit child
                c_exit 127
                return 0
            _ -> return pid

finally' :: Shell a -> Shell b -> Shell a
finally' action cleanup = do
    result <- action
    _ <- cleanup
    pure result

openRedirect :: Handle -> Maybe Redirect -> IO Handle
openRedirect fallback Nothing = pure fallback
openRedirect _ (Just (Redirect path Overwrite)) = openFile path WriteMode
openRedirect _ (Just (Redirect path Append)) = openFile path AppendMode

closeRedirect :: Maybe Redirect -> Handle -> IO ()
closeRedirect Nothing _ = pure ()
closeRedirect (Just _) h = hClose h

executeBody :: Handle -> Handle -> CommandBody -> Shell ()
executeBody _ _ Empty = return ()
executeBody _ _ (BuiltinCmd (Type "")) = return ()
executeBody _ _ (BuiltinCmd Jobs) = return ()
executeBody _ _ (BuiltinCmd (Exit code)) = saveHistory >> liftIO (exitWith $ toExitCode code)
executeBody h _ (BuiltinCmd (Echo str)) = liftIO (hPutStrLn h str)
executeBody h _ (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . hPutStrLn h
executeBody h _ (BuiltinCmd PWD) = liftIO $ getCurrentDirectory >>= hPutStrLn h
executeBody _ _ (BuiltinCmd (History (ReadHistory path))) = do
    content <- liftIO $ readFile path
    mapM_ addHistory $ filter (not . null) $ lines content
    markHistorySaved
executeBody _ _ (BuiltinCmd (History (WriteHistory path))) = do
    entries <- getHistory
    liftIO $ writeFile path $ unlines entries
executeBody _ _ (BuiltinCmd (History (AppendHistory path))) = do
    unsaved <- getUnsavedHistory
    liftIO $ appendFile path $ unlines unsaved
    markHistorySaved
executeBody h _ (BuiltinCmd (History (ShowHistory mCount))) = do
    entries <- getHistory
    let numbered = zip [1 :: Int ..] entries
        visible = maybe numbered (\n -> drop (length numbered - n) numbered) mCount
        formatted = map (\(i, cmd) -> "    " ++ show i ++ "  " ++ cmd) visible
    liftIO $ mapM_ (hPutStrLn h) formatted
executeBody _ eh (BuiltinCmd (CD Nothing)) = liftIO $ hPutStrLn eh "cd: missing arguments"
executeBody _ eh (BuiltinCmd (CD (Just cdDir))) = do
    Env{homeDir = homeDirectory} <- ask
    let dir = resolveHomeDir homeDirectory cdDir
    exists <- liftIO $ doesDirectoryExist dir
    liftIO $
        if exists
            then setCurrentDirectory dir
            else hPutStrLn eh $ "cd: " ++ dir ++ ": No such file or directory"
executeBody h eh (External cmd args) = do
    let p = (proc cmd args){std_out = UseHandle h, std_err = UseHandle eh}
    result <- liftIO $ try $ do
        (_, _, _, ph) <- createProcess p
        void $ waitForProcess ph

    liftIO $ case (result :: Either IOException ()) of
        Left e
            | isDoesNotExistError e -> hPutStrLn eh $ cmd ++ ": command not found"
            | isPermissionError e -> hPutStrLn eh $ cmd ++ ": permission denied"
            | otherwise -> hPutStrLn eh $ cmd ++ ": " ++ show e
        Right () -> return ()

typeOfCommand :: Command -> Shell String
typeOfCommand (Command Empty _ _) = return ""
typeOfCommand (Command (BuiltinCmd b) _ _) = return $ builtinName b ++ " is a shell builtin"
typeOfCommand (Command (External cmd _) _ _) = do
    env <- ask
    mbPath <- liftIO $ getExecutablePathFromPaths (envPaths env) cmd
    return $ case mbPath of
        Just path -> cmd ++ " is " ++ path
        Nothing -> cmd ++ ": not found"

resolveHomeDir :: FilePath -> FilePath -> FilePath
resolveHomeDir homeDirectory path = case path of
    "~" -> homeDirectory
    "~/" -> homeDirectory
    '~' : rest -> homeDirectory </> rest
    _ -> path

executePipeline :: [Command] -> Shell ()
executePipeline [] = pure ()
executePipeline [cmd] = execute cmd
executePipeline cmds = do
    env <- ask
    liftIO $ do
        phs <- launchChain env Nothing cmds
        mapM_ waitForProcess phs

launchChain :: Env -> Maybe Handle -> [Command] -> IO [ProcessHandle]
launchChain _ _ [] = pure []
-- Last command in pipeline
launchChain env mIn [cmd] = case body cmd of
    External c as -> do
        outH <- openRedirect stdout (stdoutRedirect cmd)
        errH <- openRedirect stderr (stderrRedirect cmd)
        let p = (proc c as){std_in = maybe Inherit UseHandle mIn, std_out = UseHandle outH, std_err = UseHandle errH}
        (_, _, _, ph) <- createProcess p
        mapM_ hClose mIn
        pure [ph]
    cmdBody -> do
        outH <- openRedirect stdout (stdoutRedirect cmd)
        errH <- openRedirect stderr (stderrRedirect cmd)
        runReaderT (runShell $ executeBody outH errH cmdBody) env
        closeRedirect (stdoutRedirect cmd) outH
        closeRedirect (stderrRedirect cmd) errH
        mapM_ hClose mIn
        pure []
-- Non-last command in pipeline
launchChain env mIn (cmd : rest) = case body cmd of
    External c as -> do
        errH <- openRedirect stderr (stderrRedirect cmd)
        let p = (proc c as){std_in = maybe Inherit UseHandle mIn, std_out = CreatePipe, std_err = UseHandle errH}
        (_, mPipe, _, ph) <- createProcess p
        mapM_ hClose mIn
        pipeOut <- maybe (fail "expected pipe handle") pure mPipe
        phs <- launchChain env (Just pipeOut) rest
        pure (ph : phs)
    cmdBody -> do
        (pipeRead, pipeWrite) <- createPipe
        errH <- openRedirect stderr (stderrRedirect cmd)
        runReaderT (runShell $ executeBody pipeWrite errH cmdBody) env
        hClose pipeWrite
        closeRedirect (stderrRedirect cmd) errH
        mapM_ hClose mIn
        launchChain env (Just pipeRead) rest

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code
