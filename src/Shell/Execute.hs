module Shell.Execute (
    execute,
    executePipeline,
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, finally, try)
import Control.Monad (void)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Shell.Env (Env (..), Shell (..))
import Shell.Parser (Builtin (..), Command (..), CommandBody (..), Redirect (..), RedirectMode (..), builtinName, parseCommand)
import Shell.Path (getExecutablePathFromPaths)
import System.Directory (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hPutStrLn, openFile, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createPipe, createProcess, proc, waitForProcess)

execute :: Command -> Shell ()
execute Command{body = cmdBody, stdoutRedirect = stdoutR, stderrRedirect = stderrR} = do
    stdoutHandle <- liftIO $ openRedirect stdout stdoutR
    stderrHandle <- liftIO $ openRedirect stderr stderrR
    executeBody stdoutHandle stderrHandle cmdBody
        `finally'` do
            liftIO $ closeRedirect stdoutR stdoutHandle
            liftIO $ closeRedirect stderrR stderrHandle

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
executeBody _ _ (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
executeBody h _ (BuiltinCmd (Echo str)) = liftIO (hPutStrLn h str)
executeBody h _ (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . hPutStrLn h
executeBody h _ (BuiltinCmd PWD) = liftIO $ getCurrentDirectory >>= hPutStrLn h
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

data Waitable = WaitProcess ProcessHandle | WaitThread (MVar ())

waitFor :: Waitable -> IO ()
waitFor (WaitProcess ph) = void $ waitForProcess ph
waitFor (WaitThread mv) = takeMVar mv

executePipeline :: [Command] -> Shell ()
executePipeline [] = pure ()
executePipeline [cmd] = execute cmd
executePipeline cmds = do
    env <- ask
    liftIO $ do
        ws <- launchChain env Nothing cmds
        mapM_ waitFor ws

launchChain :: Env -> Maybe Handle -> [Command] -> IO [Waitable]
launchChain _ _ [] = pure []
-- Last command in pipeline
launchChain env mIn [cmd] = case body cmd of
    External c as -> do
        outH <- openRedirect stdout (stdoutRedirect cmd)
        errH <- openRedirect stderr (stderrRedirect cmd)
        let p = (proc c as){std_in = maybe Inherit UseHandle mIn, std_out = UseHandle outH, std_err = UseHandle errH}
        (_, _, _, ph) <- createProcess p
        mapM_ hClose mIn
        pure [WaitProcess ph]
    cmdBody -> do
        outH <- openRedirect stdout (stdoutRedirect cmd)
        errH <- openRedirect stderr (stderrRedirect cmd)
        let toClose = [outH | Just _ <- [stdoutRedirect cmd]] ++ [errH | Just _ <- [stderrRedirect cmd]]
        w <- forkBuiltin env cmdBody outH errH toClose
        mapM_ hClose mIn
        pure [w]
-- Non-last command in pipeline
launchChain env mIn (cmd : rest) = case body cmd of
    External c as -> do
        errH <- openRedirect stderr (stderrRedirect cmd)
        let p = (proc c as){std_in = maybe Inherit UseHandle mIn, std_out = CreatePipe, std_err = UseHandle errH}
        (_, mPipe, _, ph) <- createProcess p
        mapM_ hClose mIn
        pipeOut <- maybe (fail "expected pipe handle") pure mPipe
        ws <- launchChain env (Just pipeOut) rest
        pure (WaitProcess ph : ws)
    cmdBody -> do
        (pipeRead, pipeWrite) <- createPipe
        errH <- openRedirect stderr (stderrRedirect cmd)
        let toClose = pipeWrite : [errH | Just _ <- [stderrRedirect cmd]]
        w <- forkBuiltin env cmdBody pipeWrite errH toClose
        mapM_ hClose mIn
        ws <- launchChain env (Just pipeRead) rest
        pure (w : ws)

forkBuiltin :: Env -> CommandBody -> Handle -> Handle -> [Handle] -> IO Waitable
forkBuiltin env cmdBody outH errH toClose = do
    done <- newEmptyMVar
    _ <-
        forkIO $
            runReaderT (runShell $ executeBody outH errH cmdBody) env
                `finally` do
                    mapM_ hClose toClose
                    putMVar done ()
    pure (WaitThread done)

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code
