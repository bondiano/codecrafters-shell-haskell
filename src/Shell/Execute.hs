module Shell.Execute (
    execute,
) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.Reader
import Shell.Env (Env (..), Shell)
import Shell.Parser (Builtin (..), Command (..), CommandBody (..), Redirect (..), RedirectMode (..), builtinName, parseCommand)
import Shell.Path (getExecutablePathFromPaths)
import System.Directory (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hPutStrLn, openFile, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

execute :: Command -> Shell ()
execute (Command cmdBody redirect) = do
    handle <- liftIO $ openRedirect redirect
    executeBody handle cmdBody
    liftIO $ closeRedirect redirect handle

openRedirect :: Maybe Redirect -> IO Handle
openRedirect Nothing = return stdout
openRedirect (Just (Redirect path Overwrite)) = openFile path WriteMode
openRedirect (Just (Redirect path Append)) = openFile path AppendMode

closeRedirect :: Maybe Redirect -> Handle -> IO ()
closeRedirect Nothing _ = return ()
closeRedirect (Just _) h = hClose h

executeBody :: Handle -> CommandBody -> Shell ()
executeBody _ Empty = return ()
executeBody _ (BuiltinCmd (Type "")) = return ()
executeBody _ (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
executeBody h (BuiltinCmd (Echo str)) = liftIO (hPutStrLn h str)
executeBody h (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . hPutStrLn h
executeBody h (BuiltinCmd PWD) = liftIO $ getCurrentDirectory >>= hPutStrLn h
executeBody _ (BuiltinCmd (CD Nothing)) = liftIO $ putStrLn "cd: missing arguments"
executeBody _ (BuiltinCmd (CD (Just cdDir))) = do
    Env{homeDir = homeDirectory} <- ask
    let dir = resolveHomeDir homeDirectory cdDir
    exists <- liftIO $ doesDirectoryExist dir
    liftIO $
        if exists
            then setCurrentDirectory dir
            else putStrLn $ "cd: " ++ dir ++ ": No such file or directory"
executeBody h (External cmd args) = do
    let p = (proc cmd args){std_out = UseHandle h}
    result <- liftIO $ try $ do
        (_, _, _, ph) <- createProcess p
        void $ waitForProcess ph

    liftIO $ case (result :: Either IOException ()) of
        Left e
            | isDoesNotExistError e -> putStrLn $ cmd ++ ": command not found"
            | isPermissionError e -> putStrLn $ cmd ++ ": permission denied"
            | otherwise -> putStrLn $ cmd ++ ": " ++ show e
        Right () -> return ()

typeOfCommand :: Command -> Shell String
typeOfCommand (Command Empty _) = return ""
typeOfCommand (Command (BuiltinCmd b) _) = return $ builtinName b ++ " is a shell builtin"
typeOfCommand (Command (External cmd _) _) = do
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

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code
