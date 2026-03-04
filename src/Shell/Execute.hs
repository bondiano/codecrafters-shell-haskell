module Shell.Execute (
    execute,
) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.Reader
import Shell.Env (Env (..), Shell)
import Shell.Parser (Builtin (..), Command (..), builtinName, parseCommand)
import Shell.Path (getExecutablePathFromPaths)
import System.Directory (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process (createProcess, proc, waitForProcess)

execute :: Command -> Shell ()
execute Empty = return ()
execute (BuiltinCmd (Type "")) = return ()
execute (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
execute (BuiltinCmd (Echo str)) = liftIO (putStrLn str)
execute (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . putStrLn
execute (BuiltinCmd PWD) = liftIO $ getCurrentDirectory >>= putStrLn
execute (BuiltinCmd (CD Nothing)) = liftIO $ putStrLn "cd: missing arguments"
execute (BuiltinCmd (CD (Just cdDir))) = do
    Env{homeDir = homeDirectory} <- ask
    let dir = resolveHomeDir homeDirectory cdDir
    exists <- liftIO $ doesDirectoryExist dir
    liftIO $
        if exists
            then setCurrentDirectory dir
            else putStrLn $ "cd: " ++ dir ++ ": No such file or directory"
execute (External cmd args) = do
    result <- liftIO $ try $ do
        (_, _, _, ph) <- createProcess (proc cmd args)
        void $ waitForProcess ph

    liftIO $ case (result :: Either IOException ()) of
        Left e
            | isDoesNotExistError e -> putStrLn $ cmd ++ ": command not found"
            | isPermissionError e -> putStrLn $ cmd ++ ": permission denied"
            | otherwise -> putStrLn $ cmd ++ ": " ++ show e
        Right () -> return ()

typeOfCommand :: Command -> Shell String
typeOfCommand Empty = return ""
typeOfCommand (BuiltinCmd b) = return $ builtinName b ++ " is a shell builtin"
typeOfCommand (External cmd _) = do
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
