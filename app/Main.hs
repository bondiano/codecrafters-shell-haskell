module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void)
import Control.Monad.ListM (findM)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, executable, getPermissions, getCurrentDirectory, setCurrentDirectory, doesDirectoryExist)
import System.Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath (splitSearchPath, (</>))
import System.IO (hFlush, isEOF, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Process (createProcess, proc, waitForProcess)
import Text.Read (readMaybe)

data Builtin = Exit Int | Echo String | Type String | PWD | CD String

data Command = BuiltinCmd Builtin | External String [String] | Empty

newtype Env = Env {envPaths :: [FilePath]}

type Shell = ReaderT Env IO

builtinName :: Builtin -> String
builtinName (Exit _) = "exit"
builtinName (Echo _) = "echo"
builtinName (Type _) = "type"
builtinName PWD = "pwd"
builtinName (CD _) = "cd"

buildEnv :: IO Env
buildEnv = Env <$> buildEnvPath

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"

parseCommand :: String -> Command
parseCommand input = case words input of
    ("exit" : args) -> BuiltinCmd $ Exit $ parseExitCode args
    ("echo" : args) -> BuiltinCmd $ Echo $ unwords args
    ("type" : args) -> BuiltinCmd $ Type $ unwords args
    ("pwd" : _) -> BuiltinCmd PWD
    ("cd" : args) -> BuiltinCmd $ CD $ unwords args
    (cmd : args) -> External cmd args
    [] -> Empty

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)

typeOfCommand :: Command -> Shell String
typeOfCommand Empty = pure ""
typeOfCommand (BuiltinCmd b) = pure $ builtinName b ++ " is a shell builtin"
typeOfCommand (External cmd _) = do
    env <- ask
    mbPath <- liftIO $ getExecutablePathFromPaths (envPaths env) cmd

    return $ case mbPath of
        Just path -> cmd ++ " is " ++ path
        Nothing -> cmd ++ ": not found"

getExecutablePathFromPaths :: [FilePath] -> String -> IO (Maybe String)
getExecutablePathFromPaths dirs cmd =
    fmap (</> cmd) <$> findM (\d -> isExecutable (d </> cmd)) dirs

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    exists <- doesFileExist path
    if exists then executable <$> getPermissions path else pure False

execute :: Command -> Shell ()
execute Empty = pure ()
execute (BuiltinCmd (Type "")) = pure ()
execute (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
execute (BuiltinCmd (Echo str)) = liftIO (putStrLn str)
execute (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . putStrLn
execute (BuiltinCmd PWD) = liftIO $ getCurrentDirectory >>= putStrLn
execute (BuiltinCmd (CD "")) = liftIO $ putStrLn "cd: missing arguments"
execute (BuiltinCmd (CD dir)) = liftIO $ changeDirectory dir
execute (External cmd args) = do
    result <- liftIO $ try $ do
        (_, _, _, ph) <- createProcess (proc cmd args)
        void $ waitForProcess ph

    liftIO $ case (result :: Either IOException ()) of
        Left e
            | isDoesNotExistError e -> putStrLn $ cmd ++ ": command not found"
            | isPermissionError e -> putStrLn $ cmd ++ ": permission denied"
            | otherwise -> putStrLn $ cmd ++ ": " ++ show e
        Right () -> pure ()

changeDirectory :: String -> IO ()
changeDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then putStrLn $ "cd: " ++ dir ++ ": No such file or directory"
        else setCurrentDirectory dir

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    eof <- liftIO isEOF
    unless eof $ do
        input <- liftIO getLine
        unless (null input) $ execute (parseCommand input)
        repl
