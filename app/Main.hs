module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath (splitSearchPath, (</>))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data Builtin = Exit Int | Echo String | Type String

data Command = BuiltinCmd Builtin | External String [String]

newtype Env = Env {envPaths :: [FilePath]}

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"

buildEnv :: IO Env
buildEnv = do
    paths <- buildEnvPath

    return
        Env
            { envPaths = paths
            }

type Shell = ReaderT Env IO

builtinName :: Builtin -> String
builtinName (Exit _) = "exit"
builtinName (Echo _) = "echo"
builtinName (Type _) = "type"

parseCommand :: String -> Command
parseCommand input = case words input of
    ("exit" : args) -> BuiltinCmd $ Exit $ parseExitCode args
    ("echo" : args) -> BuiltinCmd $ Echo $ unwords args
    ("type" : args) -> BuiltinCmd $ Type $ unwords args
    (cmd : args) -> External cmd args
    [] -> External "" []

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

getExecutablePathFromPaths :: [FilePath] -> String -> IO (Maybe String)
getExecutablePathFromPaths [] _ = pure Nothing
getExecutablePathFromPaths (dir : dirs) cmd = do
    let fullPath = dir </> cmd
    exists <- doesFileExist fullPath
    if exists then pure $ Just fullPath else getExecutablePathFromPaths dirs cmd

typeOfCommand :: Command -> Shell String
typeOfCommand (BuiltinCmd b) = pure $ builtinName b ++ " is a shell builtin"
typeOfCommand (External cmd _) = do
    env <- ask
    mbPaths <- liftIO $ getExecutablePathFromPaths (envPaths env) cmd

    return $ case mbPaths of
        Just path -> cmd ++ " is " ++ path
        Nothing -> cmd ++ ": not found"

execute :: Command -> Shell ()
execute (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
execute cmd = do
    result <- eval cmd
    liftIO $ putStrLn result

eval :: Command -> Shell String
eval (BuiltinCmd (Echo str)) = pure str
eval (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name)
eval (BuiltinCmd (Exit _)) = pure "" -- unreachable
eval (External cmd _args) = pure $ cmd ++ ": command not found"

repl :: Shell ()
repl = do
    liftIO $ do
        putStr "$ "
        hFlush stdout

    input <- liftIO getLine
    unless (null input) $ execute (parseCommand input)

    repl

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env
