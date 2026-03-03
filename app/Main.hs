module Main (main) where

import Control.Monad (unless)
import Control.Monad.ListM (findM)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, executable, getPermissions)
import System.Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath (splitSearchPath, (</>))
import System.IO (hFlush, isEOF, stdout)
import System.Process (callProcess)
import Text.Read (readMaybe)

data Builtin = Exit Int | Echo String | Type String

data Command = BuiltinCmd Builtin | External String [String] | Empty

newtype Env = Env {envPaths :: [FilePath]}

buildEnvPath :: IO [FilePath]
buildEnvPath = maybe [] splitSearchPath <$> lookupEnv "PATH"

buildEnv :: IO Env
buildEnv = Env <$> buildEnvPath

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
    [] -> Empty

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    exists <- doesFileExist path
    if exists then executable <$> getPermissions path else pure False

getExecutablePathFromPaths :: [FilePath] -> String -> IO (Maybe String)
getExecutablePathFromPaths dirs cmd =
    fmap (</> cmd) <$> findM (\d -> isExecutable (d </> cmd)) dirs

typeOfCommand :: Command -> Shell String
typeOfCommand Empty = pure ""
typeOfCommand (BuiltinCmd b) = pure $ builtinName b ++ " is a shell builtin"
typeOfCommand (External cmd _) = do
    env <- ask
    mbPath <- liftIO $ getExecutablePathFromPaths (envPaths env) cmd

    return $ case mbPath of
        Just path -> cmd ++ " is " ++ path
        Nothing -> cmd ++ ": not found"

execute :: Command -> Shell ()
execute Empty = pure ()
execute (BuiltinCmd (Exit code)) = liftIO $ exitWith $ toExitCode code
execute (BuiltinCmd (Echo str)) = liftIO (putStrLn str)
execute (BuiltinCmd (Type "")) = pure ()
execute (BuiltinCmd (Type name)) = typeOfCommand (parseCommand name) >>= liftIO . putStrLn
execute (External cmd args) = do
    env <- ask
    mbPath <- liftIO $ getExecutablePathFromPaths (envPaths env) cmd

    case mbPath of
        Just path -> liftIO $ callProcess path args
        Nothing -> liftIO $ putStrLn $ cmd ++ ": command not found"

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    eof <- liftIO isEOF
    unless eof $ do
        input <- liftIO getLine
        unless (null input) $ execute (parseCommand input)
        repl

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env
