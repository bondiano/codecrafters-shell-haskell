module Main (main) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data Command = Exit Int | Echo String | Type Command | External String [String]

commandName :: Command -> String
commandName (Exit _) = "exit"
commandName (Echo _) = "echo"
commandName (Type _) = "type"
commandName (External cmd _) = cmd

parseCommand :: String -> Command
parseCommand input = case words input of
    ("exit" : args) -> Exit $ parseExitCode args
    ("echo" : args) -> Echo $ unwords args
    ("type" : args) -> Type $ parseCommand $ unwords args
    (cmd : args) -> External cmd args
    [] -> External "" []

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

typeOfCommand :: Command -> String
typeOfCommand (External cmd _) = cmd ++ ": not found"
typeOfCommand cmd = commandName cmd ++ " is a shell builtin"

execute :: Command -> IO ()
execute (Echo str) = putStrLn str
execute (External cmd _args) = putStrLn (cmd ++ ": command not found")
execute (Type arg) = putStrLn $ typeOfCommand arg
execute (Exit code) = exitWith $ toExitCode code

repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout

    input <- getLine
    if null input then repl else execute (parseCommand input)

main :: IO ()
main = repl
