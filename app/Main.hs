module Main (main) where

import System.IO (hFlush, stdout)
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)
import Control.Monad (when)

data Commands = Exit Int | Echo String | External String [String]

parseCommand :: String -> Commands
parseCommand input = case words input of
    ("exit" : atgs) -> Exit $ parseExitCode atgs
    ("echo" : args) -> Echo $ unwords args
    (cmd : args)    -> External cmd args
    []               -> External "" []

parseExitCode :: [String] -> Int
parseExitCode []    = 0
parseExitCode (x:_) = read x

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

execute :: Commands -> IO ()
execute (Echo str)          = putStrLn str
execute (External cmd _args) = putStrLn (cmd ++ ": command not found")
execute (Exit code)         = exitWith (toExitCode code)

executeCommand :: Commands -> IO Bool
executeCommand (Exit code) = exitWith (toExitCode code) >> return False
executeCommand cmd = execute cmd >> return True

repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout

    input <- getLine
    continue <- executeCommand $ parseCommand input
    when continue repl

main :: IO ()
main = repl
