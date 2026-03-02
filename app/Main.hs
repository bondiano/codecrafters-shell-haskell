module Main (main) where

import System.IO (hFlush, stdout)
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)
import qualified Control.Monad

data Commands = Exit Int | External String [String]

parseCommand :: String -> Commands
parseCommand input = case words input of
    ("exit" : atgs) -> Exit $ parseExitCode atgs
    (cmd : args)    -> External cmd args
    []               -> External "" []

parseExitCode :: [String] -> Int
parseExitCode []    = 0
parseExitCode (x:_) = read x

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

executeCommand :: Commands -> IO Bool
executeCommand (Exit code) = exitWith (toExitCode code) >> return False
executeCommand (External cmd args) = putStrLn (cmd ++ " " ++ unwords args) >> return True

repl :: IO ()
repl = do
    putStr "$ "
    hFlush stdout

    input <- getLine
    continue <- executeCommand $ parseCommand input
    Control.Monad.when continue repl

main :: IO ()
main = repl
