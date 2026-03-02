module Main (main) where

import System.IO (hFlush, stdout)

loop = do
    putStr "$ "
    hFlush stdout

    command <- getLine
    putStrLn $ command ++ ": command not found"
    loop

main :: IO ()
main = loop
