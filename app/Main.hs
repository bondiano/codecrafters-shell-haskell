module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.List (nub)
import Shell
import System.Directory (listDirectory)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    env <- buildEnv
    runReaderT (runShell repl) env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    paths <- asks envPaths
    execNames <- liftIO $ getExecutableNames paths
    let cmdCompletions = nub $ builtinNames ++ execNames
    fileCompletions <- liftIO $ listDirectory "."
    result <- liftIO $ readInput cmdCompletions fileCompletions
    case result of
        Nothing -> pure ()
        Just input -> do
            unless (null input) $ execute (parseCommand input)
            repl
