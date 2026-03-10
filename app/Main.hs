module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Shell
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    paths <- asks envPaths
    execNames <- liftIO $ getExecutableNames paths
    let completions = builtinNames ++ execNames
    result <- liftIO $ readInput completions
    case result of
        Nothing -> pure ()
        Just input -> do
            unless (null input) $ execute (parseCommand input)
            repl
