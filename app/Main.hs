module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import Data.List (nub)
import Shell
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    env <- buildEnv
    runReaderT (runShell repl) env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    paths <- asks envPaths
    execNames <- liftIO $ getExecutableNames paths
    let cmdCompletions = nub $ builtinNames ++ execNames
    result <- liftIO $ readInput cmdCompletions
    case result of
        Nothing -> pure ()
        Just input -> do
            unless (null input) $ executePipeline (parsePipeline input)
            repl
