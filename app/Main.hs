module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (liftIO, runReaderT)
import Shell
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    result <- liftIO readInput
    case result of
        Nothing -> pure ()
        Just input -> do
            unless (null input) $ execute (parseCommand input)
            repl
