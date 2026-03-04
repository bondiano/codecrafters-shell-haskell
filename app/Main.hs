module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (liftIO, runReaderT)
import Shell
import System.IO (hFlush, isEOF, stdout)

main :: IO ()
main = do
    env <- buildEnv
    runReaderT repl env

repl :: Shell ()
repl = do
    liftIO $ putStr "$ " >> hFlush stdout

    eof <- liftIO isEOF
    unless eof $ do
        input <- liftIO getLine
        unless (null input) $ execute (parseCommand input)
        repl
