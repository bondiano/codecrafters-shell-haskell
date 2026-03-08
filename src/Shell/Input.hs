{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use putStr" #-}
{-# HLINT ignore "Use getChar" #-}
module Shell.Input (readInput) where

import Data.List (isPrefixOf)
import System.IO (
    BufferMode (NoBuffering),
    hFlush,
    hGetChar,
    hPutStr,
    hSetBuffering,
    hSetEcho,
    stdin,
    stdout,
 )

{- | Read a line of input character-by-character with TAB completion support.
Sets terminal to raw mode (no buffering, no echo), reads each char manually,
and restores nothing — raw mode stays on for the whole session.
-}
readInput :: IO (Maybe String)
readInput = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    loop []
  where
    loop :: String -> IO (Maybe String)
    loop buf = do
        c <- hGetChar stdin
        case c of
            '\n' -> do
                hPutStr stdout "\n"
                hFlush stdout
                pure (Just (reverse buf))
            '\DEL' -> handleBackspace buf
            '\b' -> handleBackspace buf
            '\t' -> handleTab buf
            _ -> do
                hPutStr stdout [c]
                hFlush stdout
                loop (c : buf)

    handleBackspace :: String -> IO (Maybe String)
    handleBackspace [] = loop []
    handleBackspace (_ : rest) = do
        -- Move cursor back, overwrite with space, move back again
        hPutStr stdout "\b \b"
        hFlush stdout
        loop rest

    builtins :: [String]
    builtins = ["echo", "exit", "type", "pwd", "cd"]

    handleTab :: String -> IO (Maybe String)
    handleTab buf = do
        let text = reverse buf
            matches = filter (text `isPrefixOf`) builtins
        case matches of
            [match] -> do
                let suffix = drop (length text) match ++ " "
                hPutStr stdout suffix
                hFlush stdout
                loop (reverse (match ++ " "))
            _ -> loop buf
