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
readInput :: [String] -> IO (Maybe String)
readInput completions = do
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

    handleTab :: String -> IO (Maybe String)
    handleTab buf = do
        let text = reverse buf
            matches = filter (text `isPrefixOf`) completions
        case matches of
            [match] -> do
                let suffix = drop (length text) match ++ " "
                hPutStr stdout suffix
                hFlush stdout
                loop (reverse (match ++ " "))
            _ -> do
                hPutStr stdout "\x07"
                hFlush stdout
                loop buf
