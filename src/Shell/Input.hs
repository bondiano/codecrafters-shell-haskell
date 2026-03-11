{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use putStr" #-}
{-# HLINT ignore "Use getChar" #-}
module Shell.Input (readInput) where

import Data.List (isPrefixOf, sort)
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
    loop [] 0
  where
    loop :: String -> Int -> IO (Maybe String)
    loop buf tabCount = do
        c <- hGetChar stdin
        case c of
            '\n' -> do
                hPutStr stdout "\n"
                hFlush stdout
                pure (Just (reverse buf))
            '\DEL' -> handleBackspace buf
            '\b' -> handleBackspace buf
            '\t' -> handleTab buf tabCount
            _ -> do
                hPutStr stdout [c]
                hFlush stdout
                loop (c : buf) 0

    handleBackspace :: String -> IO (Maybe String)
    handleBackspace [] = loop [] 0
    handleBackspace (_ : rest) = do
        hPutStr stdout "\b \b"
        hFlush stdout
        loop rest 0

    handleTab :: String -> Int -> IO (Maybe String)
    handleTab buf tabCount = do
        let text = reverse buf
            matches = sort $ filter (text `isPrefixOf`) completions
        case matches of
            [match] -> do
                let suffix = drop (length text) match ++ " "
                hPutStr stdout suffix
                hFlush stdout
                loop (reverse (match ++ " ")) 0
            (_ : _ : _)
                | tabCount >= 1 -> do
                    hPutStr stdout $ "\n" ++ unwords' matches ++ "\n$ " ++ text
                    hFlush stdout
                    loop buf 0
                | otherwise -> do
                    hPutStr stdout "\x07"
                    hFlush stdout
                    loop buf 1
            _ -> do
                hPutStr stdout "\x07"
                hFlush stdout
                loop buf 0

    unwords' :: [String] -> String
    unwords' [] = ""
    unwords' [x] = x
    unwords' (x : xs) = x ++ "  " ++ unwords' xs
