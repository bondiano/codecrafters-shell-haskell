{-# LANGUAGE LambdaCase #-}

module Shell.Input (readInput) where

import Data.List (isPrefixOf, sort)
import System.IO (
    BufferMode (NoBuffering),
    hFlush,
    hSetBuffering,
    hSetEcho,
    stdin,
    stdout,
 )

-- Types

data InputEvent = KeyChar Char | KeyBackspace | KeyTab | KeyEnter

data InputState = InputState
    { buffer :: String -- normal order (not reversed)
    , tabCount :: !Int
    }

data Action = Emit String | Bell

-- Pure logic

charToEvent :: Char -> InputEvent
charToEvent = \case
    '\n' -> KeyEnter
    '\DEL' -> KeyBackspace
    '\b' -> KeyBackspace
    '\t' -> KeyTab
    c -> KeyChar c

handleEvent :: [String] -> [String] -> InputEvent -> InputState -> Either String (InputState, [Action])
handleEvent _ _ KeyEnter st = Left (buffer st)
handleEvent _ _ (KeyChar c) st =
    Right (st{buffer = buffer st ++ [c], tabCount = 0}, [Emit [c]])
handleEvent _ _ KeyBackspace st = case buffer st of
    [] -> Right (st, [])
    buf -> Right (st{buffer = init buf, tabCount = 0}, [Emit "\b \b"])
handleEvent cmdCompletions fileCompletions KeyTab st =
    let (pre, word) = splitBuffer (buffer st)
        candidates = if null pre then cmdCompletions else fileCompletions
     in handleTab pre word candidates st

splitBuffer :: String -> (String, String)
splitBuffer buf = case break (== ' ') (reverse buf) of
    (rword, rrest) -> (reverse rrest, reverse rword)

handleTab :: String -> String -> [String] -> InputState -> Either String (InputState, [Action])
handleTab pre word completions st = complete (sort $ filter (word `isPrefixOf`) completions)
  where
    complete [match] =
        let suffix = drop (length word) match ++ " "
         in Right (st{buffer = pre ++ match ++ " ", tabCount = 0}, [Emit suffix])
    complete ms@(_ : _ : _)
        | length pfx > length word =
            let suffix = drop (length word) pfx
             in Right (st{buffer = pre ++ pfx, tabCount = 0}, [Emit suffix])
        | tabCount st >= 1 =
            Right (st{tabCount = 0}, [Emit $ "\n" ++ unwords' ms ++ "\n$ " ++ buffer st])
        | otherwise =
            Right (st{tabCount = 1}, [Bell])
      where
        pfx = longestCommonPrefix ms
    complete _ = Right (st{tabCount = 0}, [Bell])

longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""
longestCommonPrefix xs = foldr1 commonPrefix xs

commonPrefix :: String -> String -> String
commonPrefix (a : as') (b : bs)
    | a == b = a : commonPrefix as' bs
commonPrefix _ _ = ""

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (x : xs) = x ++ "  " ++ unwords' xs

-- IO shell

emit :: String -> IO ()
emit s = putStr s >> hFlush stdout

runAction :: Action -> IO ()
runAction = \case
    Emit s -> emit s
    Bell -> emit "\x07"

{- | Read a line of input character-by-character with TAB completion support.
Sets terminal to raw mode (no buffering, no echo), reads each char manually,
and restores nothing — raw mode stays on for the whole session.
-}
readInput :: [String] -> [String] -> IO (Maybe String)
readInput cmdCompletions fileCompletions = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    loop (InputState "" 0)
  where
    loop st = do
        c <- getChar
        case handleEvent cmdCompletions fileCompletions (charToEvent c) st of
            Left result -> do
                emit "\n"
                pure (Just result)
            Right (st', actions) -> do
                mapM_ runAction actions
                loop st'
