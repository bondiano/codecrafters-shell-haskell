{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Shell.Input (readInput) where

import Data.List (isPrefixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO (
    hFlush,
    stdout,
 )

-- Types

data InputEvent = KeyChar Char | KeyBackspace | KeyTab | KeyEnter | KeyLeft | KeyRight | KeyUp | KeyDown | KeyIgnored | KeyEscape

data InputState = InputState
    { buffer :: String -- normal order (not reversed)
    , cursorPos :: !Int -- 0-based cursor position within buffer
    , tabCount :: !Int
    , historyEntries :: [String] -- history list
    , historyIdx :: !Int -- index into history; length = new input
    , savedInput :: String -- saved current input when browsing history
    }

data Action = Emit String | Bell

-- | Result of handling an event
data HandleResult
    = Done String -- line finished
    | Continue InputState [Action] -- keep reading
    | NeedFileComplete InputState -- need IO to get file completions

-- ANSI cursor helpers

-- | Move cursor left n positions
cursorLeft :: Int -> String
cursorLeft n
    | n <= 0 = ""
    | otherwise = "\ESC[" ++ show n ++ "D"

-- | Move cursor right n positions
cursorRight :: Int -> String
cursorRight n
    | n <= 0 = ""
    | otherwise = "\ESC[" ++ show n ++ "C"

{- | Rewrite tail of buffer after an insert/delete at cursor, then reposition cursor.
Emits: chars after cursor + spaces to clear + moves cursor back.
-}
rewriteTail :: String -> Int -> String
rewriteTail tailStr clearExtra =
    let total = length tailStr + clearExtra
     in tailStr ++ replicate clearExtra ' ' ++ cursorLeft total

-- | Build ANSI output to replace the current line with a new buffer
replaceLineOutput :: InputState -> String -> String
replaceLineOutput st newBuf =
    let oldLen = length (buffer st)
        pos = cursorPos st
        moveToStart = cursorLeft pos
        clearAndWrite = newBuf ++ replicate (max 0 (oldLen - length newBuf)) ' '
        moveBack = cursorLeft (max 0 (oldLen - length newBuf))
     in moveToStart ++ clearAndWrite ++ moveBack

-- | Update state with a new buffer from history navigation
replaceLine :: InputState -> String -> Int -> InputState
replaceLine st newBuf newIdx =
    st{buffer = newBuf, cursorPos = length newBuf, historyIdx = newIdx, tabCount = 0}

-- Pure logic

-- | Read a single input event, consuming multi-byte escape sequences.
readEvent :: IO InputEvent
readEvent = do
    evt <- charToEvent <$> getChar
    case evt of
        KeyEscape -> resolveEscape
        _ -> pure evt

charToEvent :: Char -> InputEvent
charToEvent = \case
    '\n' -> KeyEnter
    '\DEL' -> KeyBackspace
    '\b' -> KeyBackspace
    '\t' -> KeyTab
    '\ESC' -> KeyEscape
    c -> KeyChar c

-- | Resolve a pending escape into a concrete event by reading the CSI sequence.
resolveEscape :: IO InputEvent
resolveEscape = do
    c1 <- getChar
    case c1 of
        '[' -> csiEvent <$> getChar
        _ -> pure KeyIgnored

csiEvent :: Char -> InputEvent
csiEvent = \case
    'A' -> KeyUp
    'B' -> KeyDown
    'C' -> KeyRight
    'D' -> KeyLeft
    _ -> KeyIgnored

handleEvent :: [String] -> InputEvent -> InputState -> HandleResult
handleEvent _ KeyIgnored st = Continue st []
handleEvent _ KeyEscape st = Continue st [] -- resolved in readEvent, never reaches here
handleEvent _ KeyEnter st = Done (buffer st)
handleEvent _ KeyLeft st
    | cursorPos st > 0 =
        Continue (st{cursorPos = cursorPos st - 1}) [Emit (cursorLeft 1)]
    | otherwise = Continue st []
handleEvent _ KeyRight st
    | cursorPos st < length (buffer st) =
        Continue (st{cursorPos = cursorPos st + 1}) [Emit (cursorRight 1)]
    | otherwise = Continue st []
handleEvent _ KeyUp st
    | historyIdx st > 0 =
        let newIdx = historyIdx st - 1
            newBuf = historyEntries st !! newIdx
            st' =
                if historyIdx st == length (historyEntries st)
                    then st{savedInput = buffer st}
                    else st
         in Continue (replaceLine st' newBuf newIdx) [Emit (replaceLineOutput st newBuf)]
    | otherwise = Continue st []
handleEvent _ KeyDown st
    | historyIdx st < length (historyEntries st) =
        let newIdx = historyIdx st + 1
            newBuf =
                if newIdx == length (historyEntries st)
                    then savedInput st
                    else historyEntries st !! newIdx
         in Continue (replaceLine st newBuf newIdx) [Emit (replaceLineOutput st newBuf)]
    | otherwise = Continue st []
handleEvent _ (KeyChar c) st =
    let pos = cursorPos st
        buf = buffer st
        (before, after) = splitAt pos buf
        newBuf = before ++ [c] ++ after
        output = [c] : [rewriteTail after 0 | not (null after)]
     in Continue (st{buffer = newBuf, cursorPos = pos + 1, tabCount = 0}) [Emit (concat output)]
handleEvent _ KeyBackspace st
    | cursorPos st == 0 = Continue st []
    | otherwise =
        let pos = cursorPos st
            buf = buffer st
            (before, after) = splitAt pos buf
            newBuf = init before ++ after
            output = "\b" ++ rewriteTail after 1
         in Continue (st{buffer = newBuf, cursorPos = pos - 1, tabCount = 0}) [Emit output]
handleEvent cmdCompletions KeyTab st =
    let (pre, word) = splitBuffer (buffer st)
     in if null pre
            then toResult $ handleTab pre word (map (,False) cmdCompletions) st
            else NeedFileComplete st

toResult :: Either String (InputState, [Action]) -> HandleResult
toResult (Left s) = Done s
toResult (Right (st, acts)) = Continue st acts

splitBuffer :: String -> (String, String)
splitBuffer buf = case break (== ' ') (reverse buf) of
    (rword, rrest) -> (reverse rrest, reverse rword)

handleTab :: String -> String -> [(String, Bool)] -> InputState -> Either String (InputState, [Action])
handleTab pre word completions st = complete (sort $ filter ((word `isPrefixOf`) . fst) completions)
  where
    complete [(match, isDir)] =
        let trail = if isDir then "/" else " "
            suffix = drop (length word) match ++ trail
            newBuf = pre ++ match ++ trail
         in Right (st{buffer = newBuf, cursorPos = length newBuf, tabCount = 0}, [Emit suffix])
    complete ms@(_ : _ : _)
        | length pfx > length word =
            let suffix = drop (length word) pfx
                newBuf = pre ++ pfx
             in Right (st{buffer = newBuf, cursorPos = length newBuf, tabCount = 0}, [Emit suffix])
        | tabCount st >= 1 =
            let display = map (\(name, isDir) -> if isDir then name ++ "/" else name) ms
                newEmit = "\r\n" ++ unwords' display ++ "\r\n$ " ++ buffer st
             in Right (st{tabCount = 0}, [Emit newEmit])
        | otherwise =
            Right (st{tabCount = 1}, [Bell])
      where
        pfx = longestCommonPrefix (map fst ms)
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

{- | Get file completions for a word, supporting nested paths.
  Returns (name, isDirectory) pairs.
-}
getFileCompletions :: String -> IO [(String, Bool)]
getFileCompletions word = case splitAtLastSlash word of
    Nothing -> do
        entries <- listDirectory "."
        let matched = filter (word `isPrefixOf`) entries
        mapM (\e -> (e,) <$> doesDirectoryExist e) matched
    Just (dirPath, prefix) -> do
        exists <- doesDirectoryExist dirPath
        if exists
            then do
                entries <- listDirectory dirPath
                let matched = filter (prefix `isPrefixOf`) entries
                mapM (\e -> (dirPath ++ e,) <$> doesDirectoryExist (dirPath ++ e)) matched
            else pure []
  where
    splitAtLastSlash s = case break (== '/') (reverse s) of
        (_, []) -> Nothing
        (rprefix, rdir) -> Just (reverse rdir, reverse rprefix)

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
readInput cmdCompletions history = loop (InputState "" 0 0 history (length history) "")
  where
    loop st = do
        evt <- readEvent
        case handleEvent cmdCompletions evt st of
            Done result -> do
                emit "\n"
                pure (Just result)
            Continue st' actions -> do
                mapM_ runAction actions
                loop st'
            NeedFileComplete st' -> do
                let (pre, word) = splitBuffer (buffer st')
                candidates <- getFileCompletions word
                case handleTab pre word candidates st' of
                    Left result -> do
                        emit "\n"
                        pure (Just result)
                    Right (st'', actions) -> do
                        mapM_ runAction actions
                        loop st''
