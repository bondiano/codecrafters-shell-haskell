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

handleEvent :: [String] -> InputEvent -> InputState -> Either String (InputState, [Action])
handleEvent _ KeyEnter st = Left (buffer st)
handleEvent _ (KeyChar c) st =
    Right (st{buffer = buffer st ++ [c], tabCount = 0}, [Emit [c]])
handleEvent _ KeyBackspace st = case buffer st of
    [] -> Right (st, [])
    buf -> Right (st{buffer = init buf, tabCount = 0}, [Emit "\b \b"])
handleEvent completions KeyTab st = handleTab completions st

handleTab :: [String] -> InputState -> Either String (InputState, [Action])
handleTab completions st = complete (sort $ filter (text `isPrefixOf`) completions)
  where
    text = buffer st
    complete [match] =
        let suffix = drop (length text) match ++ " "
         in Right (st{buffer = match ++ " ", tabCount = 0}, [Emit suffix])
    complete ms@(_ : _ : _)
        | length prefix > length text =
            let suffix = drop (length text) prefix
             in Right (st{buffer = prefix, tabCount = 0}, [Emit suffix])
        | tabCount st >= 1 =
            Right (st{tabCount = 0}, [Emit $ "\n" ++ unwords' ms ++ "\n$ " ++ text])
        | otherwise =
            Right (st{tabCount = 1}, [Bell])
      where
        prefix = longestCommonPrefix ms
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
readInput :: [String] -> IO (Maybe String)
readInput completions = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    loop (InputState "" 0)
  where
    loop st = do
        c <- getChar
        case handleEvent completions (charToEvent c) st of
            Left result -> do
                emit "\n"
                pure (Just result)
            Right (st', actions) -> do
                mapM_ runAction actions
                loop st'
