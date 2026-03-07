module Shell.Parser (
    Builtin (..),
    Command (..),
    CommandBody (..),
    Redirect (..),
    RedirectMode (..),
    builtinName,
    parseCommand,
    parseArgs,
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)

data Builtin = Exit Int | Echo String | Type String | PWD | CD (Maybe FilePath)

data RedirectMode = Overwrite | Append deriving (Eq, Show)
data Redirect = Redirect {target :: FilePath, mode :: RedirectMode} deriving (Eq, Show)

data CommandBody = BuiltinCmd Builtin | External String [String] | Empty
data Command = Command {body :: CommandBody, stdoutRedirect :: Maybe Redirect}

builtinName :: Builtin -> String
builtinName (Exit _) = "exit"
builtinName (Echo _) = "echo"
builtinName (Type _) = "type"
builtinName PWD = "pwd"
builtinName (CD _) = "cd"

data QuoteState = Unquoted | InSingle | InDouble

data ParseState = ParseState
    { quoteState :: QuoteState
    , currentToken :: Maybe String
    , tokens :: [String]
    }

parseArgs :: String -> [String]
parseArgs = finalize . go ParseState{quoteState = Unquoted, currentToken = Nothing, tokens = []}
  where
    finalize state = reverse $ case currentToken state of
        Nothing -> tokens state
        Just token -> reverse token : tokens state

    go :: ParseState -> String -> ParseState
    go st [] = st
    -- Quote toggles
    go st@ParseState{quoteState = Unquoted} ('\'' : rest) =
        go st{quoteState = InSingle, currentToken = Just (fromMaybe [] (currentToken st))} rest
    go st@ParseState{quoteState = Unquoted} ('"' : rest) =
        go st{quoteState = InDouble, currentToken = Just (fromMaybe [] (currentToken st))} rest
    go st@ParseState{quoteState = InSingle} ('\'' : rest) =
        go st{quoteState = Unquoted} rest
    go st@ParseState{quoteState = InDouble} ('"' : rest) =
        go st{quoteState = Unquoted} rest
    -- Backslash inside double quotes
    go st@ParseState{quoteState = InDouble} ('\\' : next : rest)
        | next `elem` ['\\', '"'] = go st{currentToken = Just (next : fromMaybe [] (currentToken st))} rest
        | otherwise = go st{currentToken = Just (next : '\\' : fromMaybe [] (currentToken st))} rest
    -- Space outside quotes — flush token
    go st@ParseState{quoteState = Unquoted} (' ' : rest) = case currentToken st of
        Just t -> go st{currentToken = Nothing, tokens = reverse t : tokens st} rest
        Nothing -> go st rest
    -- Backslash outside quotes — escape next char
    go st@ParseState{quoteState = Unquoted} ('\\' : next : rest) =
        go st{currentToken = Just (next : fromMaybe [] (currentToken st))} rest
    -- Any char inside quotes or outside — append
    go st (c : rest) =
        go st{currentToken = Just (c : fromMaybe [] (currentToken st))} rest

extractRedirect :: [String] -> ([String], Maybe Redirect)
extractRedirect = go []
  where
    go acc [] = (reverse acc, Nothing)
    go acc (op : file : rest)
        | op `elem` [">", "1>"] = (reverse acc ++ rest, Just (Redirect file Overwrite))
    go acc (t : rest) = go (t : acc) rest

parseCommand :: String -> Command
parseCommand input =
    let allTokens = parseArgs input
        (cmdTokens, redirect) = extractRedirect allTokens
        cmdBody = case cmdTokens of
            ("exit" : args) -> BuiltinCmd $ Exit $ parseExitCode args
            ("echo" : args) -> BuiltinCmd $ Echo $ unwords args
            ("type" : args) -> BuiltinCmd $ Type $ unwords args
            ("pwd" : _) -> BuiltinCmd PWD
            ["cd"] -> BuiltinCmd $ CD Nothing
            ("cd" : args) -> BuiltinCmd $ CD $ Just $ unwords args
            (cmd : args) -> External cmd args
            [] -> Empty
     in Command{body = cmdBody, stdoutRedirect = redirect}

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)
