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
data Command = Command {body :: CommandBody, stdoutRedirect :: Maybe Redirect, stderrRedirect :: Maybe Redirect}

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

extractRedirects :: [String] -> ([String], Maybe Redirect, Maybe Redirect)
extractRedirects = go [] Nothing Nothing
  where
    go acc stdoutR stderrR [] = (reverse acc, stdoutR, stderrR)
    go acc stdoutR stderrR (op : file : rest)
        | op `elem` [">", "1>"] = go acc (Just (Redirect file Overwrite)) stderrR rest
        | op == "2>" = go acc stdoutR (Just (Redirect file Overwrite)) rest
        | op `elem` [">>", "1>>"] = go acc (Just (Redirect file Append)) stderrR rest
        | op == "2>>" = go acc stdoutR (Just (Redirect file Append)) rest
    go acc stdoutR stderrR (t : rest) = go (t : acc) stdoutR stderrR rest

parseCommand :: String -> Command
parseCommand input =
    let allTokens = parseArgs input
        (cmdTokens, stdoutR, stderrR) = extractRedirects allTokens
        cmdBody = case cmdTokens of
            ("exit" : args) -> BuiltinCmd $ Exit $ parseExitCode args
            ("echo" : args) -> BuiltinCmd $ Echo $ unwords args
            ("type" : args) -> BuiltinCmd $ Type $ unwords args
            ("pwd" : _) -> BuiltinCmd PWD
            ["cd"] -> BuiltinCmd $ CD Nothing
            ("cd" : args) -> BuiltinCmd $ CD $ Just $ unwords args
            (cmd : args) -> External cmd args
            [] -> Empty
     in Command{body = cmdBody, stdoutRedirect = stdoutR, stderrRedirect = stderrR}

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)
