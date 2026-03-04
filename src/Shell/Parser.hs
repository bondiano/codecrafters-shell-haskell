module Shell.Parser (
    Builtin (..),
    Command (..),
    builtinName,
    parseCommand,
    parseArgs,
) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Builtin = Exit Int | Echo String | Type String | PWD | CD (Maybe FilePath)

data Command = BuiltinCmd Builtin | External String [String] | Empty

builtinName :: Builtin -> String
builtinName (Exit _) = "exit"
builtinName (Echo _) = "echo"
builtinName (Type _) = "type"
builtinName PWD = "pwd"
builtinName (CD _) = "cd"

parseArgs :: String -> [String]
parseArgs = finalize . go False False [] []
  where
    finalize (hasToken, token, acc)
        | hasToken = reverse $ reverse token : acc
        | otherwise = reverse acc

    go _ hasToken token acc [] = (hasToken, token, acc)
    go inQuote _ token acc ('\'' : rest) = go (not inQuote) True token acc rest
    go True _ token acc (c : rest) = go True True (c : token) acc rest
    go False hasToken token acc (' ' : rest)
        | hasToken = go False False [] (reverse token : acc) rest
        | otherwise = go False False [] acc rest
    go False _ token acc (c : rest) = go False True (c : token) acc rest

parseCommand :: String -> Command
parseCommand input = case parseArgs input of
    ("exit" : args) -> BuiltinCmd $ Exit $ parseExitCode args
    ("echo" : args) -> BuiltinCmd $ Echo $ unwords args
    ("type" : args) -> BuiltinCmd $ Type $ unwords args
    ("pwd" : _) -> BuiltinCmd PWD
    ["cd"] -> BuiltinCmd $ CD Nothing
    ("cd" : args) -> BuiltinCmd $ CD $ Just $ unwords args
    (cmd : args) -> External cmd args
    [] -> Empty

parseExitCode :: [String] -> Int
parseExitCode [] = 0
parseExitCode (x : _) = fromMaybe 0 (readMaybe x)
