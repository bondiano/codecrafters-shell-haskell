module Main where

import Shell.Parser (parseArgs)
import System.Exit (exitFailure, exitSuccess)

data TestResult = Pass | Fail String String String

runTest :: String -> [String] -> TestResult
runTest input expected =
  let actual = parseArgs input
   in if actual == expected
        then Pass
        else Fail input (show expected) (show actual)

tests :: [(String, [String])]
tests =
  [ -- basic splitting
    ("hello world", ["hello", "world"])
  , ("hello    world", ["hello", "world"])
  , ("", [])
  , ("   ", [])
  , -- single quotes preserve spaces
    ("'hello world'", ["hello world"])
  , ("'hello    world'", ["hello    world"])
  , ("'world     test'", ["world     test"])
  , -- adjacent quotes concatenate
    ("'hello''world'", ["helloworld"])
  , ("hello''world", ["helloworld"])
  , ("a'b'c", ["abc"])
  , -- empty quotes
    ("''", [""])
  , -- mixed quoted and unquoted
    ("echo 'hello world'", ["echo", "hello world"])
  , ("cat '/tmp/file name'", ["cat", "/tmp/file name"])
  , ("a 'b c' d", ["a", "b c", "d"])
  , -- from the challenge examples
    ("echo 'shell hello'", ["echo", "shell hello"])
  , ("cat '/tmp/file name' '/tmp/file name with spaces'", ["cat", "/tmp/file name", "/tmp/file name with spaces"])
  ]

main :: IO ()
main = do
  let results = map (\(input, expected) -> runTest input expected) tests
      failures = [(i, e, a) | Fail i e a <- results]
      total = length results
      passed = total - length failures

  mapM_
    ( \(input, expected, actual) ->
        putStrLn $ "FAIL: parseArgs " ++ show input ++ "\n  expected: " ++ expected ++ "\n  actual:   " ++ actual
    )
    failures

  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"

  if null failures then exitSuccess else exitFailure
