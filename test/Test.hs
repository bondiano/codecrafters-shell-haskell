module Main where

import Shell.Parser (parseArgs)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "parseArgs"
    [ testGroup
        "basic splitting"
        [ testCase "two words" $ parseArgs "hello world" @?= ["hello", "world"]
        , testCase "multiple spaces" $ parseArgs "hello    world" @?= ["hello", "world"]
        , testCase "empty string" $ parseArgs "" @?= []
        , testCase "only spaces" $ parseArgs "   " @?= []
        ]
    , testGroup
        "single quotes"
        [ testCase "preserves spaces" $ parseArgs "'hello world'" @?= ["hello world"]
        , testCase "preserves multiple spaces" $ parseArgs "'hello    world'" @?= ["hello    world"]
        , testCase "challenge example" $ parseArgs "'world     test'" @?= ["world     test"]
        ]
    , testGroup
        "adjacent quotes"
        [ testCase "concatenates quoted" $ parseArgs "'hello''world'" @?= ["helloworld"]
        , testCase "empty quotes in word" $ parseArgs "hello''world" @?= ["helloworld"]
        , testCase "quotes inside word" $ parseArgs "a'b'c" @?= ["abc"]
        ]
    , testGroup
        "empty quotes"
        [ testCase "standalone empty quotes" $ parseArgs "''" @?= [""]
        ]
    , testGroup
        "mixed"
        [ testCase "echo with quoted arg" $ parseArgs "echo 'hello world'" @?= ["echo", "hello world"]
        , testCase "cat with quoted path" $ parseArgs "cat '/tmp/file name'" @?= ["cat", "/tmp/file name"]
        , testCase "quoted in middle" $ parseArgs "a 'b c' d" @?= ["a", "b c", "d"]
        ]
    , testGroup
        "double quotes"
        [ testCase "preserves spaces" $ parseArgs "echo \"hello    world\"" @?= ["echo", "hello    world"]
        , testCase "concatenates adjacent" $ parseArgs "echo \"hello\"\"world\"" @?= ["echo", "helloworld"]
        , testCase "separate arguments" $ parseArgs "echo \"hello\" \"world\"" @?= ["echo", "hello", "world"]
        , testCase "single quote inside double" $ parseArgs "echo \"shell's test\"" @?= ["echo", "shell's test"]
        , testCase "empty double quotes" $ parseArgs "\"\"" @?= [""]
        , testCase "mixed with spaces" $ parseArgs "echo \"quz  hello\"  \"bar\"" @?= ["echo", "quz  hello", "bar"]
        , testCase "multiple quoted args" $ parseArgs "echo \"bar\"  \"shell's\"  \"foo\"" @?= ["echo", "bar", "shell's", "foo"]
        , testCase "cat with double-quoted paths" $
            parseArgs "cat \"/tmp/file name\" \"/tmp/'file name' with spaces\""
              @?= ["cat", "/tmp/file name", "/tmp/'file name' with spaces"]
        ]
    , testGroup
        "mixed quote types"
        [ testCase "single then double" $ parseArgs "'hello'\"world\"" @?= ["helloworld"]
        , testCase "double then single" $ parseArgs "\"hello\"'world'" @?= ["helloworld"]
        , testCase "double quote inside single" $ parseArgs "echo 'he said \"hi\"'" @?= ["echo", "he said \"hi\""]
        , testCase "unquoted + double" $ parseArgs "hello\"world foo\"bar" @?= ["helloworld foobar"]
        ]
    , testGroup
        "challenge examples"
        [ testCase "echo shell hello" $ parseArgs "echo 'shell hello'" @?= ["echo", "shell hello"]
        , testCase "cat with two paths" $
            parseArgs "cat '/tmp/file name' '/tmp/file name with spaces'"
              @?= ["cat", "/tmp/file name", "/tmp/file name with spaces"]
        ]
    ]
