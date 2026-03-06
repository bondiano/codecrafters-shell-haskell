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
        "backslash outside quotes"
        [ testCase "escaped spaces in word" $
            parseArgs "three\\ \\ \\ spaces" @?= ["three   spaces"]
        , testCase "escaped space then unescaped spaces" $
            parseArgs "before\\     after" @?= ["before ", "after"]
        , testCase "backslash + regular char" $
            parseArgs "test\\nexample" @?= ["testnexample"]
        , testCase "escaped backslash" $
            parseArgs "hello\\\\world" @?= ["hello\\world"]
        , testCase "escaped single quotes" $
            parseArgs "\\'hello\\'" @?= ["'hello'"]
        , testCase "escaped double quotes" $
            parseArgs "\\\"hello\\\"" @?= ["\"hello\""]
        , testCase "multiple escaped spaces (echo)" $
            parseArgs "echo multiple\\ \\ \\ \\ spaces" @?= ["echo", "multiple    spaces"]
        , testCase "escaped quotes in echo" $
            parseArgs "echo \\'\\\"literal quotes\\\"\\'" @?= ["echo", "'\"literal", "quotes\"'"]
        , testCase "backslash + underscore" $
            parseArgs "echo ignore\\_backslash" @?= ["echo", "ignore_backslash"]
        , testCase "cat with backslash in paths" $
            parseArgs "cat /tmp/\\_ignored_1 /tmp/ignore_\\2 /tmp/just_one_\\\\_3"
              @?= ["cat", "/tmp/_ignored_1", "/tmp/ignore_2", "/tmp/just_one_\\_3"]
        , testCase "trailing backslash" $
            parseArgs "hello\\" @?= ["hello\\"]
        , testCase "backslash only" $
            parseArgs "\\" @?= ["\\"]
        ]
    , testGroup
        "backslash inside single quotes"
        [ testCase "backslashes are literal" $
            parseArgs "echo 'shell\\\\\\nscript'" @?= ["echo", "shell\\\\\\nscript"]
        , testCase "backslash + double quote is literal" $
            parseArgs "echo 'example\\\"test'" @?= ["echo", "example\\\"test"]
        , testCase "multiple backslashes" $
            parseArgs "echo 'multiple\\\\slashes'" @?= ["echo", "multiple\\\\slashes"]
        , testCase "backslash-quote pairs are literal" $
            parseArgs "echo 'every\\\"thing_is\\\"literal'" @?= ["echo", "every\\\"thing_is\\\"literal"]
        , testCase "cat with backslash in single-quoted paths" $
            parseArgs "cat /tmp/'no slash 1' /tmp/'one slash \\2' /tmp/'two slashes \\\\3'"
              @?= ["cat", "/tmp/no slash 1", "/tmp/one slash \\2", "/tmp/two slashes \\\\3"]
        ]
    , testGroup
        "backslash inside double quotes"
        [ testCase "escaped backslash" $
            parseArgs "echo \"A \\\\ escapes itself\"" @?= ["echo", "A \\ escapes itself"]
        , testCase "escaped double quote" $
            parseArgs "echo \"A \\\" inside double quotes\"" @?= ["echo", "A \" inside double quotes"]
        , testCase "backslash before normal char is literal" $
            parseArgs "echo \"just'one'\\\\n'backslash\"" @?= ["echo", "just'one'\\n'backslash"]
        , testCase "escaped quote then unquoted" $
            parseArgs "echo \"inside\\\"literal_quote.\"outside\\\"" @?= ["echo", "inside\"literal_quote.outside\""]
        , testCase "cat with backslash in double-quoted paths" $
            parseArgs "cat /tmp/\"number 1\" /tmp/\"doublequote \\\" 2\" /tmp/\"backslash \\\\ 3\""
              @?= ["cat", "/tmp/number 1", "/tmp/doublequote \" 2", "/tmp/backslash \\ 3"]
        , testCase "backslash before letter is literal (both chars kept)" $
            parseArgs "echo \"hello\\nworld\"" @?= ["echo", "hello\\nworld"]
        , testCase "multiple escaped quotes" $
            parseArgs "echo \"\\\"hello\\\"\"" @?= ["echo", "\"hello\""]
        , testCase "backslash at end of double quotes" $
            parseArgs "echo \"trailing\\\\\"" @?= ["echo", "trailing\\"]
        ]
    , testGroup
        "challenge examples"
        [ testCase "echo shell hello" $ parseArgs "echo 'shell hello'" @?= ["echo", "shell hello"]
        , testCase "cat with two paths" $
            parseArgs "cat '/tmp/file name' '/tmp/file name with spaces'"
              @?= ["cat", "/tmp/file name", "/tmp/file name with spaces"]
        ]
    ]
