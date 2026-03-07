module Main where

import Shell.Parser (Builtin (..), Command (..), CommandBody (..), Redirect (..), RedirectMode (..), parseArgs, parseCommand)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Shell" [parseArgsTests, redirectTests]

redirectTests :: TestTree
redirectTests =
    testGroup
        "redirect parsing"
        [ testGroup
            "stdout redirect"
            [ testCase "echo with >" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello > file.txt"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Just (Redirect "file.txt" Overwrite)
                er @?= Nothing
            , testCase "echo with 1>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello 1> file.txt"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Just (Redirect "file.txt" Overwrite)
                er @?= Nothing
            , testCase "no redirect" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Nothing
                er @?= Nothing
            , testCase "external with redirect" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls /tmp > out.txt"
                assertBody b (External "ls" ["/tmp"])
                r @?= Just (Redirect "out.txt" Overwrite)
                er @?= Nothing
            , testCase "redirect with quoted filename" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hi > 'my file.txt'"
                assertBody b (BuiltinCmd (Echo "hi"))
                r @?= Just (Redirect "my file.txt" Overwrite)
                er @?= Nothing
            , testCase "empty command is preserved" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand ""
                assertBody b Empty
                r @?= Nothing
                er @?= Nothing
            ]
        , testGroup
            "stderr redirect"
            [ testCase "echo with 2>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello 2> errors.txt"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Nothing
                er @?= Just (Redirect "errors.txt" Overwrite)
            , testCase "external with 2>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls nonexistent 2> /tmp/err.md"
                assertBody b (External "ls" ["nonexistent"])
                r @?= Nothing
                er @?= Just (Redirect "/tmp/err.md" Overwrite)
            , testCase "cat with 2>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "cat file1 nonexistent 2> /tmp/quz/quz.md"
                assertBody b (External "cat" ["file1", "nonexistent"])
                r @?= Nothing
                er @?= Just (Redirect "/tmp/quz/quz.md" Overwrite)
            , testCase "2> with quoted filename" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls bad 2> 'my errors.txt'"
                assertBody b (External "ls" ["bad"])
                r @?= Nothing
                er @?= Just (Redirect "my errors.txt" Overwrite)
            ]
        , testGroup
            "stdout append redirect"
            [ testCase "echo with >>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello >> file.txt"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Just (Redirect "file.txt" Append)
                er @?= Nothing
            , testCase "echo with 1>>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hello 1>> file.txt"
                assertBody b (BuiltinCmd (Echo "hello"))
                r @?= Just (Redirect "file.txt" Append)
                er @?= Nothing
            , testCase "external with >>" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls /tmp >> out.txt"
                assertBody b (External "ls" ["/tmp"])
                r @?= Just (Redirect "out.txt" Append)
                er @?= Nothing
            , testCase ">> with quoted filename" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "echo hi >> 'my file.txt'"
                assertBody b (BuiltinCmd (Echo "hi"))
                r @?= Just (Redirect "my file.txt" Append)
                er @?= Nothing
            ]
        , testGroup
            "both stdout and stderr redirects"
            [ testCase "stdout and stderr to different files" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls foo 1> out.txt 2> err.txt"
                assertBody b (External "ls" ["foo"])
                r @?= Just (Redirect "out.txt" Overwrite)
                er @?= Just (Redirect "err.txt" Overwrite)
            , testCase "stderr before stdout" $ do
                let Command{body = b, stdoutRedirect = r, stderrRedirect = er} = parseCommand "ls foo 2> err.txt > out.txt"
                assertBody b (External "ls" ["foo"])
                r @?= Just (Redirect "out.txt" Overwrite)
                er @?= Just (Redirect "err.txt" Overwrite)
            ]
        ]

assertBody :: CommandBody -> CommandBody -> Assertion
assertBody (BuiltinCmd (Echo a)) (BuiltinCmd (Echo b)) = a @?= b
assertBody (External a as) (External b bs) = do a @?= b; as @?= bs
assertBody Empty Empty = return ()
assertBody got expected = assertFailure $ "Expected " ++ showBody expected ++ " but got " ++ showBody got

showBody :: CommandBody -> String
showBody (BuiltinCmd (Echo s)) = "Echo " ++ show s
showBody (External cmd args) = "External " ++ show cmd ++ " " ++ show args
showBody Empty = "Empty"
showBody _ = "CommandBody"

parseArgsTests :: TestTree
parseArgsTests =
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
