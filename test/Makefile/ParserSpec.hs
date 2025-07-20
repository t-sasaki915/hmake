module Makefile.ParserSpec (makefileParserSpec) where

import           Test.Hspec        (Spec, describe, it)
import           Test.Hspec.Parsec (shouldFailOn, shouldParse)

import           Data.Text         (Text)
import           Text.Parsec       (ParseError, Parsec, digit, eof, parse)

import           Makefile.Parser

makefileParserSpec :: Spec
makefileParserSpec = do
    describe "comment parser" $ do
        it "should parse a comment" $
            parseEof comment "-- COMMENT" `shouldParse` " COMMENT"

        it "should parse a comment with newline" $
            parseEof comment "-- COMMENT\n" `shouldParse` " COMMENT"

    describe "list parser" $ do
        it "should parse an empty list" $
            parseEof (list digit) "[]" `shouldParse` []

        it "should parse an empty list with whitespaces" $
            parseEof (list digit) "[   ]" `shouldParse` []

        it "should parse an empty list with newlines" $
            parseEof (list digit) "[\n]" `shouldParse` []

        it "should parse a list with single element" $
            parseEof (list digit) "[1]" `shouldParse` ['1']

        it "should parse a list with single element and whitespaces" $
            parseEof (list digit) "[   1  ]" `shouldParse` ['1']

        it "should parse a list with single element and newlines" $
            parseEof (list digit) "[ 1\n]" `shouldParse` ['1']

        it "should parse a list with multiple elements" $
            parseEof (list digit) "[1,2,3]" `shouldParse` ['1', '2', '3']

        it "should parse a list with multiple elements and whitespaces" $
            parseEof (list digit) "[  1 , 2,  3   ]" `shouldParse` ['1', '2', '3']

        it "should parse a list with multiple elements and newlines" $
            parseEof (list digit) "[ 1\n, 2\n, 3\n]" `shouldParse` ['1', '2', '3']

    describe "target parser" $ do
        it "should parse a target with no dependency list" $
            parseEof target "a.out:\n" `shouldParse` TargetToken "a.out" [] []

        it "should parse a target with no dependency list and whitespaces" $
            parseEof target "a.out  :      \n" `shouldParse` TargetToken "a.out" [] []

        it "should parse a target with a dependency list" $
            parseEof target "a.out: [main.o, sub.o]\n" `shouldParse` TargetToken "a.out" ["main.o", "sub.o"] []

        it "should parse a target with a dependency list and whitespaces" $
            parseEof target "a.out   :  [   main.o , sub.o   ]\n" `shouldParse` TargetToken "a.out" ["main.o", "sub.o"] []

        it "should parse a target with a dependency list containing whitespaces" $
            parseEof target "a.out: [ main.o\n, sub.o\n]\n" `shouldParse` TargetToken "a.out" ["main.o", "sub.o"] []

        it "should parse a target with actions" $
            parseEof target "a.out: [main.o, sub.o]\n    cc -o a.out main.o sub.o -lm" `shouldParse` TargetToken "a.out" ["main.o", "sub.o"] ["cc -o a.out main.o sub.o -lm"]

        it "should parse a target with multiple actions" $
            parseEof target "build:\n    mkdir -p build\n    cabal v2-build" `shouldParse` TargetToken "build" [] ["mkdir -p build", "cabal v2-build"]

    describe "text value parser" $ do
        it "should parse a text value" $
            parseEof textValueToken "\"AA BB CC !!! &&&\"" `shouldParse` TextValueToken "AA BB CC !!! &&&"

        it "should not parse a malformed text value" $
            parseEof textValueToken `shouldFailOn` "\"AA"

    describe "integer value parser" $ do
        it "should parse an integer value" $
            parseEof integerValueToken "123456" `shouldParse` IntegerValueToken 123456

        it "should not parse a malformed integer value" $
            parseEof integerValueToken `shouldFailOn` "1234.56"

    describe "float value parser" $ do
        it "should parse a float value" $
            parseEof floatValueToken "1234.56" `shouldParse` FloatValueToken 1234.56

        it "should not parse a malformed float value" $
            parseEof floatValueToken `shouldFailOn` "123456"

    describe "bool value parser" $ do
        it "should parse True" $
            parseEof boolValueToken "True" `shouldParse` BoolValueToken True

        it "should parse False" $
            parseEof boolValueToken "False" `shouldParse` BoolValueToken False

        it "should not parse a malformed bool value" $
            parseEof boolValueToken `shouldFailOn` "Tru"

    describe "list value parser" $ do
        it "should parse a list value" $
            parseEof listValueToken "[\"abc\", 123, True]" `shouldParse` ListValueToken [TextValueToken "abc", IntegerValueToken 123, BoolValueToken True]

        it "should parse a list value with whitespaces" $
            parseEof listValueToken "[  \"abc\"  , 123,   True ]" `shouldParse` ListValueToken [TextValueToken "abc", IntegerValueToken 123, BoolValueToken True]

        it "should parse a list value with newlines" $
            parseEof listValueToken "[ \"abc\"\n, 123\n, True\n]" `shouldParse` ListValueToken [TextValueToken "abc", IntegerValueToken 123, BoolValueToken True]

        it "should not parse a malformed list" $
            parseEof listValueToken `shouldFailOn` "["

    describe "target reference parser" $ do
        it "should parse a target reference" $
            parseEof targetReferenceToken "main.o" `shouldParse` TargetReferenceToken "main.o"

        it "should not parse a malformed target reference" $
            parseEof targetReferenceToken `shouldFailOn` "[],"

    describe "variable parser" $ do
        it "should parse a variable with text value" $
            parseEof variable "BUILD_DIR := \"build\"" `shouldParse` VariableToken False "BUILD_DIR" (TextValueToken "build")

        it "should parse a variable with integer value and whitespaces" $
            parseEof variable "THRESHOLD   :=    8" `shouldParse` VariableToken False "THRESHOLD" (IntegerValueToken 8)

        it "should parse a constant variable with float value" $
            parseEof variable "const PI := 3.14" `shouldParse` VariableToken True "PI" (FloatValueToken 3.14)

        it "should parse a constant variable with list value and whitespaces" $
            parseEof variable "const    PHONY   :=    [ build  ,  clean ]" `shouldParse` VariableToken True "PHONY" (ListValueToken [TargetReferenceToken "build", TargetReferenceToken "clean"])

    describe "makefile parser" $ do
        it "should parse a Makefile with a target" $
            parseEof makefileParser "a.out: [main.o, sub.o]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] []]

        it "should parse a Makefile with multiple targets" $
            parseEof makefileParser "a.out: [main.o, sub.o]\nmain.o: [main.c, header.h]\nsub.o: [sub.c]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] [], TargetToken "main.o" ["main.c", "header.h"] [], TargetToken "sub.o" ["sub.c"] []]

        it "should parse a Makefile with a comment" $
            parseEof makefileParser "-- COMMENT1" `shouldParse` []

        it "should parse a Makefile with a target surrounded by comments" $
            parseEof makefileParser "-- COMMENT1\na.out: [main.o, sub.o]\n-- COMMENT2" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] []]

        it "should parse a Makefile with a target and a comment before the target" $
            parseEof makefileParser "-- COMMENT\na.out: [main.o, sub.o]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] []]

        it "should parse a Makefile with a target and a comment after the target" $
            parseEof makefileParser "a.out: [main.o, sub.o]\n-- COMMENT" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] []]

        it "should parse a Makefile with multiple targets and comments" $
            parseEof makefileParser "-- Binary\na.out: [main.o, sub.o]\n-- Object file of main\nmain.o: [main.c, header.h]\n"  `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] [], TargetToken "main.o" ["main.c", "header.h"] []]

        it "should parse a Makefile with newlines" $
            parseEof makefileParser "\na.out: [main.o, sub.o]\n\nmain.o: [main.c, header.h]\n\nsub.o: [sub.c]\n\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] [], TargetToken "main.o" ["main.c", "header.h"] [], TargetToken "sub.o" ["sub.c"] []]

        it "should parse a Makefile with a target and multiple variables" $
            parseEof makefileParser "const BUILD_DIR := \"build\"\nRETRY := True\na.out: [main.o, sub.o]\n" `shouldParse` [VariableToken True "BUILD_DIR" (TextValueToken "build"), VariableToken False "RETRY" (BoolValueToken True), TargetToken "a.out" ["main.o", "sub.o"] []]

        it "should parse a Makefile with multiple targets and actions" $
            parseEof makefileParser "a.out: [main.o, sub.o]\n    cc -o a.out main.o sub.o -lm\nmain.o: [main.c, header.h]\n    cc -c main.c\nsub.o: [sub.c]\n    cc -c sub.c" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"] ["cc -o a.out main.o sub.o -lm"], TargetToken "main.o" ["main.c", "header.h"] ["cc -c main.c"], TargetToken "sub.o" ["sub.c"] ["cc -c sub.c"]]

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
