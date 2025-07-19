module Makefile.ParserSpec (makefileParserSpec) where

import           Test.Hspec        (Spec, describe, it)
import           Test.Hspec.Parsec (shouldParse)

import           Data.Text         (Text)
import           Text.Parsec       (ParseError, Parsec, eof, parse)

import           Makefile.Parser

makefileParserSpec :: Spec
makefileParserSpec = do
    describe "comment parser" $ do
        it "should parse a comment" $
            parseEof comment "-- COMMENT" `shouldParse` " COMMENT"

        it "should parse a comment with newline" $
            parseEof comment "-- COMMENT\n" `shouldParse` " COMMENT"

    describe "dependency list parser" $ do
        it "should parse a newline character" $
            parseEof dependencyList "\n" `shouldParse` []

        it "should parse an empty list" $
            parseEof dependencyList "[]\n" `shouldParse` []

        it "should parse an empty list with whitespaces" $
            parseEof dependencyList "[   ]     \n" `shouldParse` []

        it "should parse an empty list with newlines" $
            parseEof dependencyList "[\n]\n" `shouldParse` []

        it "should parse a list with single element" $
            parseEof dependencyList "[abc]\n" `shouldParse` ["abc"]

        it "should parse a list with single element and whitespaces" $
            parseEof dependencyList "[   abc  ]      \n" `shouldParse` ["abc"]

        it "should parse a list with single element and newlines" $
            parseEof dependencyList "[ abc\n]\n" `shouldParse` ["abc"]

        it "should parse a list with multiple elements" $
            parseEof dependencyList "[abc,def,ghi]\n" `shouldParse` ["abc", "def", "ghi"]

        it "should parse a list with multiple elements and whitespaces" $
            parseEof dependencyList "[  abc , def,  ghi   ]  \n" `shouldParse` ["abc", "def", "ghi"]

        it "should parse a list with multiple elements and newlines" $
            parseEof dependencyList "[ abc\n, def\n, ghi\n]\n" `shouldParse` ["abc", "def", "ghi"]

    describe "target parser" $ do
        it "should parse a target with no dependency list" $
            parseEof target "a.out:\n" `shouldParse` (TargetToken "a.out" [])

        it "should parse a target with no dependency list and whitespaces" $
            parseEof target "a.out  :      \n" `shouldParse` (TargetToken "a.out" [])

        it "should parse a target with a dependency list" $
            parseEof target "a.out: [main.o, sub.o]\n" `shouldParse` (TargetToken "a.out" ["main.o", "sub.o"])

        it "should parse a target with a dependency list and whitespaces" $
            parseEof target "a.out   :  [   main.o , sub.o   ] \n" `shouldParse` (TargetToken "a.out" ["main.o", "sub.o"])

        it "should parse a target with a dependency list containing whitespaces" $
            parseEof target "a.out: [ main.o\n, sub.o\n]\n" `shouldParse` (TargetToken "a.out" ["main.o", "sub.o"])

    describe "makefile parser" $ do
        it "should parse a Makefile with a target" $
            parseEof makefileParser "a.out: [main.o, sub.o]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"]]

        it "should parse a Makefile with multiple targets" $
            parseEof makefileParser "a.out: [main.o, sub.o]\nmain.o: [main.c, header.h]\nsub.o: [sub.c]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"], TargetToken "main.o" ["main.c", "header.h"], TargetToken "sub.o" ["sub.c"]]

        it "should parse a Makefile with a comment" $
            parseEof makefileParser "-- COMMENT1" `shouldParse` []

        it "should parse a Makefile with a target surrounded by comments" $
            parseEof makefileParser "-- COMMENT1\na.out: [main.o, sub.o]\n-- COMMENT2" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"]]

        it "should parse a Makefile with a target and a comment before the target" $
            parseEof makefileParser "-- COMMENT\na.out: [main.o, sub.o]\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"]]

        it "should parse a Makefile with a target and a comment after the target" $
            parseEof makefileParser "a.out: [main.o, sub.o]\n-- COMMENT" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"]]

        it "should parse a Makefile with multiple targets and comments" $
            parseEof makefileParser "-- Binary\na.out: [main.o, sub.o]\n-- Object file of main\nmain.o: [main.c, header.h]\n"  `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"], TargetToken "main.o" ["main.c", "header.h"]]

        it "should parse a Makefile with newlines" $
            parseEof makefileParser "\na.out: [main.o, sub.o]\n\nmain.o: [main.c, header.h]\n\nsub.o: [sub.c]\n\n" `shouldParse` [TargetToken "a.out" ["main.o", "sub.o"], TargetToken "main.o" ["main.c", "header.h"], TargetToken "sub.o" ["sub.c"]]

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
