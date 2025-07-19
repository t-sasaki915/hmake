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

        it "should parse a list with single element" $
            parseEof dependencyList "[abc]\n" `shouldParse` ["abc"]

        it "should parse a list with single element and whitespaces" $
            parseEof dependencyList "[   abc  ]      \n" `shouldParse` ["abc"]

        it "should parse a list with multiple elements" $
            parseEof dependencyList "[abc,def,ghi]\n" `shouldParse` ["abc", "def", "ghi"]

        it "should parse a list with multiple elements and whitespaces" $
            parseEof dependencyList "[  abc , def,  ghi   ]  \n" `shouldParse` ["abc", "def", "ghi"]

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
