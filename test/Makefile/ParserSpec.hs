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

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
