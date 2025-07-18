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

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
