module Makefile.Parser
    ( MakefileToken (..)
    , target
    , dependencyList
    , dependency
    , comment
    ) where

import           Control.Monad            (void)
import           Data.Functor             (($>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Text.Parsec

import           Makefile.Parser.Internal (msnoc)

type Parser a = Parsec Text () a

data MakefileToken = TargetToken Text [Text]
                   | CommentToken Text

target :: Parser MakefileToken
target = do
    targetName <- Text.pack <$> manyTill alphaNum (lookAhead (spaces <|> void (char ':')))
    _          <- spaces
    _          <- char ':'
    _          <- spaces
    dependList <- dependencyList

    pure (TargetToken targetName dependList)

dependencyList :: Parser [Text]
dependencyList = try emptyDependencyList
             <|> try nonEmptyDependencyList
             <|> noDependencyList
    where
        noDependencyList       = newline $> []
        emptyDependencyList    = char '[' >> spaces >> char ']' >> newline $> []
        nonEmptyDependencyList = char '[' *> spaces *> (many (spaces *> dependency <* spaces <* char ',' <* spaces) `msnoc` dependency) <* spaces <* char ']' <* newline

dependency :: Parser Text
dependency = Text.pack <$> manyTill alphaNum (lookAhead (spaces <|> void (char ',') <|> void (char ']')))

comment :: Parser MakefileToken
comment = CommentToken . Text.pack <$> (spaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
