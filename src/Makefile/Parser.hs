module Makefile.Parser
    ( TargetToken (..)
    , makefileParser
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

import           Makefile.Parser.Internal

type Parser a = Parsec Text () a

data TargetToken = TargetToken Text [Text] deriving (Show, Eq)

makefileParser :: Parser [TargetToken]
makefileParser = many $ optional comment *> target <* optional comment

target :: Parser TargetToken
target = do
    targetName <- Text.pack <$> manyTill alphaNum (lookAhead (spaces <|> void (char ':')))
    _          <- spaces
    _          <- char ':'
    _          <- spaces
    dependList <- dependencyList

    pure (TargetToken targetName dependList)

dependencyList :: Parser [Text]
dependencyList = try noDependencyList
             <|> try emptyDependencyList
             <|> nonEmptyDependencyList
    where
        noDependencyList       = newline $> []
        emptyDependencyList    = char '[' *> spaces *> char ']' *> spacesAndNewline $> []
        nonEmptyDependencyList = char '[' *> spaces *> (many (spaces *> dependency <* spaces <* char ',' <* spaces) `msnoc` dependency) <* spaces <* char ']' <* spacesAndNewline

dependency :: Parser Text
dependency = Text.pack <$> manyTill alphaNum (lookAhead (spaces <|> void (char ',') <|> void (char ']')))

comment :: Parser Text
comment = Text.pack <$> (spaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
