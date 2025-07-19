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

data TargetToken = TargetToken Text [Text] deriving (Show, Eq)

makefileParser :: Parser [TargetToken]
makefileParser = many $ optional comment *> target <* optional comment

target :: Parser TargetToken
target = do
    targetName <- dependency
    _          <- skipSpaces
    _          <- char ':'
    _          <- skipSpaces
    dependList <- dependencyList

    pure (TargetToken targetName dependList)

dependencyList :: Parser [Text]
dependencyList = try (newline $> []) <|> (char '[' *> spaces *> sepBy dependency (try (spaces *> char ',' <* spaces)) <* spaces <* char ']' <* spacesAndNewline)

dependency :: Parser Text
dependency = Text.pack <$> many1 (alphaNum <|> oneOf acceptableSymbols)

comment :: Parser Text
comment = Text.pack <$> (spaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
