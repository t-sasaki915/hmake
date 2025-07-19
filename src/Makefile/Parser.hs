module Makefile.Parser
    ( TargetToken (..)
    , makefileParser
    , target
    , dependencyList
    , targetName
    , comment
    ) where

import           Control.Monad            (void)
import           Data.Char                (isSpace)
import           Data.Functor             (($>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Text.Parsec

import           Makefile.Parser.Internal

data TargetToken = TargetToken Text [Text] deriving (Show, Eq)

makefileParser :: Parser [TargetToken]
makefileParser = skipMeaningless *> many (target <* skipMeaningless) <* eof

target :: Parser TargetToken
target = do
    targetName' <- targetName
    _           <- skipSpaces *> char ':' <* skipSpaces
    dependList  <- dependencyList

    pure (TargetToken targetName' dependList)

dependencyList :: Parser [Text]
dependencyList = try (newline $> []) <|> (char '[' *> spaces *> targetName `sepBy` try (spaces *> char ',' <* spaces) <* spaces <* char ']' <* skipSpaces <* newline)

targetName :: Parser Text
targetName = Text.pack <$> many1 (alphaNum <|> oneOf acceptableSymbols)

skipMeaningless :: Parser ()
skipMeaningless = skipMany (void (satisfy isSpace) <|> void newline <|> void (try comment))

comment :: Parser Text
comment = Text.pack <$> (spaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
