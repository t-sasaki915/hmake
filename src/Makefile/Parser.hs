module Makefile.Parser
    ( MakefileToken (..)
    , VariableValueToken (..)
    , makefileParser
    , target
    , variable
    , dependencyList
    , targetName
    , variableName
    , variableValue
    , textValueToken
    , integerValueToken
    , floatValueToken
    , boolValueToken
    , listValueToken
    , comment
    ) where

import           Control.Monad            (void)
import           Data.Char                (intToDigit, isSpace)
import           Data.Functor             (($>))
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Text.Parsec

import           Makefile.Parser.Internal

data MakefileToken = TargetToken Text [Text]
                   | VariableToken Bool Text VariableValueToken
                   deriving (Show, Eq)

data VariableValueToken = TextValueToken Text
                        | IntegerValueToken Integer
                        | FloatValueToken Float
                        | BoolValueToken Bool
                        | ListValueToken [VariableValueToken]
                        deriving (Show, Eq)

makefileParser :: Parser [MakefileToken]
makefileParser = skipMeaningless *> many (target <* skipMeaningless) <* eof

target :: Parser MakefileToken
target = do
    targetName' <- targetName
    _           <- skipSpaces *> char ':' <* skipSpaces

    TargetToken targetName' <$> dependencyList

variable :: Parser MakefileToken
variable = do
    isConst  <- isJust <$> optionMaybe (string "const")
    _        <- skipSpaces
    varName  <- variableName
    _        <- skipSpaces *> string ":=" <* skipSpaces

    VariableToken isConst varName <$> variableValue

dependencyList :: Parser [Text]
dependencyList = try (newline $> [])
             <|> (char '[' *> spaces *> targetName `sepBy` try (spaces *> char ',' <* spaces) <* spaces <* char ']' <* skipSpaces <* newline)

targetName :: Parser Text
targetName = Text.pack <$> many1 (alphaNum <|> oneOf acceptableSymbols)

variableName :: Parser Text
variableName = Text.pack <$> many1 (oneOf (['A'..'Z'] <> map intToDigit [0..9] <> ['_']))

variableValue :: Parser VariableValueToken
variableValue = try textValueToken <|> try floatValueToken <|> try integerValueToken <|> try boolValueToken <|> listValueToken

textValueToken :: Parser VariableValueToken
textValueToken = TextValueToken . Text.pack <$> (char '"' *> manyTill anyChar (char '"'))

integerValueToken :: Parser VariableValueToken
integerValueToken = IntegerValueToken . read <$> many1 digit

floatValueToken :: Parser VariableValueToken
floatValueToken = FloatValueToken . read <$> (many1 digit `monadConcat` (char '.' $> ".") `monadConcat` many1 digit)

boolValueToken :: Parser VariableValueToken
boolValueToken = BoolValueToken <$> (try (string "True" $> True) <|> (string "False" $> False))

listValueToken :: Parser VariableValueToken
listValueToken = ListValueToken <$> (char '[' *> spaces *> variableValue `sepBy` try (spaces *> char ',' <* spaces) <* spaces <* char ']')

skipMeaningless :: Parser ()
skipMeaningless = skipMany (void (satisfy isSpace) <|> void newline <|> void (try comment))

comment :: Parser Text
comment = Text.pack <$> (spaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
