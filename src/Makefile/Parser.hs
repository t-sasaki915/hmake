module Makefile.Parser
    ( MakefileToken (..)
    , VariableValueToken (..)
    , parseMakefile
    , makefileParser
    , target
    , variable
    , list
    , targetName
    , variableName
    , variableValue
    , textValueToken
    , integerValueToken
    , floatValueToken
    , boolValueToken
    , listValueToken
    , targetReferenceToken
    , comment
    ) where

import           Control.Monad            (void)
import           Data.Char                (intToDigit)
import           Data.Functor             (($>))
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Text.Parsec

import           Makefile.Parser.Internal

data MakefileToken = TargetToken Text [Text] [Text]
                   | VariableToken Bool Text VariableValueToken
                   deriving (Show, Eq)

data VariableValueToken = TextValueToken Text
                        | IntegerValueToken Integer
                        | FloatValueToken Float
                        | BoolValueToken Bool
                        | ListValueToken [VariableValueToken]
                        | TargetReferenceToken Text
                        deriving (Show, Eq)

parseMakefile :: Text -> Either ParseError [MakefileToken]
parseMakefile = parse makefileParser ""

makefileParser :: Parser [MakefileToken]
makefileParser = skipMeaningless *> many ((try variable <|> target) <* skipMeaningless) <* eof

target :: Parser MakefileToken
target = do
    targetName' <- targetName
    _           <- skipSpaces *> char ':' <* skipSpaces
    dependList  <- (lookAhead newline $> []) <|> list targetName
    _           <- newline
    _           <- skipMeaningless
    content     <- many (try commandLine <* skipMeaningless)

    pure (TargetToken targetName' dependList content)

commandLine :: Parser Text
commandLine = Text.pack <$> (skipSpaces1 *> manyTill1 anyChar (try (void newline) <|> eof))

variable :: Parser MakefileToken
variable = do
    isConst  <- isJust <$> optionMaybe (string "const")
    _        <- skipSpaces
    varName  <- variableName
    _        <- skipSpaces *> string ":=" <* skipSpaces

    VariableToken isConst varName <$> variableValue

list :: Parser a -> Parser [a]
list p = char '[' *> spaces *> p `sepBy` try (spaces *> char ',' <* spaces) <* spaces <* char ']'

targetName :: Parser Text
targetName = Text.pack <$> many1 (alphaNum <|> oneOf acceptableSymbols)

variableName :: Parser Text
variableName = Text.pack <$> many1 (oneOf (['A'..'Z'] <> map intToDigit [0..9] <> ['_']))

variableValue :: Parser VariableValueToken
variableValue = try textValueToken <|> try floatValueToken <|> try integerValueToken <|> try boolValueToken <|> try listValueToken <|> targetReferenceToken

textValueToken :: Parser VariableValueToken
textValueToken = TextValueToken . Text.pack <$> (char '"' *> manyTill anyChar (char '"'))

integerValueToken :: Parser VariableValueToken
integerValueToken = IntegerValueToken . read <$> many1 digit

floatValueToken :: Parser VariableValueToken
floatValueToken = FloatValueToken . read <$> (many1 digit `monadConcat` (char '.' $> ".") `monadConcat` many1 digit)

boolValueToken :: Parser VariableValueToken
boolValueToken = BoolValueToken <$> (try (string "True" $> True) <|> (string "False" $> False))

listValueToken :: Parser VariableValueToken
listValueToken = ListValueToken <$> list variableValue

targetReferenceToken :: Parser VariableValueToken
targetReferenceToken = TargetReferenceToken <$> targetName

skipMeaningless :: Parser ()
skipMeaningless = skipMany (try (void comment) <|> void (char '\n'))

comment :: Parser Text
comment = Text.pack <$> (skipSpaces *> string "--" *> manyTill anyChar (try (void newline <|> eof)))
