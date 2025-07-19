module Makefile.Parser.Internal
    ( Parser
    , skipSpaces
    , acceptableSymbols
    ) where

import           Data.Text    (Text)
import           Text.Parsec

type Parser a = Parsec Text () a

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

acceptableSymbols :: [Char]
acceptableSymbols = "`~!@#$%^&*()_+{}|<>?-=;'./"
