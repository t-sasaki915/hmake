module Makefile.Parser.Internal
    ( Parser
    , spacesAndNewline
    , skipSpaces
    , acceptableSymbols
    ) where

import           Data.Functor (($>))
import           Data.Text    (Text)
import           Text.Parsec

type Parser a = Parsec Text () a

spacesAndNewline :: Parser ()
spacesAndNewline = skipSpaces *> newline $> ()

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

acceptableSymbols :: [Char]
acceptableSymbols = "`~!@#$%^&*()_+{}|<>?-=;'./"
