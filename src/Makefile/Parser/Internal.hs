module Makefile.Parser.Internal
    ( spacesAndNewline
    , acceptableSymbols
    ) where

import           Data.Functor (($>))
import           Data.Text    (Text)
import           Text.Parsec

spacesAndNewline :: Parsec Text () ()
spacesAndNewline = skipMany (char ' ' <|> char '\t') *> newline $> ()

acceptableSymbols :: [Char]
acceptableSymbols = "`~!@#$%^&*()_+{}|<>?-=;'./"
