module Makefile.Parser.Internal
    ( Parser
    , skipSpaces
    , acceptableSymbols
    , monadConcat
    ) where

import           Data.Text   (Text)
import           Text.Parsec

type Parser a = Parsec Text () a

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

acceptableSymbols :: [Char]
acceptableSymbols = "`~!@#$%^&*()_+{}|<>?-=;'./"

monadConcat :: (Monad m, Monoid a) => m a -> m a -> m a
monadConcat = liftA2 (<>)
