module Makefile.Parser.Internal
    ( Parser
    , skipSpaces
    , skipSpaces1
    , manyTill1
    , acceptableSymbols
    , monadConcat
    ) where

import           Data.Text   (Text)
import           Text.Parsec

type Parser a = Parsec Text () a

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

skipSpaces1 :: Parser ()
skipSpaces1 = skipMany1 (char ' ' <|> char '\t')

manyTill1 :: Parser a -> Parser b -> Parser [a]
manyTill1 p end = (:) <$> p <*> manyTill p end

acceptableSymbols :: [Char]
acceptableSymbols = "`~!@#$%^&*()_+{}|<>?-=;'./"

monadConcat :: (Monad m, Monoid a) => m a -> m a -> m a
monadConcat = liftA2 (<>)
