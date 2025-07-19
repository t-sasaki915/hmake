module Makefile.Parser.Internal
    ( spacesAndNewline
    , msnoc
    ) where

import           Data.Functor (($>))
import           Data.Text    (Text)
import           Text.Parsec

spacesAndNewline :: Parsec Text () ()
spacesAndNewline = skipMany (char ' ' <|> char '\t') *> newline $> ()

msnoc :: Monad m => m [a] -> m a -> m [a]
msnoc mxs mx = mxs >>= \xs -> mx >>= \x -> pure (xs <> [x])
