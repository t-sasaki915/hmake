module Makefile.Parser.Internal (msnoc) where

msnoc :: Monad m => m [a] -> m a -> m [a]
msnoc mxs mx = mxs >>= \xs -> mx >>= \x -> pure (xs <> [x])
