module Main (main) where

import qualified Data.Text       as Text
import           Makefile.Parser (parseMakefile)

main :: IO ()
main =
    readFile "HMakefile" >>= \makefileContent ->
        case parseMakefile (Text.pack makefileContent) of
            Right tokens -> print tokens
            Left err     -> error (show err)
