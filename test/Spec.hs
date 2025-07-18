import           Test.Hspec          (hspec)

import           Makefile.ParserSpec (makefileParserSpec)

main :: IO ()
main = hspec $ do
    makefileParserSpec
