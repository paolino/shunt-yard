import Prelude

import Control.Monad (void)
import MonadicList (el, list)
import ShuntYard.EvaluatorTest (testEvaluator)
import ShuntYard.LexerTest (testLexer)
import ShuntYard.ParserTest (testParser)
import Test.HUnit (Test (..), Testable (..), runTestTTAndExit)

main :: IO ()
main = void
    $ runTestTTAndExit
    $ TestList
    $ list
    $ do
        el testLexer
        el testParser
        el testEvaluator
