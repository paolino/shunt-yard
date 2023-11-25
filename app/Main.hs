import Data.List (intercalate)
import ShuntYard.Evaluator (evaluator)
import ShuntYard.Lexer (lexer)
import ShuntYard.Parser (parser)
import System.Environment (getArgs)

main :: IO ()
main = do
    as <- getArgs
    print $ evaluator . parser . lexer $ intercalate " " as
