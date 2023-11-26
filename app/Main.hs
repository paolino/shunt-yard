{-# LANGUAGE BlockArguments #-}

import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.List (lines)
import ShuntYard.Evaluator (evaluator)
import ShuntYard.Lexer (lexer)
import ShuntYard.Parser (parser)

main :: IO ()
main = do
    xs <- lines <$> getContents
    forM_ xs $ \x ->
        catch
            do print . evaluator . parser . lexer $ x
            do \e -> print (e :: SomeException)
