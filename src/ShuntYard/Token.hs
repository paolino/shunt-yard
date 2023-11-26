module ShuntYard.Token
    ( Token (..)
    )
where

import ShuntYard.Operator (Operator)

data Token
    = OpenParens
    | ClosedParens
    | Digit Int
    | Natural Int Int
    | Double Double
    | Dot
    | Operator Operator
    | Space
    deriving (Show, Eq)
