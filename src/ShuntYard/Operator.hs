module ShuntYard.Operator
    ( Operator (..)
    , operation
    , Precedence (..)
    , precedence
    )
where

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

operation :: (Fractional a) => Operator -> a -> a -> a
operation Plus = (+)
operation Minus = (-)
operation Times = (*)
operation Div = (/)

data Precedence = Low | High
    deriving (Show, Eq, Ord)

precedence :: Operator -> Precedence
precedence Plus = Low
precedence Minus = Low
precedence Times = High
precedence Div = High
