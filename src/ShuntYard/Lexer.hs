{-# LANGUAGE LambdaCase #-}

module ShuntYard.Lexer
    ( lexer
    , Token (..)
    , Operator (..)
    , Precedence (..)
    , operation
    , precedence
    )
where

import Prelude

import Data.Char (ord)

-- arithmetic operators
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

-- arithmetic tokens
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

-- this is removing digits
compressNatural :: [Token] -> [Token]
compressNatural = foldr f []
  where
    f (Digit x) (Natural m y : xs) = Natural (10 * m) (10 * x + y) : xs
    f (Digit x) xs = Natural 10 x : xs
    f x xs = x : xs

-- this is removing the dot, why we cannot remove Naturals already ?
mkDouble :: [Token] -> [Token]
mkDouble = foldr f []
  where
    f (Natural _ x) (Dot : Natural m y : xs) =
        Double (fromIntegral x + fromIntegral y / fromIntegral m) : xs
    f x xs = x : xs

rmNatural :: [Token] -> [Token]
rmNatural = map $ \case
    Natural _ x -> Double $ fromIntegral x
    x -> x

tokenize :: String -> [Token]
tokenize = fmap $ \case
    '(' -> OpenParens
    ')' -> ClosedParens
    '+' -> Operator Plus
    '-' -> Operator Minus
    '*' -> Operator Times
    '/' -> Operator Div
    ' ' -> Space
    '.' -> Dot
    x ->
        if x `elem` ['0' .. '9']
            then Digit $ ord x - ord '0'
            else error "unexpected character"

rmSpaces :: [Token] -> [Token]
rmSpaces = filter (/= Space)

lexer :: String -> [Token]
lexer =
    withFakeStart fixUnaryMinus
        . rmNatural
        . mkDouble
        . compressNatural
        . rmSpaces
        . tokenize

withFakeStart :: ([Token] -> [Token]) -> [Token] -> [Token]
withFakeStart f = tail . f . (OpenParens :)

fixUnaryMinus :: [Token] -> [Token]
fixUnaryMinus [] = []
fixUnaryMinus (OpenParens : Operator Minus : xs) =
    OpenParens : Double 0 : Operator Minus : fixUnaryMinus xs
fixUnaryMinus (x : xs) = x : fixUnaryMinus xs
