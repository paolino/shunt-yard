{-# LANGUAGE BlockArguments #-}

module ShuntYard.LexerTest where

import Prelude

import AssertList (assertEq, moduleTest)
import ShuntYard.Lexer (lexer)
import ShuntYard.Operator (Operator (..))
import ShuntYard.Token (Token (..))
import Test.HUnit (Test)

testLexer :: Test
testLexer = moduleTest "lexer" $ do
    assertEq
        do "lexer \"1\""
        do lexer "1"
        do [Double 1]
    assertEq
        do "lexer \"1.1\""
        do lexer "1.1"
        do [Double 1.1]
    assertEq
        do "lexer \"12\""
        do lexer "12"
        do [Double 12]
    assertEq
        do "lexer \"12.12\""
        do lexer "12.12"
        do [Double 12.12]
    assertEq
        do "lexer \"15\""
        do lexer "15"
        do [Double 15]
    assertEq
        do "lexer \"1 + 2\""
        do lexer "1 + 2"
        do [Double 1, Operator Plus, Double 2]
    assertEq
        do "lexer \"1 + 2 * 3\""
        do lexer "1 + 2 * 3"
        do [Double 1, Operator Plus, Double 2, Operator Times, Double 3]
    assertEq
        do "lexer \"(1 + 2) * 3\""
        do lexer "(1 + 2) * 3"
        do
            [ OpenParens
                , Double 1
                , Operator Plus
                , Double 2
                , ClosedParens
                , Operator Times
                , Double 3
                ]
    assertEq
        do "lexer \"5 - (3 * 2) + 1\""
        do lexer "5 - (3 * 2) + 1"
        do
            [ Double 5
                , Operator Minus
                , OpenParens
                , Double 3
                , Operator Times
                , Double 2
                , ClosedParens
                , Operator Plus
                , Double 1
                ]
    assertEq
        do "lexer \"(1 + 2) * (3 + 4)\""
        do lexer "(1 + 2) * (3 + 4)"
        do
            [ OpenParens
                , Double 1
                , Operator Plus
                , Double 2
                , ClosedParens
                , Operator Times
                , OpenParens
                , Double 3
                , Operator Plus
                , Double 4
                , ClosedParens
                ]
    assertEq
        do "lexer \"-1\""
        do lexer "-1"
        do [Double 0, Operator Minus, Double 1]
    assertEq
        do "lexer \"-1 + 2\""
        do lexer "-1 + 2"
        do [Double 0, Operator Minus, Double 1, Operator Plus, Double 2]
    assertEq
        do "lexer \"-(2)\""
        do lexer "-(2)"
        do [Double 0, Operator Minus, OpenParens, Double 2, ClosedParens]
    assertEq
        do "lexer \"(-2)\""
        do lexer "(-2)"
        do [OpenParens, Double 0, Operator Minus, Double 2, ClosedParens]
    assertEq
        do "lexer \"-(1 + 2)\""
        do lexer "-(1 + 2)"
        do
            [ Double 0
                , Operator Minus
                , OpenParens
                , Double 1
                , Operator Plus
                , Double 2
                , ClosedParens
                ]
    assertEq
        do "lexer \"(-1 + 2)\""
        do lexer "(-1 + 2)"
        do
            [ OpenParens
                , Double 0
                , Operator Minus
                , Double 1
                , Operator Plus
                , Double 2
                , ClosedParens
                ]
    assertEq
        do "parse \"2 * (-8 + 1)\""
        do lexer "2 * (-8 + 1)"
        do
            [ Double 2
                , Operator Times
                , OpenParens
                , Double 0
                , Operator Minus
                , Double 8
                , Operator Plus
                , Double 1
                , ClosedParens
                ]
