{-# LANGUAGE BlockArguments #-}

module ShuntYard.ParserTest
    ( testParser
    )
where

import Prelude

import AssertList (assertEq, moduleTest)
import ShuntYard.Lexer (lexer)
import ShuntYard.Operator (Operator (..))
import ShuntYard.Parser (Item (..), parser)
import Test.HUnit (Test)

testParser :: Test
testParser = moduleTest "parser" do
    assertEq
        do "parse \"1 + 2\""
        do parser $ lexer "1 + 2"
        do [NumberI 1.0, NumberI 2.0, OpI Plus]
    assertEq
        do "parse \"1 + 2 * 3\""
        do parser $ lexer "1 + 2 * 3"
        do
            [ NumberI 1.0
                , NumberI 2.0
                , NumberI 3.0
                , OpI Times
                , OpI Plus
                ]
    assertEq
        do "parse \"3 * 2 + 4\""
        do parser $ lexer "3 * 2 + 4"
        do
            [ NumberI 3.0
                , NumberI 2.0
                , OpI Times
                , NumberI 4.0
                , OpI Plus
                ]
    assertEq
        do "parse \"5 - 3 * 2 + 1\""
        do parser $ lexer "5 - 3 * 2 + 1"
        do
            [ NumberI 5.0
                , NumberI 3.0
                , NumberI 2.0
                , OpI Times
                , OpI Minus
                , NumberI 1.0
                , OpI Plus
                ]
    assertEq
        do "parse \"(1 + 2) * 3\""
        do parser $ lexer "(1 + 2) * 3"
        do
            [ NumberI 1.0
                , NumberI 2.0
                , OpI Plus
                , NumberI 3.0
                , OpI Times
                ]
