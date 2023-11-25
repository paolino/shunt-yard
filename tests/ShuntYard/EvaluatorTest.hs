{-# LANGUAGE BlockArguments #-}

module ShuntYard.EvaluatorTest
    ( testEvaluator
    )
where

import Prelude

import AssertList (AssertList, assertEq, moduleTest)
import MonadicList (List, el, list)
import ShuntYard.Evaluator (evaluator)
import ShuntYard.Lexer (Operator (..), Token (..), lexer)
import ShuntYard.Parser (Item (..), parser)
import Test.HUnit (Test (..), Testable (..), assertEqual, runTestTT)

testEvaluator :: Test
testEvaluator = moduleTest "evaluator" $ do
    assertEq
        do "eval \"1\""
        do evaluator $ parser $ lexer "1"
        do 1
    assertEq
        do "eval \"1.1\""
        do evaluator $ parser $ lexer "1.1"
        do 1.1
    assertEq
        do "eval \"12\""
        do evaluator $ parser $ lexer "12"
        do 12
    assertEq
        do "eval \"12.12\""
        do evaluator $ parser $ lexer "12.12"
        do 12.12
    assertEq
        do "eval \"1 + 2\""
        do evaluator $ parser $ lexer "1 + 2"
        do 3
    assertEq
        do "eval \"1 + 2 * 3\""
        do evaluator $ parser $ lexer "1 + 2 * 3"
        do 7
    assertEq
        do "eval \"3 * 2 + 4\""
        do evaluator $ parser $ lexer "3 * 2 + 4"
        do 10
    assertEq
        do "eval \"5 - 3 * 2 + 1\""
        do evaluator $ parser $ lexer "5 - 3 * 2 + 1"
        do 0
    assertEq
        do "eval \"(1 + 2) * 3\""
        do evaluator $ parser $ lexer "(1 + 2) * 3"
        do 9
    assertEq
        do "eval \"(1 + 2) * (3 + 4)\""
        do evaluator $ parser $ lexer "(1 + 2) * (3 + 4)"
        do 21
    assertEq
        do "eval \"(1)\""
        do evaluator $ parser $ lexer "(1)"
        do 1
    assertEq
        do "eval \"((1))\""
        do evaluator $ parser $ lexer "((1))"
        do 1
    assertEq
        do "eval \"((1 + 2))\""
        do evaluator $ parser $ lexer "((1 + 2))"
        do 3
    assertEq
        do "eval \"((1 + 2) * (3))\""
        do evaluator $ parser $ lexer "((1 + 2) * (3))"
        do 9
    assertEq
        do "eval \"-1\""
        do evaluator $ parser $ lexer "-1"
        do -1
    assertEq
        do "eval \"-1 + 2\""
        do evaluator $ parser $ lexer "-1 + 2"
        do 1
    assertEq
        do "eval \"-(2)\""
        do evaluator $ parser $ lexer "-(2)"
        do -2
    assertEq
        do "eval \"(-2)\""
        do evaluator $ parser $ lexer "(-2)"
        do -2
    assertEq
        do "eval \"-(1 + 2)\""
        do evaluator $ parser $ lexer "-(1 + 2)"
        do -3
    assertEq
        do "eval \"(-1 + 2)\""
        do evaluator $ parser $ lexer "(-1 + 2)"
        do 1
    assertEq
        do "eval \"15 * 4\""
        do evaluator $ parser $ lexer "15 * 4"
        do 60
    assertEq
        do "eval \"((15 * 4) - (7 + 3)) / 2\""
        do evaluator $ parser $ lexer "((15 * 4) - (7 + 3)) / 2"
        do 25