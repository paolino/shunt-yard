{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module AssertList
    ( AssertList
    , assertEq
    , moduleTest
    )
where

import Prelude

import Control.Monad (void)
import Control.Monad.Operational
    ( Program
    , ProgramView (..)
    , ProgramViewT (..)
    , singleton
    , view
    )
import MonadicList (List, el, list)
import ShuntYard.Evaluator (evaluator)
import ShuntYard.Parser (Item (..), parser)
import Test.HUnit (Test (..), Testable (..), assertEqual, runTestTT)

type AssertList = List Test

assertEq :: (Eq a, Show a) => String -> a -> a -> AssertList ()
assertEq msg expected actual = el $ TestCase $ assertEqual msg expected actual

moduleTest :: String -> List Test a -> Test
moduleTest moduleName = TestLabel moduleName . TestList . list
