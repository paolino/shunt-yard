{-# LANGUAGE GADTs #-}

module ShuntYard.Parser
    ( Stack (..)
    , Item (..)
    , Operand
    , parser
    )
where

import Prelude

import Control.Monad (MonadPlus (..), forever, void)
import Control.Monad.Operational
    ( Program
    , ProgramView
    , ProgramViewT (Return, (:>>=))
    , singleton
    , view
    )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import ShuntYard.Operator (Operator, precedence)
import ShuntYard.Token (Token (..))

data Operand = Negative Operand | Value Double
    deriving (Show, Eq)

data Item = NumberI Double | OpI Operator
    deriving (Show, Eq)

data OperatorOrOpen
    = OpOpenParens
    | OpO Operator
    deriving (Show, Eq)

data OperatorOrClosed
    = OpC Operator
    | OpClosedParens
    deriving (Show, Eq)

-- a parser for a stream of tokens
data ParseT l where
    Consume :: ParseT (Maybe Token)
    PushItem :: Item -> ParseT ()
    PushOperator :: OperatorOrOpen -> ParseT ()
    PopOperator :: ParseT (Maybe OperatorOrOpen)

type Parse a = Program ParseT a

consume :: Parse (Maybe Token)
consume = singleton Consume

pushItem :: Item -> Parse ()
pushItem = singleton . PushItem

pushOperator :: OperatorOrOpen -> Parse ()
pushOperator = singleton . PushOperator

popOperator :: Parse (Maybe OperatorOrOpen)
popOperator = singleton PopOperator

parseP :: Parse ()
parseP = do
    mx <- consume
    case mx of
        Nothing -> pure ()
        Just x ->
            case x of
                Double y -> do
                    pushItem $ NumberI y
                    parseP
                Operator op -> do
                    migrateOperators $ OpC op
                    pushOperator $ OpO op
                    parseP
                OpenParens -> do
                    pushOperator OpOpenParens
                    parseP
                ClosedParens -> do
                    migrateOperators OpClosedParens
                    parseP
                op -> error $ "unexpected token: " <> show op

migrateOperators :: OperatorOrClosed -> Parse ()
migrateOperators opCurWithParens = void $ runMaybeT $ forever $ do
    opStackWithParens <- MaybeT popOperator
    case opStackWithParens of
        OpOpenParens -> do
            case opCurWithParens of
                OpClosedParens -> pure ()
                OpC opCur -> lift $ pushOperator OpOpenParens
            mzero
        OpO opStack -> case opCurWithParens of
            OpC opCur -> do
                if precedence opStack >= precedence opCur
                    then do
                        lift $ pushItem $ OpI opStack
                    else do
                        lift $ pushOperator $ OpO opStack
                        mzero
            OpClosedParens -> do
                lift $ pushItem $ OpI opStack

data Stack = Stack [Item] [OperatorOrOpen]
    deriving (Show, Eq)

runParse :: Parse () -> [Token] -> Stack
runParse f is = go (Stack [] []) is f
  where
    go s xs f' = interpret s xs $ view f'
    interpret :: Stack -> [Token] -> ProgramView ParseT () -> Stack
    interpret s _xs (Return {}) = s
    interpret s [] (Consume :>>= k) =
        go s [] $ k Nothing
    interpret s (x : xs) (Consume :>>= k) =
        go s xs $ k $ Just x
    interpret s is' (PushItem x :>>= k) =
        let Stack xs os = go s is' $ k ()
        in Stack (x : xs) os
    interpret (Stack xs os) is' (PushOperator o :>>= k) =
        go (Stack xs (o : os)) is' $ k ()
    interpret (Stack xs []) is' (PopOperator :>>= k) =
        go
            (Stack xs [])
            is'
            $ k Nothing
    interpret (Stack xs (o : os)) is' (PopOperator :>>= k) =
        go (Stack xs os) is' $ k $ Just o

parser :: [Token] -> [Item]
parser ts = case runParse parseP ts of
    Stack xs os -> xs <> fmap onlyOps os

onlyOps :: OperatorOrOpen -> Item
onlyOps (OpO op) = OpI op
onlyOps OpOpenParens = error "onlyOps: unexpected OpOpenParens"
