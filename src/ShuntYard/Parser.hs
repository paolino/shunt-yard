{-# LANGUAGE GADTs #-}

module ShuntYard.Parser
    ( Stack (..)
    , Item (..)
    , Operand
    , parser
    )
where

import Prelude hiding (fail)

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
import Debug.Trace (traceShow)
import ShuntYard.Lexer
    ( Operator (..)
    , Token (..)
    , precedence
    )

data Operand = Negative Operand | Value Double
    deriving (Show, Eq)

data Item = NumberI Double | OpI Operator
    deriving (Show, Eq)

data OperatorWithParens
    = OpOpenParens
    | Op Operator
    | OpClosedParens
    deriving (Show, Eq)

-- a parser for a stream of tokens
data ParseT l where
    Consume :: ParseT (Maybe Token)
    PushItem :: Item -> ParseT ()
    PushOperator :: OperatorWithParens -> ParseT ()
    PopOperator :: ParseT (Maybe OperatorWithParens)

type Parse a = Program ParseT a

consume :: Parse (Maybe Token)
consume = singleton Consume

pushItem :: Item -> Parse ()
pushItem = singleton . PushItem

pushOperator :: OperatorWithParens -> Parse ()
pushOperator = singleton . PushOperator

popOperator :: Parse (Maybe OperatorWithParens)
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
                    migrateOperators $ Op op
                    pushOperator $ Op op
                    parseP
                OpenParens -> do
                    pushOperator OpOpenParens
                    parseP
                ClosedParens -> do
                    migrateOperators OpClosedParens
                    parseP
                op -> error $ "unexpected token: " <> show op

migrateOperators :: OperatorWithParens -> Parse ()
migrateOperators opCurWithParens = void $ runMaybeT $ forever $ do
    opStackWithParens <- MaybeT popOperator
    case opStackWithParens of
        OpOpenParens -> mzero
        Op opStack -> case opCurWithParens of
            Op opCur -> do
                if precedence opStack >= precedence opCur
                    then do
                        lift $ pushItem $ OpI opStack
                    else do
                        lift $ pushOperator $ Op opStack
                        mzero
            OpClosedParens -> do
                lift $ pushItem $ OpI opStack
            OpOpenParens ->
                error "migrateOperators: unexpected operator: OpOpenParens"
        _ ->
            error "migrateOperators: unexpected operator: ClosedParens"

data Stack = Stack [Item] [OperatorWithParens]
    deriving (Show, Eq)

runParse :: Parse a -> [Token] -> (a, Stack)
runParse f is = go (Stack [] []) is f
  where
    go s xs f' = interpret s xs $ view f'
    interpret :: Stack -> [Token] -> ProgramView ParseT a -> (a, Stack)
    interpret s _xs (Return x) = (x, s)
    interpret s [] (Consume :>>= k) =
        go s [] $ k Nothing
    interpret s (x : xs) (Consume :>>= k) =
        go s xs $ k $ Just x
    interpret (Stack xs os) is' (PushItem x :>>= k) =
        go (Stack (x : xs) os) is' $ k ()
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
parser ts = case snd $ runParse parseP ts of
    Stack xs os -> reverse xs <> fmap onlyOps os

onlyOps :: OperatorWithParens -> Item
onlyOps (Op op) = OpI op
onlyOps op = error $ "onlyOps: unexpected operator" <> show op
