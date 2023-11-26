{-# LANGUAGE GADTs #-}

module ShuntYard.Evaluator
    ( evaluator
    )
where

import Prelude hiding (fail)

import Control.Monad (forever, void)
import Control.Monad.Operational
    ( Program
    , ProgramView
    , ProgramViewT ((:>>=))
    , singleton
    , view
    )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import ShuntYard.Operator (operation)
import ShuntYard.Parser (Item (..))

data EvalT l where
    ConsumeE :: EvalT (Maybe Item)
    PushValue :: Double -> EvalT ()
    PopValue :: EvalT (Maybe Double)

type Eval a = Program EvalT a

consumeE :: Eval (Maybe Item)
consumeE = singleton ConsumeE

pushValue :: Double -> Eval ()
pushValue = singleton . PushValue

popValue :: Eval (Maybe Double)
popValue = singleton PopValue

evalP :: Eval ()
evalP = void $ runMaybeT $ forever $ do
    x <- MaybeT consumeE
    case x of
        NumberI y ->
            lift $ pushValue y
        OpI op -> do
            y <- MaybeT popValue
            z <- MaybeT popValue
            lift $ pushValue $ operation op z y

type Result = ([Double], [Item])

runEval :: Eval a -> [Item] -> Result
runEval = go []
  where
    go :: [Double] -> Eval a -> [Item] -> Result
    go xs f is = eval xs is $ view f
    eval :: [Double] -> [Item] -> ProgramView EvalT a -> Result
    eval xs (i : is) (ConsumeE :>>= k) = go xs (k $ Just i) is
    eval xs is (PushValue x :>>= k) = go (x : xs) (k ()) is
    eval (x : xs) is (PopValue :>>= k) = go xs (k $ Just x) is
    eval xs is _ = (xs, is)

evaluator :: [Item] -> Double
evaluator is = case fst $ runEval evalP is of
    [] -> error "empty stack"
    [x] -> x
    xs -> error $ "result has leftovers" <> show xs
