{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module MonadicList
    ( List
    , el
    , list
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

data ListT e a where
    Cons :: e -> ListT e ()

type List e = Program (ListT e)

el :: e -> List e ()
el = singleton . Cons

list :: List e a -> [e]
list = go . view
  where
    go :: ProgramView (ListT e) a -> [e]
    go (Return _) = []
    go (Cons x :>>= k) = x : list (k ())
