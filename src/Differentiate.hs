{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Differentiate where

import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.Array.Sugar (Elt, Arrays, Vector)
import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Tuple (Atuple(..), Tuple(..), IsTuple, TupleRepr, fromTuple, toTuple, TupleIdx(..))
import Types (Tools(..))
import qualified Types as T


class (Elt a, Elt dx, IsFloating dx) => DifferentiateTupleRepr a dx where
  difftr :: Tools -> Tuple Exp a -> Exp dx -> Tuple Exp a
--  gradtr :: Tools -> Tuple Exp a -> Acc (Vector dx) -> Tuple Exp a

instance (Elt dx, IsFloating dx) => DifferentiateTupleRepr () dx where
  difftr _ NilTup x = NilTup
--  gradtr _ NilTup x = NilAtup

instance (Differentiate a dx, DifferentiateTupleRepr t dx) => DifferentiateTupleRepr (t, a) dx where
  difftr tk (SnocTup t v) x = (difftr tk t x) `SnocTup` (T.diffT tk v x)
--  gradtr tk (SnocAtup t v) x = (gradtr tk t x) `SnocTup` (T.diffT tk v x)

class (Elt a, Elt dx, IsFloating dx) => Differentiate a dx where
  diffT :: Tools -> Exp a -> Exp dx -> Exp a
--  gradT :: Tools -> Exp a -> Acc (Vector dx) -> Exp a

instance {-# OVERLAPPING #-} (Elt a, IsFloating a) => Differentiate a a where
  diffT tk (Exp fpreexp) x = diff' tk fpreexp x
--  gradT tk (Exp fpreexp) x = grad' tk fpreexp x

instance {-# OVERLAPPING #-} ( IsTuple a
         , Elt a
         , DifferentiateTupleRepr (TupleRepr a) dx)
         => Differentiate a dx where
  diffT tk (Exp preF) x = case preF of
                            Tuple t ->
                              Exp $ Tuple $ difftr tk t x
--  gradT tk (Exp preF) x = case preF of
--                            Tuple t ->
--                              Exp $ Tuple $ gradtr tk t x
