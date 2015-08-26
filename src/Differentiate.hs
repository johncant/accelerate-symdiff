{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Differentiate where

import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun)
import Data.Array.Accelerate.Array.Sugar (Elt, Arrays, Vector)
import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Tuple (Atuple(..), Tuple(..), IsTuple, TupleRepr)




class ToolsT t where
  diffF :: forall a dx. (DifferentiateValue a dx) => t -> (Exp dx -> Exp a) -> Exp dx -> Exp a
  diffT :: forall a dx. (DifferentiateValue a dx) => t -> Exp a -> Exp dx -> Exp a
--                   , gradF :: forall a dx. (Acc (Vector dx) -> Exp a) -> Acc (Vector dx) -> Acc (Vector a)
--                   , gradT :: forall a dx. Exp a -> Acc (Vector dx) -> Acc (Vector a)
--                   , mapdiffF :: Differentiate => (Acc (Vector dx) -> Acc (Vector a)) -> Acc (Vector dx) -> Acc (Vector a)
--                   , mapdiffT :: Differentiate => Acc (Vector a) -> Acc (Vector dx) -> Acc (Vector a)
--                   , jacobianF :: Differentiate => (Acc (Vector dx) -> Acc (Vector a)) -> Acc (Vector dx) -> Acc (Array (DIM0:.DIM1:.DIM2) a)
--                   , jacobianT :: Differentiate => Exp a -> Exp dx -> Exp a
  diffprimfun :: forall a b. (Elt a, Elt b, IsFloating b) => t -> PrimFun (a -> b) -> Exp a -> Exp b -> Exp b
  diff' :: forall a. (Elt a, IsFloating a) => t -> PreExp Acc Exp a -> Exp a -> Exp a






class (Elt a, Elt dx, IsFloating dx) => DifferentiateTupleRepr a dx where
  difftr :: (ToolsT tk) => tk -> Tuple Exp a -> Exp dx -> Tuple Exp a
--  gradtr :: tk -> Tuple Exp a -> Acc (Vector dx) -> Tuple Exp a

instance (Elt dx, IsFloating dx) => DifferentiateTupleRepr () dx where
  difftr _ NilTup _ = NilTup
--  gradtr _ NilTup x = NilAtup

instance (DifferentiateValue a dx, DifferentiateTupleRepr t dx) => DifferentiateTupleRepr (t, a) dx where
  difftr tk (SnocTup t v) x = (difftr tk t x) `SnocTup` (diffT tk v x)
--  gradtr tk (SnocAtup t v) x = (gradtr tk t x) `SnocTup` (diffT tk v x)





class (Elt a, Elt dx, IsFloating dx) => DifferentiateValue a dx where
  diffVT :: (ToolsT tk) => tk -> Exp a -> Exp dx -> Exp a
--  gradT :: tk -> Exp a -> Acc (Vector dx) -> Exp a

class (Elt a, Elt dx, IsFloating dx) => DifferentiateTuple a dx where
  diffTT :: (ToolsT tk) => tk -> Exp a -> Exp dx -> Exp a
--  gradT :: tk -> Exp a -> Acc (Vector dx) -> Exp a

instance {-# OVERLAPPING #-} (DifferentiateValue a a) => DifferentiateValue a a where
  diffVT tk (Exp fpreexp) x = diff' tk fpreexp x
--  gradT tk (Exp fpreexp) x = grad' tk fpreexp x

instance {-# OVERLAPPING #-} ( IsTuple a
         , Elt a
         , DifferentiateTupleRepr (TupleRepr a) dx)
         => DifferentiateTuple a dx where
  diffTT tk (Exp preF) x = case preF of
                            Tuple t ->
                              Exp $ Tuple $ difftr tk t x
--  gradT tk (Exp preF) x = case preF of
--                            Tuple t ->
--                              Exp $ Tuple $ gradtr tk t x


