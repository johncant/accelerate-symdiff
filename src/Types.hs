{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Types where


import Data.Typeable
import Data.Array.Accelerate.Array.Sugar (Elt, Arrays, Vector)
import Data.Array.Accelerate.Array.Sugar (Foreign(..))
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun)
import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Tuple (Atuple(..), Tuple(..), IsTuple, TupleRepr, fromTuple, toTuple, TupleIdx(..))

data HFalse
data HTrue


data WithRespectTo t1 t2 where
-- Variable id
  WithRespectTo :: (Elt t1) => Int -> WithRespectTo t1 t1
  WithRespectToA :: (Arrays t1) => Int -> WithRespectTo t1 t1
  deriving (Typeable)


instance Foreign WithRespectTo where
  strForeign (WithRespectTo n) = "WithRespectTo " ++ show n

matchMarkers :: (Foreign f, Foreign g) => f a b -> g c d -> Bool
matchMarkers f1 f2 = wrt && eq where
  wrt = take 14 (strForeign f1) == "WithRespectTo "
  eq = strForeign f2 == strForeign f1


class (Elt a, Elt dx, IsFloating dx) => DifferentiableTupleRepr a dx
instance (Elt dx, IsFloating dx) => DifferentiableTupleRepr () dx
instance (Differentiable a dx, DifferentiableTupleRepr t dx) => DifferentiableTupleRepr (t, a) dx

class (Elt a, Elt dx, IsFloating dx) => Differentiable a dx
instance {-# OVERLAPPING #-} (Elt a, IsFloating a) => Differentiable a a
instance {-# OVERLAPPING #-} ( IsTuple a
         , Elt a
         , DifferentiableTupleRepr (TupleRepr a) dx)
         => Differentiable a dx


data Tools = Tools { diffF :: forall a dx. (Differentiable a dx) => (Exp dx -> Exp a) -> Exp dx -> Exp a
                   , diffT :: forall a dx. (Differentiable a dx) => Exp a -> Exp dx -> Exp a
--                   , gradF :: forall a dx. (Acc (Vector dx) -> Exp a) -> Acc (Vector dx) -> Acc (Vector a)
--                   , gradT :: forall a dx. Exp a -> Acc (Vector dx) -> Acc (Vector a)
--                   , mapdiffF :: Differentiate => (Acc (Vector dx) -> Acc (Vector a)) -> Acc (Vector dx) -> Acc (Vector a)
--                   , mapdiffT :: Differentiate => Acc (Vector a) -> Acc (Vector dx) -> Acc (Vector a)
--                   , jacobianF :: Differentiate => (Acc (Vector dx) -> Acc (Vector a)) -> Acc (Vector dx) -> Acc (Array (DIM0:.DIM1:.DIM2) a)
--                   , jacobianT :: Differentiate => Exp a -> Exp dx -> Exp a
                   , diffprimfun :: forall a b dx. (Elt a, Elt dx, IsFloating dx) => PrimFun (b -> a) -> Exp b -> Exp dx -> Exp a
                   , diff' :: forall a b dx. (Elt a, IsFloating a) => PreExp Acc Exp a -> Exp a -> Exp a
                   }

type family ScalarType a
type instance ScalarType (a, a) = a
type instance ScalarType Float = Float
type instance ScalarType Double = Double
--type instance ScalarType CFloat = CFloat
--type instance ScalarType CDouble = CDouble


