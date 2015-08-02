{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module Smart where

import Prelude hiding ((>*), (<*))
import Data.Array.Accelerate.Type (IsFloating, NumType(..), numType)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Data.Array.Accelerate.Tuple (Tuple(..))
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import AST

diff :: (Eq t, Elt t, IsFloating t, Eq tx, Elt tx, IsFloating tx) => Exp t -> Exp tx -> Exp t
diff expf@(Exp f) expx@(Exp x) = case f ===== x of
                                  True -> (constant 1)
                                  False -> diff' f expx

-- Now we are free to ignore f == x .

diff' :: ( Elt tf, IsFloating tf, Num tf, Eq tf
         , Elt tx, IsFloating tx, Num tx, Eq tx)
            => PreExp Acc Exp tf
            -> Exp tx
            -> Exp tf

diff' tag@(Tag level) _ = Exp $ tag

diff' (Const cf) _ = constant 0

diff' (Tuple t) _ = constant 0 -- TODO

diff' (Prj _ _) _ = constant 0 -- TODO

diff' (IndexNil) _ = constant 0

diff' (IndexCons _ _) _ = constant 0

diff' (IndexHead _) _ = constant 0

diff' (IndexTail _) _ = constant 0

diff' (IndexAny) _ = constant 0

diff' (ToIndex _ _) _ = constant 0

diff' (FromIndex _ _) _ = constant 0

diff' (Cond c l r) dx = Exp $ Cond c dl dr where
  dl = diff l dx
  dr = diff r dx

diff' (While _ _ _) _ = constant 0 -- TODO

diff' (PrimConst _) _ = constant 0

diff' (PrimApp pf a) dx = diffprimfun pf a dx

diff' (Index _ _) _ = constant 0

diff' (LinearIndex _ _) _ = constant 0 -- TODO

diff' (Shape _) _ = constant 0

diff' (ShapeSize _) _ = constant 0

diff' (Intersect _ _) _ = constant 0 -- TODO

diff' (Foreign _ _ _) _ = constant 0 -- TODO



chainUnary :: ( IsNum tf, Elt tf, IsFloating tf, Eq tf
              , IsNum tx, Elt tx, IsFloating tx, Eq tx)
           => Exp tf
           -> Exp tf
           -> Exp tx
           -> Exp tf

chainUnary dexp a dx = dexp * (diff a dx)

chainBinary :: ( IsNum tf, Elt tf, IsFloating tf, Eq tf
               , IsNum tx, Elt tx, IsFloating tx, Eq tx)
            => Exp tf -> Exp tf
            -> Exp tf -> Exp tf
            -> Exp tx
            -> Exp tf
chainBinary dwrta1 dwrta2 a1 a2 dx = dwrta1 * (diff a1 dx) + dwrta2 * (diff a2 dx)

-- Problem: PrimFun (a -> b) appears in the AST and can be any type in theory
-- Although in practice, a and b will only be types that result from the data
-- constructors. We need to handle these differently, but you can't pattern
-- match based on type. If we naively use a class and instances, the compiler
-- will bork, since not all PrimFuns will be in the class, only the constructable
-- ones. We need this:
--
-- https://wiki.haskell.org/GHC/AdvancedOverlap
--


type family ArgT pf
type family ResultT pf
type instance ArgT (PrimFun (a -> b)) = a
type instance ResultT (PrimFun (a -> b)) = b

-- Outer class of all PrimFun s
-- TODO - if this does not work, use type families to specify types
class DifferentiatePrimFun pf where
  diffprimfun :: ( Elt (ArgT pf)
                 , Elt (ResultT pf)
                 , Elt tx, Eq tx, IsFloating tx
                 )
              => pf
              -> Exp (ArgT pf)
              -> Exp tx
              -> Exp (ResultT pf)

-- Inner class with flags
class DifferentiatePrimFun' flag pf where
  diffprimfun' :: ( Elt (ArgT pf)
                  , Elt (ResultT pf)
                  , Elt tx, Eq tx, IsFloating tx
--                  , IsFloating (ArgT pf)
--                  , IsFloating (ResultT pf)
--                  , IsFloating tx
                  )
               => flag
               -> pf
               -> Exp (ArgT pf)
               -> Exp tx
               -> Exp (ResultT pf)

instance (DifferentiatePrimFun' flag pf, TypesMatchPred pf flag) => DifferentiatePrimFun pf where
  diffprimfun = diffprimfun' (undefined::flag)

-- ...
class TypesMatchPred pf flag | pf -> flag where {}

-- Used if no matches

--instance TypeCast flag PFUseless => TypesMatchPred (PrimFun sig) flag
instance (flag ~ PFUseless) => TypesMatchPred pf flag
--{-# OVERLAPPABLE #-}

-- Useful instances
-- Unfortunately this won't work
--instance (IsNum t, Elt t, IsFloating t) => TypesMatchPred (PrimFun (t -> t)) PFUnaryFloatFloat
-- We have to use this:
instance TypesMatchPred (PrimFun (Float -> Float)) PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CFloat -> CFloat)) PFUnaryFloatFloat
--instance TypesMatchPred (PrimFun (Double -> Double)) PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CDouble -> CDouble)) PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (Double -> Double)) PFUnaryFloatFloat

-- Type level flags for classifying PrimFun s
data PFUnaryFloatFloat  -- Floating -> Floating
data PFBinaryFloatFloat -- (Floating, Floating ) -> Floating
data PFUnaryOtherFloat  -- Other -> Floating
data PFUseless          -- Other -> Other

-- Default:
instance (Num (ResultT pf)) => DifferentiatePrimFun' PFUseless pf where
  diffprimfun' _ (fun::pf) a dx = constant (69::ResultT pf)

-- In order of definition in Accelerate:
-- Some credit goes to Wolfram Alpha
instance (IsFloating a, Elt a, Eq a) => DifferentiatePrimFun' PFUnaryFloatFloat (PrimFun (a -> a)) where

  -- basic stuff
  diffprimfun' _ (PrimNeg ty) a dx = mkNeg $ diff a dx

  diffprimfun' _ (PrimAbs ty) a dx = chainUnary (signum a) a dx

  diffprimfun' _ (PrimSig ty) a _ = constant 0

  diffprimfun' _ (PrimRecip ty) a dx = chainUnary (a**(-2)) a dx

  -- trig
  diffprimfun' _ (PrimSin ty) a dx = chainUnary (cos a) a dx

  diffprimfun' _ (PrimCos ty) a dx = chainUnary (- sin a) a dx

  diffprimfun' _ (PrimTan ty) a dx = chainUnary ((cos a)**(-2)) a dx

  -- inverse trig
  diffprimfun' _ (PrimAsin ty) a dx = chainUnary (1/(sqrt (1 - a**2))) a dx

  diffprimfun' _ (PrimAcos ty) a dx = chainUnary (-1/(sqrt (1 - a**2))) a dx

  diffprimfun' _ (PrimAtan ty) a dx = chainUnary (1/(1+a**2)) a dx

  -- inverse hyperbolic
  diffprimfun' _ (PrimAsinh ty) a dx = chainUnary (1/(sqrt (1 + a**2))) a dx

  diffprimfun' _ (PrimAcosh ty) a dx = chainUnary (1/(sqrt (a**2 - 1))) a dx

  diffprimfun' _ (PrimAtanh ty) a dx = chainUnary (1/(1 - a**2)) a dx

  -- other important funcs
  diffprimfun' _ (PrimExpFloating ty) a dx = chainUnary a a dx

  diffprimfun' _ (PrimSqrt ty) a dx = chainUnary (0.5 * a** (-0.5)) a dx

  diffprimfun' _ (PrimLog ty) a dx = chainUnary (recip a) a dx

instance (IsFloating a, Elt a, Eq a) => DifferentiatePrimFun' PFBinaryFloatFloat (PrimFun ((a,a) -> a)) where

  -- DMAS
  diffprimfun' _ (PrimAdd ty) ta1a2 dx = chainBinary 1 1 a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimSub ty) ta1a2 dx = chainBinary 1 (-1) a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimMul ty) ta1a2 dx = chainBinary a2 a1 a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimFDiv ty) ta1a2 dx = chainBinary (1/a2) (-a1/a2**2) a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  -- O and inverse O
  diffprimfun' _ (PrimFPow ty) ta1a2 dx = chainBinary (a2*a1**(a2-1)) (log a1 * a1**a2) a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimLogBase ty) ta1a2 dx = chainBinary (-(log a2)/(a1*(log a1)**2)) (recip (log a1 * log a2)) a1 a2 dx where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimAtan2 ty) ta1a2 dx = (chainBinary (-a2) (-a1) a1 a2 dx)/(a1**2 + a2**2) where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimMax ty) ta1a2 dx = (a2 >* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
    (a1, a2) = untup2 ta1a2

  diffprimfun' _ (PrimMin ty) ta1a2 dx = (a2 <* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
    (a1, a2) = untup2 ta1a2

-- All functions that input non-floats and output floats must have a differential of 0 or be undifferentiable

instance (IsFloating tb, Elt tb, Eq tb,
          IsIntegral ta, Elt ta, Eq ta
          ) => DifferentiatePrimFun' PFUnaryOtherFloat (PrimFun (ta -> tb)) where
                 diffprimfun' _ (PrimFromIntegral t1 t2) _ _ = constant 0


class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

