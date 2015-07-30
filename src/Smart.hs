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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module Smart (diff) where

import Data.Array.Accelerate.Type (IsFloating, NumType(..), numType)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Data.Array.Accelerate.Tuple (Tuple(..))
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate

diff :: (Elt t, IsFloating t, Elt tx, IsFloating tx) => Exp t -> Exp tx -> Exp t
diff expf@(Exp f) expx@(Exp x) = case f of
                                  x -> (constant 1)
                                  _ -> diff' f expx

-- Now we are free to ignore f == x .

diff' :: ( Elt tf, IsFloating tf, Num tf
         , Elt tx, IsFloating tx, Num tx)
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

diff' (PrimApp pf x) _ = constant 0 -- TODO - AST

diff' (Index _ _) _ = constant 0

diff' (LinearIndex _ _) _ = constant 0 -- TODO

diff' (Shape _) _ = constant 0

diff' (ShapeSize _) _ = constant 0

diff' (Intersect _ _) _ = constant 0 -- TODO

diff' (Foreign _ _ _) _ = constant 0 -- TODO



chainUnary :: ( IsNum tf, Elt tf, IsFloating tf
              , IsNum tx, Elt tx, IsFloating tx)
           => Exp tf
           -> Exp tf
           -> Exp tx
           -> Exp tf

chainUnary dexp a dx = mkMul dexp (diff a dx) where

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
class (Elt tx) => DifferentiatePrimFun pf tx where
  diffprimfun :: ( Elt (ArgT pf)
                 , Elt (ResultT pf)
                 , Elt tx
                 )
              => pf
              -> Exp (ArgT pf)
              -> Exp tx
              -> Exp (ResultT pf)

-- Inner class with flags
class (Elt tx) => DifferentiatePrimFun' flag pf tx where
  diffprimfun' :: ( Elt (ArgT pf)
                  , Elt (ResultT pf)
                  , Elt tx
--                  , IsFloating (ArgT pf)
--                  , IsFloating (ResultT pf)
--                  , IsFloating tx
                  )
               => flag
               -> pf
               -> Exp (ArgT pf)
               -> Exp tx
               -> Exp (ResultT pf)

instance (DifferentiatePrimFun' flag pf tx, TypesMatchPred pf tx flag) => DifferentiatePrimFun pf tx where
  diffprimfun = diffprimfun' (undefined::flag)

-- ...
class TypesMatchPred pf tx flag | pf tx -> flag where {}

-- Used if no matches

instance TypeCast flag PFUseless => TypesMatchPred pf tx flag

-- Useful instances
-- Unfortunately this won't work
--instance (IsNum t, Elt t, IsFloating t) => TypesMatchPred (PrimFun (t -> t)) PFUnaryFloatFloat
-- We have to use this:
instance TypesMatchPred (PrimFun (Float -> Float)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CFloat -> CFloat)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (Double -> Double)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CDouble -> CDouble)) tx PFUnaryFloatFloat

-- Type level flags for classifying PrimFun s
data PFUnaryFloatFloat  -- Floating -> Floating
data PFBinaryFloatFloat -- (Floating, Floating ) -> Floating
data PFOtherFloat       -- Other -> Floating
data PFUseless          -- Other -> Other

-- impl
instance (IsFloating a, Elt a, IsFloating tx, Elt tx) => DifferentiatePrimFun' PFUnaryFloatFloat (PrimFun (a -> a)) tx where

  diffprimfun' _ (PrimNeg ty) a dx = mkNeg $ diff a dx

  diffprimfun' _ (PrimAbs ty) a dx = chainUnary (signum a) a dx

  diffprimfun' _ (PrimSig ty) a _ = constant 0

  diffprimfun' _ (PrimRecip ty) a dx = chainUnary (a**(-2)) a dx

  diffprimfun' _ (PrimSin ty) a dx = chainUnary (cos a) a dx

  diffprimfun' _ (PrimCos ty) a dx = chainUnary (- sin a) a dx

  diffprimfun' _ (PrimTan ty) a dx = chainUnary ((cos a)**(-2)) a dx

  diffprimfun' _ (PrimAsin ty) a dx = chainUnary (sqrt (1 - a**2)) a dx


instance (Elt a) => DifferentiatePrimFun' PFUseless pf a where
  diffprimfun' _ fun a dx = undefined

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x
