{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
                                  True -> (constant 1.0)
                                  False -> diff' f expx

-- Now we are free to ignore f == x .

diff' :: ( Elt tf, IsFloating tf, Eq tf
         , Elt tx, IsFloating tx, Eq tx)
            => PreExp Acc Exp tf
            -> Exp tx
            -> Exp tf

diff' tag@(Tag level) _ = Exp $ tag

diff' (Const cf) _ = constant 10

diff' (Tuple t) _ = constant 11 -- tup d0 d1 where

diff' (Prj _ _) _ = constant 12 -- TODO

diff' (IndexNil) _ = constant 13

diff' (IndexCons _ _) _ = constant 14

diff' (IndexHead _) _ = constant 15

diff' (IndexTail _) _ = constant 16

diff' (IndexAny) _ = constant 17

diff' (ToIndex _ _) _ = constant 18

diff' (FromIndex _ _) _ = constant 19

diff' (Cond c l r) dx = Exp $ Cond c dl dr where
  dl = diff l dx
  dr = diff r dx

diff' (While _ _ _) _ = constant 20 -- TODO

diff' (PrimConst _) _ = constant 21

diff' pa@(PrimApp pf a) dx = diffprimapp pf a dx -- TODO TODO TODO

diff' (Index _ _) _ = constant 22

diff' (LinearIndex _ _) _ = constant 23 -- TODO

diff' (Shape _) _ = constant 24

diff' (ShapeSize _) _ = constant 25

diff' (Intersect _ _) _ = constant 26 -- TODO

diff' (Foreign _ _ _) _ = constant 27 -- TODO


diffprimapp :: (Elt a, Elt b, Eq b, IsFloating b, Eq tx, Elt tx, IsFloating tx, DifferentiatePrimFun (PrimFun (a -> b)))
            => PrimFun (a -> b) -> Exp a -> Exp tx -> Exp b
diffprimapp pf a dx = diffprimfun pf a dx


chainUnary :: ( Elt tf, IsFloating tf, Eq tf
              , Elt tx, IsFloating tx, Eq tx)
           => Exp tf
           -> Exp tf
           -> Exp tx
           -> Exp tf

chainUnary dexp a dx = dexp * (diff a dx)

chainBinary :: (Elt tf, IsFloating tf, Eq tf
               ,Elt tx, IsFloating tx, Eq tx)
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

--type family TypeMatchResult pf where
--  TypeMatchResult (PrimFun (Float -> Float)) = PFUnaryFloatFloat
--  TypeMatchResult (PrimFun (CFloat -> CFloat)) = PFUnaryFloatFloat
--  TypeMatchResult (PrimFun (Double -> Double)) = PFUnaryFloatFloat
--  TypeMatchResult (PrimFun (CDouble -> CDouble)) = PFUnaryFloatFloat
--
--  TypeMatchResult (PrimFun ((Float, Float) -> Float)) = PFBinaryFloatFloat
--  TypeMatchResult (PrimFun ((CFloat, CFloat) -> CFloat)) = PFBinaryFloatFloat
--  TypeMatchResult (PrimFun ((Double, Double) -> Double)) = PFBinaryFloatFloat
--  TypeMatchResult (PrimFun ((CDouble, CDouble) -> CDouble)) = PFBinaryFloatFloat
--
--  TypeMatchResult (PrimFun (a -> Float)) = PFUnaryOtherFloat
--  TypeMatchResult (PrimFun (a -> CFloat)) = PFUnaryOtherFloat
--  TypeMatchResult (PrimFun (a -> Double)) = PFUnaryOtherFloat
--  TypeMatchResult (PrimFun (a -> CDouble)) = PFUnaryOtherFloat
--
--  TypeMatchResult (PrimFun (a -> a)) = PFUseless



-- In order of definition in Accelerate:
-- Some credit goes to Wolfram Alpha
diffprimfunFF :: (IsFloating a, Elt a, Eq a, IsFloating tx, Elt tx, Eq tx)
              => (PrimFun (a -> a))
              -> Exp a
              -> Exp tx
              -> Exp a

-- basic stuff
diffprimfunFF (PrimNeg ty) a dx = mkNeg $ diff a dx

diffprimfunFF (PrimAbs ty) a dx = chainUnary (signum a) a dx

diffprimfunFF (PrimSig ty) a _ = constant 30

diffprimfunFF (PrimRecip ty) a dx = chainUnary (a**(-2)) a dx

-- trig
diffprimfunFF (PrimSin ty) a dx = chainUnary (cos a) a dx

diffprimfunFF (PrimCos ty) a dx = chainUnary (- sin a) a dx

diffprimfunFF (PrimTan ty) a dx = chainUnary ((cos a)**(-2)) a dx

-- inverse trig
diffprimfunFF (PrimAsin ty) a dx = chainUnary (1/(sqrt (1 - a**2))) a dx

diffprimfunFF (PrimAcos ty) a dx = chainUnary (-1/(sqrt (1 - a**2))) a dx

diffprimfunFF (PrimAtan ty) a dx = chainUnary (1/(1+a**2)) a dx

-- inverse hyperbolic
diffprimfunFF (PrimAsinh ty) a dx = chainUnary (1/(sqrt (1 + a**2))) a dx

diffprimfunFF (PrimAcosh ty) a dx = chainUnary (1/(sqrt (a**2 - 1))) a dx

diffprimfunFF (PrimAtanh ty) a dx = chainUnary (1/(1 - a**2)) a dx

-- other important funcs
diffprimfunFF (PrimExpFloating ty) a dx = chainUnary a a dx

diffprimfunFF (PrimSqrt ty) a dx = chainUnary (0.5 * a** (-0.5)) a dx

diffprimfunFF (PrimLog ty) a dx = chainUnary (recip a) a dx




-- Binary funcs
diffprimfunTFF :: (IsFloating a, Elt a, Eq a, IsFloating tx, Elt tx, Eq tx)
               => PrimFun ((a,a) -> a)
               -> Exp (a, a)
               -> Exp tx
               -> Exp a

-- DMAS
diffprimfunTFF (PrimAdd ty) ta1a2 dx = chainBinary 1 1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimSub ty) ta1a2 dx = chainBinary 1 (-1) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMul ty) ta1a2 dx = chainBinary a2 a1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimFDiv ty) ta1a2 dx = chainBinary (1/a2) (-a1/a2**2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

-- O and inverse O
diffprimfunTFF (PrimFPow ty) ta1a2 dx = chainBinary (a2*a1**(a2-1)) (log a1 * a1**a2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimLogBase ty) ta1a2 dx = chainBinary (-(log a2)/(a1*(log a1)**2)) (recip (log a1 * log a2)) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimAtan2 ty) ta1a2 dx = (chainBinary (-a2) (-a1) a1 a2 dx)/(a1**2 + a2**2) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMax ty) ta1a2 dx = (a2 >* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMin ty) ta1a2 dx = (a2 <* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

-- All functions that input non-floats and output floats must have a differential of 0 or be undifferentiable

diffprimFunOF :: (IsFloating tb, Elt tb, Eq tb,
                  IsIntegral ta, Elt ta, Eq ta,
                  IsFloating tx, Elt tx, Eq tx
                  )
              => PrimFun (ta -> tb) -> Exp ta -> Exp tx -> Exp tb
diffprimFunOF _ _ _ = constant 63

-- Unify
-- Couldn't get AdvancedOverlap from the wiki to work for my case

-- This could be a problem: pf here allows the return type to be anything
-- Whereas it's constrained elsewhere to be Elt, Eq, IsFloating
class (Elt (PfTA pf), Eq (PfTA pf), IsFloating (PfTA pf), Elt (PfTB pf), Eq (PfTB pf), IsFloating (PfTB pf)) => PrimFunDifferentiableUnary pf where
  diffprimfun''''' :: (Eq tx, Elt tx, IsFloating tx) => pf -> Exp (PfTA pf) -> Exp tx -> Exp (PfTB pf)

instance PrimFunDifferentiableUnary (PrimFun (Float -> Float)) where
  diffprimfun''''' _ _ _ = constant 72
instance PrimFunDifferentiableUnary (PrimFun (Double -> Double)) where
  diffprimfun''''' _ _ _ = constant 72
instance PrimFunDifferentiableUnary (PrimFun (CFloat -> CFloat)) where
  diffprimfun''''' _ _ _ = constant 72
instance PrimFunDifferentiableUnary (PrimFun (CDouble -> CDouble)) where
  diffprimfun''''' _ _ _ = constant 72
-- instance PrimFunDifferentiableUnary (PrimFun ((Float, Float) -> Float)) where
--   diffprimfun''''' _ _ _ = constant 72
-- instance PrimFunDifferentiableUnary (PrimFun ((Double, Double) -> Double)) where
--   diffprimfun''''' _ _ _ = constant 72
-- instance PrimFunDifferentiableUnary (PrimFun ((CFloat, CFloat) -> CFloat)) where
--   diffprimfun''''' _ _ _ = constant 72
-- instance PrimFunDifferentiableUnary (PrimFun ((CDouble, CDouble) -> CDouble)) where
--   diffprimfun''''' _ _ _ = constant 72

-- All of these are arguments that make PrimFun differentiable
-- class PrimFunDifferentiableWithArgs a b where
--   diffprimfun'' :: (Eq tx, Elt tx, IsFloating tx) => PrimFun (a -> b) -> Exp a -> Exp tx -> Exp b
-- 
-- instance PrimFunDifferentiableWithArgs Float Float where
--   diffprimfun'' _ _ _ = constant 68.0 -- diffprimfun''FF
-- instance PrimFunDifferentiableWithArgs CFloat CFloat where
--   diffprimfun'' = diffprimfunFF
-- instance PrimFunDifferentiableWithArgs Double Double where
--   diffprimfun'' = diffprimfunFF
-- instance PrimFunDifferentiableWithArgs CDouble CDouble where
--   diffprimfun'' = diffprimfunFF
-- 
-- instance PrimFunDifferentiableWithArgs (Float, Float) Float where
--   diffprimfun'' = diffprimfunTFF
-- instance PrimFunDifferentiableWithArgs (CFloat, CFloat) CFloat where
--   diffprimfun'' = diffprimfunTFF
-- instance PrimFunDifferentiableWithArgs (Double, Double) Double where
--   diffprimfun'' = diffprimfunTFF
-- instance PrimFunDifferentiableWithArgs (CDouble, CDouble) CDouble where
--   diffprimfun'' = diffprimfunTFF


-- Get input and output types for all PrimFuns
type family PfTA pf :: *
type family PfTB pf :: *

type instance PfTA (PrimFun (a -> b)) = a
type instance PfTB (PrimFun (a -> b)) = b

class ValidPrimFun pf where {}

instance (Elt (PfTA pf), Elt (PfTB pf), Eq (PfTB pf), IsFloating (PfTB pf)) => ValidPrimFun pf

-- Accept all primfuns
class (ValidPrimFun pf) => DifferentiatePrimFun pf where
  diffprimfun :: (Elt tx, Eq tx, IsFloating tx) => pf -> Exp (PfTA pf) -> Exp tx -> Exp (PfTB pf)
  showdiff :: pf -> String

class (ValidPrimFun pf) => DifferentiatePrimFun' flag pf where
  diffprimfun' :: (Elt tx, Eq tx, IsFloating tx) => flag -> pf -> Exp (PfTA pf) -> Exp tx -> Exp (PfTB pf)
  showdiff' :: flag -> pf -> String

instance ( DifferentiablePred pf flag
         , DifferentiatePrimFun' flag pf) => DifferentiatePrimFun pf where
  diffprimfun = diffprimfun' (undefined::flag)
  showdiff = showdiff' (undefined::flag)


class (ValidPrimFun pf) => DifferentiablePred pf flag | pf -> flag where {}

instance {-# OVERLAPPING #-} (ValidPrimFun pf, TypeCast flag HFalse) => DifferentiablePred pf flag

instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun (Float -> Float)) HTrue
instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun (Double -> Double)) HTrue
instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun (CFloat -> CFloat)) HTrue
instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun (CDouble -> CDouble)) HTrue
-- instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun ((Float, Float) -> Float)) HTrue
-- instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun ((Double, Double) -> Float)) HTrue
-- instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun ((CFloat, CFloat) -> Float)) HTrue
-- instance {-# OVERLAPPING #-} DifferentiablePred (PrimFun ((CDouble, CDouble) -> Float)) HTrue

data HTrue
data HFalse

instance (PrimFunDifferentiableUnary pf) => DifferentiatePrimFun' HTrue pf where
  diffprimfun' _ = diffprimfun'''''
  showdiff' _ _ = "Differentiable Unary Function"

instance (ValidPrimFun pf) => DifferentiatePrimFun' HFalse pf where
  diffprimfun' _ _ _ _ = undefined
  showdiff' _ _ = "Some other function"


class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

-- FFS. Show using exact code from Test



class Print a where
    print :: a -> IO ()

{- the following does not work:
instance Show a => Print a where
    print x = putStrLn (show x)
instance        Print a where
    print x = putStrLn "No show method"

error:
    Duplicate instance declarations:
      instance (Show a) => Print a -- Defined at /tmp/wiki.hs:7:0
      instance Print a -- Defined at /tmp/wiki.hs:9:0
-}

class Print' flag a where
    print' :: flag -> a -> IO ()

instance (ShowPred a flag, Print' flag a) => Print a where
    print = print' (undefined::flag)


-- overlapping instances are used only for ShowPred
class ShowPred a flag | a->flag where {}

                                  -- Used only if the other
                                  -- instances don't apply
instance {-# OVERLAPPING #-} TypeCast flag HFalse => ShowPred a flag

--instance ShowPred Int  HTrue   -- These instances should be
--instance ShowPred Bool HTrue   -- the same as Show's
--instance ShowPred a flag => ShowPred [a] flag

instance Show (PrimFun (Float -> Float)) where
  show fun = "PrimFun (Float -> Float)"
instance Show (PrimFun Float) where
  show fun = "PrimFun (Float)"

instance Show (PrimFun (Int -> String)) where
  show fun = "PrimFun someothertype"

instance {-# OVERLAPPING #-} ShowPred (PrimFun a) HTrue



instance Show a => Print' HTrue a where
   print' _ x = putStrLn (show x)
instance Print' HFalse a where
   print' _ x = putStrLn "No show method"

