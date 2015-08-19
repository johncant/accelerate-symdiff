{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

module Smart where

import Prelude hiding ((>*), (<*))
import Data.Array.Accelerate.Type (IsFloating, NumType(..), numType)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Data.Array.Accelerate.Tuple (Tuple(..))
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import Types
import AST

-- class Diff t tx eq | t tx -> eq where
--   diff :: Exp t -> Exp tx -> Exp t
-- 
-- instance (Eq t, Elt t, IsFloating t) => Diff t t HTrue where
--   diff expf@(Exp f) expx@(Exp x) = case f ==== x of
--                                     True -> (constant 1.0)
--                                     False -> diff f expx
-- 
-- instance (Eq t, Elt t, IsFloating t, Eq tx, Elt tx, IsFloating tx, eq ~ HFalse) => Diff t tx eq where
--   diff expf@(Exp f) expx@(Exp x) = case f ===== x of
--                                     True -> (constant 1.0)
--                                     False -> diff f expx


-- Now we are free to ignore f == x .

-- Replace the marker for X with something
replace :: (Elt tf, Elt tx) => Exp tf -> Exp tx -> Exp tx -> Exp tf
replace (Exp f) x x' = replacePreExp f x x'

replaceTup :: (Elt tx) => Tuple Exp t -> Exp tx -> Exp tx -> Tuple Exp t
replaceTup NilTup x x' = NilTup
replaceTup (SnocTup t v) x x' = (t' `SnocTup` v') where
  t' = replaceTup t x x'
  v' = replace v x x'



replacePreExp :: (Elt tf, Elt tx) => PreExp Acc Exp tf -> Exp tx -> Exp tx -> Exp tf

replacePreExp (Tuple t) x x' = Exp $ Tuple $ replaceTup t x x'

replacePreExp (Cond c l r) x x' = Exp $ Cond c' l' r' where
  c' = replace c x x'
  l' = replace l x x'
  r' = replace r x x'

replacePreExp pa@(PrimApp pf a) x x' = Exp $ PrimApp pf (replace a x x') -- TODO TODO TODO
replacePreExp f@(Foreign f1 f2 f3) x x'= replaceForeign'' f f1 f2 f3 x x'
replacePreExp f _ _ = Exp f

--replaceForeign' :: (Elt a, Elt b, Foreign f) => f a b -> (Exp a -> Exp b) -> Exp a -> Exp tx -> Exp tx -> Exp b
--replaceForeign' WithRespectTo f2 f3 x x' = replaceForeign f3 x x'
--replaceForeign

class (Foreign f) => ReplaceForeignSelectForeignInstance (f :: * -> * -> *) match where
  replaceForeign'' :: (Elt a, Elt b, Elt c) => PreExp Acc Exp a -> f c a -> (Exp c -> Exp a) -> Exp c -> Exp b -> Exp b -> Exp a

instance {-# OVERLAPS #-} ReplaceForeignSelectForeignInstance WithRespectTo HTrue where
  replaceForeign'' f f1@(WithRespectTo _) f2 f3 x x' = replaceForeign f f1 f2 f3 x x'

instance {-# OVERLAPPABLE #-} (Foreign f, match ~ HFalse) => ReplaceForeignSelectForeignInstance f match where
  replaceForeign'' f f1 f2 f3 x x' = undefined -- Can't differentiate stuff outside of AST



class (Elt a, Elt b) => ReplaceForeign a b feq | a b -> feq where
  replaceForeign :: (Elt c) => PreExp Acc Exp a -> WithRespectTo c a -> (Exp c -> Exp a) -> Exp c -> Exp b -> Exp b -> Exp a

instance {-# OVERLAPPABLE #-} (Elt a) => ReplaceForeign a a HTrue where
  replaceForeign f f1@(WithRespectTo x'') f2 f3 x x' = x'

instance {-# OVERLAPS #-} (Elt a, Elt b, feq ~ HFalse) => ReplaceForeign a b feq where
  replaceForeign f f1@(WithRespectTo x'') f2 f3 x x' = undefined -- Impossible (haha...)

-- Differentiate an AST
diff :: ( Elt tf, IsFloating tf, Eq tf
        , Elt tx, IsFloating tx, Eq tx)
     => Exp tf
     -> Exp tx
     -> Exp tf
diff (Exp fpreexp) x = diff' fpreexp x


diff' :: ( Elt tf, IsFloating tf, Eq tf
         , Elt tx, IsFloating tx, Eq tx
         )
            => PreExp Acc Exp tf
            -> Exp tx
            -> Exp tf

diff' tag@(Tag level) _ = constant 1.0 -- Exp $ tag -- TODO we put x as this...  use foreign instead

diff' (Const cf) _ = constant 0

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

diff' pa@(PrimApp pf a) dx = diffprimfun pf a dx -- TODO TODO TODO

diff' (Index _ _) _ = constant 22

diff' (LinearIndex _ _) _ = constant 23 -- TODO

diff' (Shape _) _ = constant 24

diff' (ShapeSize _) _ = constant 25

diff' (Intersect _ _) _ = constant 26 -- TODO

diff' f@(Foreign a b c) _ = constant 1 -- TODO - make this more specific.



diffprimfun :: (Elt a, Elt b, Eq b, IsFloating b, Eq tx, Elt tx, IsFloating tx)
            => PrimFun (a -> b) -> Exp a -> Exp tx -> Exp b
diffprimfun pf@(PrimNeg _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAbs _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimSig _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimRecip _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimSin _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimCos _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimTan _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAsin _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAcos _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAtan _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAsinh _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAcosh _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimAtanh _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimExpFloating _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimSqrt _) a dx = diffAllPrimFunsFF pf a dx
diffprimfun pf@(PrimLog _) a dx = diffAllPrimFunsFF pf a dx



class (Eq b, IsFloating b, Elt b) => DifferentiatePrimFunFF a b valid | a b -> valid where
  diffAllPrimFunsFF :: (Elt tx, Eq tx, IsFloating tx) => PrimFun (a -> b) -> Exp a -> Exp tx -> Exp b

instance {-# OVERLAPPING #-} (Eq a, IsFloating a, Elt a) => DifferentiatePrimFunFF a a HTrue where
  diffAllPrimFunsFF = diffprimfunFF

instance {-# OVERLAPPING #-} (Eq a, Eq b, IsFloating b, Elt b, r ~ HFalse) => DifferentiatePrimFunFF a b r where
  diffAllPrimFunsFF = undefined




chainUnary :: ( Elt tf, IsFloating tf, Eq tf
              , Elt tx, IsFloating tx, Eq tx
              )
           => Exp tf
           -> Exp tf
           -> Exp tx
           -> Exp tf

chainUnary dexp a dx = dexp * (diff a dx)

chainBinary :: (Elt tf, IsFloating tf, Eq tf
               ,Elt tx, IsFloating tx, Eq tx
               )
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
diffprimfunFF :: (IsFloating a, Elt a, Eq a, IsFloating tx, Elt tx, Eq tx
                 )
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
diffprimfunTFF :: (IsFloating a, Elt a, Eq a, IsFloating tx, Elt tx, Eq tx
                  )
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


