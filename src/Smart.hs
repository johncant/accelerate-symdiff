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
import Data.Array.Accelerate.Tuple (Tuple(..), IsTuple, TupleRepr)
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import Unsafe.Coerce
import Types
import AST

class (Elt a, Elt dx, IsFloating dx) => Differentiate a dx where
  diff :: Exp a -> Exp dx -> Exp a
  replace :: Exp a -> Exp dx -> Exp dx -> Exp a

instance (Elt a, IsFloating a) => Differentiate a a where
  diff (Exp fpreexp) x = diff' fpreexp x
  replace (Exp f) x x' = replacePreExp f x x'

instance (Elt a, IsFloating a) => Differentiate (a, a) a where
  diff f x = tup2 ((diff' x0 x), (diff' x1 x)) where
    (Exp x0, Exp x1) = untup2 f
  replace a@(Exp (Tuple t)) x x' = tup2 (replace t0 x x', replace t1 x x') where
    (t0, t1) = untup2 a
  replace _ _ _ = tup2 (constant 111, constant 112)


-- Replace the marker for X with something

--class ReplaceTupleRepr tr tx flag where
--  replaceTup :: (Elt tx) => Tuple Exp tr -> Exp tx -> Exp tx -> Tuple Exp tr
--
--instance (Elt tx, IsFloating tx) => ReplaceTupleRepr () tx HTrue where
--  replaceTup _ _ _ = NilTup
--
--instance (IsFloating tx, ReplaceTupleRepr tr tx HTrue) => ReplaceTupleRepr (tr, tx) tx HTrue where
--  replaceTup (SnocTup t v) x x' = (t' `SnocTup` v') where
--    t' = replaceTup t x x'
--    v' = replace v x x'
--
--instance (ReplaceTupleRepr tr tx HTrue, flag ~ HFalse) => ReplaceTupleRepr (tr, a) tx flag where
--  replaceTup (SnocTup t v) x x' = (t' `SnocTup` v') where
--    t' = replaceTup t x x'
--    v' = v


replacePreExp :: (Elt tf, IsFloating tf) => PreExp Acc Exp tf -> Exp tf -> Exp tf -> Exp tf

replacePreExp f@(Tag _) _ _ = Exp f
replacePreExp f@(Const _) _ _ = Exp f
replacePreExp (Tuple t) x x' = undefined -- (IsTuple tf) => not applicable here
-- TODO - unsafecoerce might bork
--replacePreExp f@(Prj i t) (x::tx) x' = Exp $ Prj (unsafeCoerce i) $ replace (unsafeCoerce t) x x' -- TODO i can contain x

replacePreExp (Cond c l r) x x' = Exp $ Cond c' l' r' where
  c' = c --TODO - x can appear here. replace c x x'
  l' = replace l x x'
  r' = replace r x x'

replacePreExp pa@(PrimApp pf a) x x' = Exp $ PrimApp pf (replaceprimfun pf a x x')
replacePreExp f@(Foreign f1 f2 f3) x x'= x' -- replaceForeign'' f f1 f2 f3 x x'
replacePreExp f@(PrimConst _) _ _ = Exp f
--replacePreExp f _ _ = undefined





replaceprimfun :: (Elt a, Elt b, IsFloating b)
            => PrimFun (a -> b) -> Exp a -> Exp b -> Exp b -> Exp a
replaceprimfun pf@(PrimNeg _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAbs _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimSig _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimRecip _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimSin _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimCos _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimTan _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAsin _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAcos _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAtan _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAsinh _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAcosh _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimAtanh _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimExpFloating _) a x x' = replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimSqrt _) a x x'= replaceAllPrimFunsFF pf a x x'
replaceprimfun pf@(PrimLog _) a x x' = replaceAllPrimFunsFF pf a x x'

replaceprimfun pf@(PrimAdd _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimSub _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimMul _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimFDiv _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimFPow _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimLogBase _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimAtan2 _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimMax _) a x x' = replaceAllPrimFunsTFF pf a x x'
replaceprimfun pf@(PrimMin _) a x x' = replaceAllPrimFunsTFF pf a x x'

-- Differentiate an AST


diff' :: ( Elt tf, IsFloating tf
         )
            => PreExp Acc Exp tf
            -> Exp tf
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



diffprimfun :: (Elt a, Elt b, IsFloating b)
            => PrimFun (a -> b) -> Exp a -> Exp b -> Exp b
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


-- branch based on type a above ^

class (IsFloating b, Elt b, Elt a) => DifferentiatePrimFunFF a b valid | a b -> valid where
  diffAllPrimFunsFF :: PrimFun (a -> b) -> Exp a -> Exp b -> Exp b
  replaceAllPrimFunsFF :: PrimFun (a -> b) -> Exp a -> Exp b -> Exp b -> Exp b

instance {-# OVERLAPPING #-} (IsFloating a, Elt a) => DifferentiatePrimFunFF a a HTrue where
  diffAllPrimFunsFF = diffprimfunFF
  replaceAllPrimFunsFF _ = replace

instance {-# OVERLAPPING #-} (Elt a, IsFloating b, Elt b, r ~ HFalse) => DifferentiatePrimFunFF a b r where
  diffAllPrimFunsFF = undefined
  replaceAllPrimFunsFF = undefined



class (IsFloating b, Elt b, Elt atup) => DifferentiatePrimFunTFF atup b valid | atup b -> valid where
  diffAllPrimFunsTFF :: PrimFun (atup -> b) -> Exp atup -> Exp b -> Exp b
  replaceAllPrimFunsTFF :: PrimFun (atup -> b) -> Exp atup -> Exp b -> Exp b -> Exp atup

instance {-# OVERLAPPING #-} (IsFloating a, Elt a) => DifferentiatePrimFunTFF (a, a) a HTrue where
  diffAllPrimFunsTFF = diffprimfunTFF
  replaceAllPrimFunsTFF pf a x x' = replace a x x'
  
-- tup2 (replace a0 x x', replace a1 x x') where
--     (a0, a1) = untup2 a

instance {-# OVERLAPPING #-} (Elt a, IsFloating b, Elt b, r ~ HFalse) => DifferentiatePrimFunTFF a b r where
  diffAllPrimFunsTFF = undefined
  replaceAllPrimFunsTFF = undefined



chainUnary :: (Elt t, IsFloating t)
           => Exp t
           -> Exp t
           -> Exp t
           -> Exp t

chainUnary dexp a dx = dexp * (diff a dx)

chainBinary :: (Elt t, IsFloating t)
            => Exp t -> Exp t
            -> Exp t -> Exp t
            -> Exp t
            -> Exp t
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
diffprimfunFF :: Differentiate a a
              => (PrimFun (a -> a))
              -> Exp a
              -> Exp a
              -> Exp a

-- basic stuff
diffprimfunFF (PrimNeg ty) a dx = mkNeg $ diff a dx

diffprimfunFF (PrimAbs ty) a dx = chainUnary (signum a) a dx

diffprimfunFF (PrimSig ty) a _ = constant 0

diffprimfunFF (PrimRecip ty) a dx = chainUnary (-a**(-2)) a dx

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




-- -- Binary funcs
diffprimfunTFF :: (IsFloating a, Elt a
                  )
               => PrimFun ((a,a) -> a)
               -> Exp (a, a)
               -> Exp a
               -> Exp a

-- DMAS
diffprimfunTFF (PrimAdd ty) ta1a2 dx = chainBinary 1 1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimSub ty) ta1a2 dx = chainBinary 1 (-1) a1 a2 dx where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimMul ty) ta1a2 dx = chainBinary a2 a1 a1 a2 dx where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimFDiv ty) ta1a2 dx = chainBinary (1/a2) (-a1/a2**2) a1 a2 dx where
--   (a1, a2) = untup2 ta1a2
-- 
-- -- O and inverse O
-- diffprimfunTFF (PrimFPow ty) ta1a2 dx = chainBinary (a2*a1**(a2-1)) (log a1 * a1**a2) a1 a2 dx where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimLogBase ty) ta1a2 dx = chainBinary (-(log a2)/(a1*(log a1)**2)) (recip (log a1 * log a2)) a1 a2 dx where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimAtan2 ty) ta1a2 dx = (chainBinary (-a2) (-a1) a1 a2 dx)/(a1**2 + a2**2) where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimMax ty) ta1a2 dx = (a2 >* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
--   (a1, a2) = untup2 ta1a2
-- 
-- diffprimfunTFF (PrimMin ty) ta1a2 dx = (a2 <* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
--   (a1, a2) = untup2 ta1a2
-- 
-- -- All functions that input non-floats and output floats must have a differential of 0 or be undifferentiable

diffprimFunOF :: (IsFloating tb, Elt tb, Eq tb,
                  IsIntegral ta, Elt ta, Eq ta
                  )
              => PrimFun (ta -> tb) -> Exp tb -> Exp tb -> Exp tb
diffprimFunOF _ _ _ = constant 63


