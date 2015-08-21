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
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Data.Array.Accelerate.Tuple (Tuple(..), IsTuple, TupleRepr, fromTuple, toTuple, TupleIdx(..))
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import Unsafe.Coerce
import Types
import AST
import System.Random
import System.IO.Unsafe


-- TODO - prove properly that this is pure, since it uses unsafePerformIO
-- It's used to tag differentiation variables rather than traversing the AST
-- and removing them.

diffScalar :: Differentiate b a
           => (Exp a -> Exp b)
           -> Exp a
           -> Exp b
diffScalar f (x::Exp a) = diffast where
  diffast = diff (f fakex) fakex
  fakex = foreignExp (WithRespectTo marker) id x :: Exp a
  marker = (unsafePerformIO randomIO)


class (Elt a, Elt dx, IsFloating dx) => DifferentiateTupleRepr a dx where
  difftr :: Tuple Exp a -> Exp dx -> Tuple Exp a

instance (Elt dx, IsFloating dx) => DifferentiateTupleRepr () dx where
  difftr NilTup x = NilTup

instance (Differentiate a dx, DifferentiateTupleRepr t dx) => DifferentiateTupleRepr (t, a) dx where
  difftr (SnocTup t v) x = (difftr t x) `SnocTup` (diff v x)



class (Elt a, Elt dx, IsFloating dx) => Differentiate a dx where
  diff :: Exp a -> Exp dx -> Exp a

instance {-# OVERLAPPING #-} (Elt a, IsFloating a) => Differentiate a a where
  diff (Exp fpreexp) x = diff' fpreexp x

instance {-# OVERLAPPING #-} ( IsTuple a
         , Elt a
         , DifferentiateTupleRepr (TupleRepr a) dx)
         => Differentiate a dx where
  diff (Exp preF) x = case preF of
                        Tuple t ->
                          Exp $ Tuple $ difftr t x




-- Differentiate an AST


diff' :: ( Elt tf, IsFloating tf
         )
            => PreExp Acc Exp tf
            -> Exp tf
            -> Exp tf

diff' tag@(Tag level) _ = constant 1.0 -- Exp $ tag -- TODO we put x as this...  use foreign instead

diff' (Const cf) _ = constant 0

-- diff' (Tuple t) _ = constant 11 -- tup d0 d1 where

-- Not sure we can know at compile time that t is compatible with x
-- Assume tup0 is a 2-tuple with identical types
-- TODO
diff' f@(Prj (i::TupleIdx (TupleRepr tup0) tf) (te::Exp tup0)) x = Exp (Prj i dodgyTupleDiff) where
  dodgyTupleDiff = unsafeCoerce (diff teDodgy x) :: Exp tup0 where
  teDodgy = unsafeCoerce te :: Exp (tf, tf)



-- Failed attempt to unpack tuple.
--  case te of
--    (Exp (Tuple t)) -> case t of
--      NilTup -> constant 0
--      -- s is TupleRepr something, but there's no static guarantee that there is
--      SnocTup (prevt :: Tuple Exp s) v -> case i of
--        ZeroTupIdx -> diff v x
--        -- Ouch...
--        SuccTupIdx (previ::TupleIdx s e) -> diff' (Prj previRT prevtRT) x where
--          previRT = previ
--          prevtRT = Exp $ Tuple prevt

--        (Prj (unsafeCoerce previ::TupleIdx s e) (Exp $ Tuple prevt)) x

-- diff' (IndexNil) _ = constant 13

-- diff' (IndexCons _ _) _ = constant 14

-- diff' (IndexHead _) _ = constant 15

-- diff' (IndexTail _) _ = constant 16

-- diff' (IndexAny) _ = constant 17

-- diff' (ToIndex _ _) _ = constant 18

-- diff' (FromIndex _ _) _ = constant 19

diff' (Cond c l r) dx = Exp $ Cond c dl dr where
  dl = diff l dx
  dr = diff r dx

diff' (While test fun init) x = Exp $ (Prj ZeroTupIdx while') where
  while' = Exp $ While test' fun' init'
  test' v' = test v where
    (v, dudx) = untup2 v'
  fun' v' = tup2 (fun v, dudx * dfun x) where
    (v, dudx) = untup2 v'
  init' = tup2 (init, diff init x)
  dfun = diffScalar fun

diff' (PrimConst _) _ = constant 0

diff' pa@(PrimApp pf a) dx = diffprimfun pf a dx

-- diff' (Index _ _) _ = constant 22

-- diff' (LinearIndex _ _) _ = constant 23 -- TODO

-- diff' (Shape _) _ = constant 24

-- diff' (ShapeSize _) _ = constant 25

-- diff' (Intersect _ _) _ = constant 26 -- TODO

diff' f@(Foreign a b c) (Exp x) = case x of
  Foreign xa xb xc -> case matchMarkers a xa of
    True -> constant 1
    False -> diff (b c) $ Exp x



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

diffprimfun pf@(PrimAdd _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimSub _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimMul _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimFDiv _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimFPow _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimLogBase _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimAtan2 _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimMax _) a dx = diffAllPrimFunsTFF pf a dx
diffprimfun pf@(PrimMin _) a dx = diffAllPrimFunsTFF pf a dx

diffprimfun _ _ dx = constant 0


-- branch based on type a above ^



class (IsFloating b, Elt b, Elt a) => DifferentiatePrimFunFF a b valid | a b -> valid where
  diffAllPrimFunsFF :: PrimFun (a -> b) -> Exp a -> Exp b -> Exp b

instance {-# OVERLAPPING #-} (IsFloating a, Elt a) => DifferentiatePrimFunFF a a HTrue where
  diffAllPrimFunsFF = diffprimfunFF

instance {-# OVERLAPPING #-} (Elt a, IsFloating b, Elt b, r ~ HFalse) => DifferentiatePrimFunFF a b r where
  diffAllPrimFunsFF = undefined



class (IsFloating b, Elt b, Elt atup) => DifferentiatePrimFunTFF atup b valid | atup b -> valid where
  diffAllPrimFunsTFF :: PrimFun (atup -> b) -> Exp atup -> Exp b -> Exp b

instance {-# OVERLAPPING #-} (IsFloating a, Elt a) => DifferentiatePrimFunTFF (a, a) a HTrue where
  diffAllPrimFunsTFF = diffprimfunTFF

instance {-# OVERLAPPING #-} (Elt a, IsFloating b, Elt b, r ~ HFalse) => DifferentiatePrimFunTFF a b r where
  diffAllPrimFunsTFF = undefined



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

diffprimfunTFF (PrimSub ty) ta1a2 dx = chainBinary 1 (-1) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMul ty) ta1a2 dx = chainBinary a2 a1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimFDiv ty) ta1a2 dx = chainBinary (1/a2) (-a1/a2**2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

-- O and inverse O
diffprimfunTFF (PrimFPow ty) ta1a2 dx = chainBinary (a2*a1**(a2-1)) (log a1 * a1**a2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimLogBase ty) ta1a2 dx = chainBinary
                                             (-(log a2)/(a1*(log a1)**2))
                                             (recip (a2 * log a1))
                                             a1 a2 dx where
                                               (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimAtan2 ty) ta1a2 dx = (chainBinary (-a2) (-a1) a1 a2 dx)/(a1**2 + a2**2) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMax ty) ta1a2 dx = (a2 >* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF (PrimMin ty) ta1a2 dx = (a2 <* a1) ? (chainUnary 1 a2 dx, chainUnary 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

 -- All functions that input non-floats and output floats must have a differential of 0 or be undifferentiable

diffprimFunOF :: (IsFloating tb, Elt tb, Eq tb,
                  IsIntegral ta, Elt ta, Eq ta
                  )
              => PrimFun (ta -> tb) -> Exp tb -> Exp tb -> Exp tb
diffprimFunOF _ _ _ = constant 63


