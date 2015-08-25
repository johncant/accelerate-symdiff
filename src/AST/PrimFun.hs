{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module AST.PrimFun (diffprimfun) where

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
import Differentiate (Differentiate)
import System.Random
import System.IO.Unsafe



diffprimfun' :: (Elt a, Elt b, IsFloating b)
            => Tools -> PrimFun (a -> b) -> Exp a -> Exp b -> Exp b
diffprimfun' tk pf@(PrimNeg _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAbs _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimSig _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimRecip _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimSin _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimCos _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimTan _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAsin _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAcos _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAtan _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAsinh _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAcosh _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimAtanh _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimExpFloating _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimSqrt _) a dx = diffAllPrimFunsFF tk pf a dx
diffprimfun' tk pf@(PrimLog _) a dx = diffAllPrimFunsFF tk pf a dx

diffprimfun' tk pf@(PrimAdd _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimSub _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimMul _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimFDiv _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimFPow _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimLogBase _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimAtan2 _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimMax _) a dx = diffAllPrimFunsTFF tk pf a dx
diffprimfun' tk pf@(PrimMin _) a dx = diffAllPrimFunsTFF tk pf a dx

diffprimfun' tk _ _ dx = constant 0


-- branch based on type a above ^



class DifferentiatePrimFunFF a b valid | a b -> valid where
  diffAllPrimFunsFF :: Tools -> PrimFun (a -> b) -> Exp a -> Exp b -> Exp b

instance {-# OVERLAPPING #-} (Differentiate a a) => DifferentiatePrimFunFF a a HTrue where
  diffAllPrimFunsFF = diffprimfunFF

instance {-# OVERLAPPING #-} (r ~ HFalse) => DifferentiatePrimFunFF a b r where
  diffAllPrimFunsFF = undefined



class DifferentiatePrimFunTFF atup b valid | atup b -> valid where
  diffAllPrimFunsTFF :: Tools -> PrimFun (atup -> b) -> Exp atup -> Exp b -> Exp b

instance {-# OVERLAPPING #-} (Differentiate a a) => DifferentiatePrimFunTFF (a, a) a HTrue where
  diffAllPrimFunsTFF = diffprimfunTFF

instance {-# OVERLAPPING #-} (r ~ HFalse) => DifferentiatePrimFunTFF a b r where
  diffAllPrimFunsTFF _ _ _ _ = undefined



chainUnary :: (Differentiate t t)
           => Tools
           -> Exp t
           -> Exp t
           -> Exp t
           -> Exp t

chainUnary tk dexp a dx = dexp * (diffT tk a dx)

chainBinary :: (Differentiate t t)
            => Tools
            -> Exp t -> Exp t
            -> Exp t -> Exp t
            -> Exp t
            -> Exp t
chainBinary tk dwrta1 dwrta2 a1 a2 dx = dwrta1 * (diffT tk a1 dx) + dwrta2 * (diffT tk a2 dx)



-- In order of definition in Accelerate:
-- Some credit goes to Wolfram Alpha
diffprimfunFF :: (Differentiate a a)
              => Tools
              -> (PrimFun (a -> a))
              -> Exp a
              -> Exp a
              -> Exp a

-- basic stuff
diffprimfunFF tk (PrimNeg ty) a dx = mkNeg $ diffT tk a dx

diffprimfunFF tk (PrimAbs ty) a dx = chainUnary tk (signum a) a dx

diffprimfunFF tk (PrimSig ty) a _ = constant 0

diffprimfunFF tk (PrimRecip ty) a dx = chainUnary tk (-a**(-2)) a dx

-- trig
diffprimfunFF tk (PrimSin ty) a dx = chainUnary tk (cos a) a dx

diffprimfunFF tk (PrimCos ty) a dx = chainUnary tk (- sin a) a dx

diffprimfunFF tk (PrimTan ty) a dx = chainUnary tk ((cos a)**(-2)) a dx

-- inverse trig
diffprimfunFF tk (PrimAsin ty) a dx = chainUnary tk (1/(sqrt (1 - a**2))) a dx

diffprimfunFF tk (PrimAcos ty) a dx = chainUnary tk (-1/(sqrt (1 - a**2))) a dx

diffprimfunFF tk (PrimAtan ty) a dx = chainUnary tk (1/(1+a**2)) a dx

-- inverse hyperbolic
diffprimfunFF tk (PrimAsinh ty) a dx = chainUnary tk (1/(sqrt (1 + a**2))) a dx

diffprimfunFF tk (PrimAcosh ty) a dx = chainUnary tk (1/(sqrt (a**2 - 1))) a dx

diffprimfunFF tk (PrimAtanh ty) a dx = chainUnary tk (1/(1 - a**2)) a dx

-- other important funcs
diffprimfunFF tk (PrimExpFloating ty) a dx = chainUnary tk a a dx

diffprimfunFF tk (PrimSqrt ty) a dx = chainUnary tk (0.5 * a** (-0.5)) a dx

diffprimfunFF tk (PrimLog ty) a dx = chainUnary tk (recip a) a dx




-- -- Binary funcs
diffprimfunTFF :: (IsFloating a, Elt a
                  )
               => Tools
               -> PrimFun ((a,a) -> a)
               -> Exp (a, a)
               -> Exp a
               -> Exp a

-- DMAS
diffprimfunTFF tk (PrimAdd ty) ta1a2 dx = chainBinary tk 1 1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimSub ty) ta1a2 dx = chainBinary tk 1 (-1) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimMul ty) ta1a2 dx = chainBinary tk a2 a1 a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimFDiv ty) ta1a2 dx = chainBinary tk (1/a2) (-a1/a2**2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

-- O and inverse O
diffprimfunTFF tk (PrimFPow ty) ta1a2 dx = chainBinary tk (a2*a1**(a2-1)) (log a1 * a1**a2) a1 a2 dx where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimLogBase ty) ta1a2 dx = chainBinary tk
                                             (-(log a2)/(a1*(log a1)**2))
                                             (recip (a2 * log a1))
                                             a1 a2 dx where
                                               (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimAtan2 ty) ta1a2 dx = (chainBinary tk (-a2) (-a1) a1 a2 dx)/(a1**2 + a2**2) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimMax ty) ta1a2 dx = (a2 >* a1) ? (chainUnary tk 1 a2 dx, chainUnary tk 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

diffprimfunTFF tk (PrimMin ty) ta1a2 dx = (a2 <* a1) ? (chainUnary tk 1 a2 dx, chainUnary tk 1 a1 dx) where
  (a1, a2) = untup2 ta1a2

 -- All functions that input non-floats and output floats must have a differential of 0 or be undifferentiable

diffprimFunOF :: (IsFloating tb, Elt tb, Eq tb,
                  IsIntegral ta, Elt ta, Eq ta
                  )
              => Tools -> PrimFun (ta -> tb) -> Exp tb -> Exp tb -> Exp tb
diffprimFunOF _ _ _ _ = constant 63

