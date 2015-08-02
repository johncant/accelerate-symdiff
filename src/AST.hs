{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}

module AST where


import Prelude hiding ((>*), (<*))
import Data.Array.Accelerate.Type (IsFloating, NumType(..), numType)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Data.Array.Accelerate.Tuple (Tuple(..), TupleRepr, TupleIdx)
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Representation (shapeToList)


-- Are two AST fragments of different type equal?
-- IncoherentInstances is fine - we just want the first of these
class DslEquiv2 a b where
  (=====) :: a -> b -> Bool

instance (DslEquiv a) => DslEquiv2 a a where
  a ===== b = a ==== b

instance (DslEquiv a, DslEquiv b) => DslEquiv2 a b where
  a ===== b = False

-- Are two AST fragments of the same type equal?

class DslEquiv a where
  (====) :: a -> a -> Bool

instance (DslEquiv (PreExp Acc Exp t1)) => DslEquiv (Exp t1) where
  (Exp a) ==== (Exp b) = a ==== b

instance (DslEquiv t1, DslEquiv t2) => DslEquiv (t1, t2) where
  (a1, b1) ==== (a2, b2) = (a1 ==== a2) && (b1 ==== b2)

instance DslEquiv (Tuple Exp t) where
  _ ==== _ = False -- TODO

instance DslEquiv (TupleIdx t e) where
  _ ==== _ = False -- TODO

instance (Eq tf) => DslEquiv (PreExp Acc Exp tf) where
  (Tag level1) ==== (Tag level2) = level1 == level2

  (Const cf1) ==== (Const cf2) = cf2 == cf2

--  (Tuple t1) ==== (Tuple t2) = t1 ==== t2
--
--  (Prj i1 tup1) ==== (Prj i2 tup2) = (i1 ==== i2) && (tup1 ==== tup2)

  IndexNil ==== IndexNil = True

--  (IndexCons a1 b1) ==== (IndexCons a2 b2) = (a1 ==== a2) && (b1 ==== b2)

--  (IndexHead a1) ==== (IndexHead a2) = (a1 ==== a2)

--  (IndexTail a1) ==== (IndexTail a2) = (a1 ==== a2)

  IndexAny ==== IndexAny = True

--  (ToIndex a1 b1) ==== (ToIndex a2 b2) = (a1 ==== a2) && (b1 ==== b2)

  (FromIndex a1 b1) ==== (FromIndex a2 b2) = (a1 ==== a2) && (b1 ==== b2)

  (Cond c1 l1 r1) ==== (Cond c2 l2 r2) = (c1 ==== c2) && (l1 ==== l2) && (r1 ==== r2) where

--  (While a1 b1 c1) ==== (While a2 b2 c2) = (a1 ==== a2) && (b1 ==== b2) && (c1 ==== c2)

--  (PrimConst a1) ==== (PrimConst a2) = a1 == a2

--  (PrimApp pf1 a1) ==== (PrimApp pf2 a2) = (pf1 ==== pf2) && (a1 ==== a2)

--  (Index a1 b1) ==== (Index a2 b2) = (a1 ==== a2) && (b1 ==== b2)

--  (LinearIndex a1 b1) ==== (LinearIndex a2 b2) = (a1 ==== a2) && (b1 ==== b2)

--  (Shape a1) ==== (Shape a2) = a1 == a2

--  (ShapeSize a1) ==== (ShapeSize a2) = a1 == a2

  (Intersect a1 b1) ==== (Intersect a2 b2) = (a1 ==== a2) && (b1 ==== b2)

--  (Foreign a1 b1 c1) ==== (Foreign a2 b2 c2) = (a1 ==== a2) && (b1 ==== b2) && (c1 ==== c2)

  _ ==== _ = False
