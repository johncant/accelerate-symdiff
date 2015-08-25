-- {-# LANGUAGE ScopedTypeVariables #-}
module AST.Grad (grad') where

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
import System.Random
import System.IO.Unsafe



grad' :: ( Elt tf, IsFloating tf
         )
      => Tools
      -> PreExp Acc Exp tf
      -> Acc (Vector tf)
      -> Exp tf

grad' tk tag@(Tag level) x = constant 0

-- grad' tk (Const cf) _ = constant 0
-- 
-- -- grad' tk (Tuple t) _ = constant 11 -- tup d0 d1 where
-- 
-- -- Not sure we can know at compile time that t is compatible with x
-- -- Assume tup0 is a 2-tuple with identical types
-- -- TODO
-- grad' tk f@(Prj (i::TupleIdx (TupleRepr tup0) tf) (te::Exp tup0)) x = Exp (Prj i dodgyTuplegrad) where
--   dodgyTuplegrad = unsafeCoerce (gradT tk teDodgy x) :: Exp tup0 where
--   teDodgy = unsafeCoerce te :: Exp (tf, tf)
-- 
-- 
-- 
-- -- Failed attempt to unpack tuple.
-- --  case te of
-- --    (Exp (Tuple t)) -> case t of
-- --      NilTup -> constant 0
-- --      -- s is TupleRepr something, but there's no static guarantee that there is
-- --      SnocTup (prevt :: Tuple Exp s) v -> case i of
-- --        ZeroTupIdx -> gradT tk v x
-- --        -- Ouch...
-- --        SuccTupIdx (previ::TupleIdx s e) -> grad' (Prj previRT prevtRT) x where
-- --          previRT = previ
-- --          prevtRT = Exp $ Tuple prevt
-- 
-- --        (Prj (unsafeCoerce previ::TupleIdx s e) (Exp $ Tuple prevt)) x
-- 
-- -- grad' tk (IndexNil) _ = constant 13
-- 
-- -- grad' tk (IndexCons _ _) _ = constant 14
-- 
-- -- grad' tk (IndexHead _) _ = constant 15
-- 
-- -- grad' tk (IndexTail _) _ = constant 16
-- 
-- -- grad' tk (IndexAny) _ = constant 17
-- 
-- -- grad' tk (ToIndex _ _) _ = constant 18
-- 
-- -- grad' tk (FromIndex _ _) _ = constant 19
-- 
-- grad' tk (Cond c l r) dx = Exp $ Cond c dl dr where
--   dl = gradT tk l dx
--   dr = gradT tk r dx
-- 
-- grad' tk (While test fun init) x = Exp $ (Prj ZeroTupIdx while') where
--   while' = Exp $ While test' fun' init'
--   test' v' = test v where
--     (v, dudx) = untup2 v'
--   fun' v' = tup2 (fun v, dudx * dfun x) where
--     (v, dudx) = untup2 v'
--   init' = tup2 (init, gradT tk init x)
--   dfun = gradF tk fun
-- 
-- grad' tk (PrimConst _) _ = constant 0
-- 
-- grad' tk pa@(PrimApp pf a) dx = gradprimfun tk pf a dx
-- 
-- -- grad' tk (Index _ _) _ = constant 22
-- 
-- -- grad' tk (LinearIndex _ _) _ = constant 23 -- TODO
-- 
-- -- grad' tk (Shape _) _ = constant 24
-- 
-- -- grad' tk (ShapeSize _) _ = constant 25
-- 
-- -- grad' tk (Intersect _ _) _ = constant 26 -- TODO
-- 
-- grad' tk f@(Foreign a b c) (Exp x) = case x of
--   Foreign xa xb xc -> case matchMarkers a xa of
--     True -> constant 1
--     False -> gradT tk (b c) $ Exp x
-- 
