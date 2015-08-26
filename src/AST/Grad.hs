{-# LANGUAGE ScopedTypeVariables #-}
module AST.Grad (grad') where

import Prelude hiding ((<*))
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple (Tuple(..), IsTuple, TupleRepr, fromTuple, toTuple, TupleIdx(..))
import Data.Array.Accelerate
import Unsafe.Coerce
import Types
import System.IO.Unsafe
import Differentiate hiding (grad')


-- Differentiate an AST

grad' :: ( Elt tf, IsFloating tf, ToolsT tk
         )
            => tk
            -> PreExp Acc Exp tf
            -> Exp tf
            -> Exp tf

-- grad' tk tag@(Tag f level) _ =

grad' _ (Const _) _ = constant 0

-- grad' tk (Tuple t) _ = constant 11 -- tup d0 d1 where

-- Not sure we can know at compile time that t is compatible with x
-- Assume tup0 is a 2-tuple with identical types
-- TODO

--grad' tk (Prj (i::TupleIdx (TupleRepr tup0) tf) (te::Exp tup0)) x = Exp (Prj i dodgyTupleDiff) where
--  dodgyTupleDiff = unsafeCoerce (gradTT tk teDodgy x) :: Exp tup0 where
--  teDodgy = unsafeCoerce te :: Exp (tf, tf)

-- grad' tk (Foreign a b c) (Exp x) = case x of
--   Foreign xa _ _ -> case matchMarkers a xa of
--     True -> constant 1
--     False -> gradT tk (b c) $ Exp x



-- Failed attempt to unpack tuple.
--  case te of
--    (Exp (Tuple t)) -> case t of
--      NilTup -> constant 0
--      -- s is TupleRepr something, but there's no static guarantee that there is
--      SnocTup (prevt :: Tuple Exp s) v -> case i of
--        ZeroTupIdx -> grad v x
--        -- Ouch...
--        SuccTupIdx (previ::TupleIdx s e) -> grad' (Prj previRT prevtRT) x where
--          previRT = previ
--          prevtRT = Exp $ Tuple prevt

--        (Prj (unsafeCoerce previ::TupleIdx s e) (Exp $ Tuple prevt)) x

-- grad' tk (IndexNil) _ = constant 13

-- grad' tk (IndexCons _ _) _ = constant 14

-- grad' tk (IndexHead _) _ = constant 15

-- grad' tk (IndexTail _) _ = constant 16

-- grad' tk (IndexAny) _ = constant 17

-- grad' tk (ToIndex _ _) _ = constant 18

-- grad' tk (FromIndex _ _) _ = constant 19

-- grad' tk (Cond c l r) dx = Exp $ Cond c dl dr where
--   dl = gradT tk l dx
--   dr = gradT tk r dx

-- grad' tk (While test fun finit) x = Exp $ (Prj ZeroTupIdx while') where
--   while' = Exp $ While test' fun' finit'
--   test' v' = test v where
--     (v, _) = untup2 v'
--   fun' v' = tup2 (fun v, dudx * dfun x) where
--     (v, dudx) = untup2 v'
--   finit' = tup2 (finit, gradT tk finit x)
--   dfun = gradF tk fun

-- grad' _ (PrimConst _) _ = constant 0
-- 
-- grad' tk (PrimApp pf a) dx = gradprimfun tk pf a dx

-- grad' tk (Index _ _) _ = constant 22

-- grad' tk (LinearIndex _ _) _ = constant 23 -- TODO

-- grad' tk (Shape _) _ = constant 24

-- grad' tk (ShapeSize _) _ = constant 25

-- grad' tk (Intersect _ _) _ = constant 26 -- TODO

