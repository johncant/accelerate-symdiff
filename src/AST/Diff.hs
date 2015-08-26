{-# LANGUAGE ScopedTypeVariables #-}
module AST.Diff (diff') where

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
import Types hiding (diff')
import System.Random
import System.IO.Unsafe
import Differentiate hiding (diff')
import qualified Differentiate (diff')


-- Differentiate an AST

diff' :: ( Elt tf, IsFloating tf, ToolsT tk
         )
            => tk
            -> PreExp Acc Exp tf
            -> Exp tf
            -> Exp tf

-- diff' tk tag@(Tag f level) _ =

diff' tk (Const cf) _ = constant 0

-- diff' tk (Tuple t) _ = constant 11 -- tup d0 d1 where

-- Not sure we can know at compile time that t is compatible with x
-- Assume tup0 is a 2-tuple with identical types
-- TODO
diff' tk f@(Prj (i::TupleIdx (TupleRepr tup0) tf) (te::Exp tup0)) x = Exp (Prj i dodgyTupleDiff) where
  dodgyTupleDiff = unsafeCoerce (diffTT tk teDodgy x) :: Exp tup0 where
  teDodgy = unsafeCoerce te :: Exp (tf, tf)

diff' tk f@(Foreign a b c) (Exp x) = case x of
  Foreign xa xb xc -> case matchMarkers a xa of
    True -> constant 1
    False -> diffT tk (b c) $ Exp x



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

-- diff' tk (IndexNil) _ = constant 13

-- diff' tk (IndexCons _ _) _ = constant 14

-- diff' tk (IndexHead _) _ = constant 15

-- diff' tk (IndexTail _) _ = constant 16

-- diff' tk (IndexAny) _ = constant 17

-- diff' tk (ToIndex _ _) _ = constant 18

-- diff' tk (FromIndex _ _) _ = constant 19

diff' tk (Cond c l r) dx = Exp $ Cond c dl dr where
  dl = diffT tk l dx
  dr = diffT tk r dx

diff' tk (While test fun init) x = Exp $ (Prj ZeroTupIdx while') where
  while' = Exp $ While test' fun' init'
  test' v' = test v where
    (v, dudx) = untup2 v'
  fun' v' = tup2 (fun v, dudx * dfun x) where
    (v, dudx) = untup2 v'
  init' = tup2 (init, diffT tk init x)
  dfun = diffF tk fun

diff' tk (PrimConst _) _ = constant 0

diff' tk pa@(PrimApp pf a) dx = diffprimfun tk pf a dx

-- diff' tk (Index _ _) _ = constant 22

-- diff' tk (LinearIndex _ _) _ = constant 23 -- TODO

-- diff' tk (Shape _) _ = constant 24

-- diff' tk (ShapeSize _) _ = constant 25

-- diff' tk (Intersect _ _) _ = constant 26 -- TODO

