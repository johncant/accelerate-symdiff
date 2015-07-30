{-# LANGUAGE GADTs #-}
module Smart (diffpreexp) where

import qualified Data.Array.Accelerate.AST as AST (PreOpenExp(..))
import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import AST

diffpreexp :: (Elt t, IsFloating t) => PreExp Acc Exp t -> PreExp Acc Exp t -> PreExp Acc Exp t
diffpreexp f x = case f of
                  x -> Const 1
                  _ -> diffpreexp' f x

-- Now we are free to ignore f == x .

diffpreexp' :: (Elt t, IsFloating t, Num t)
            => PreExp Acc Exp t
            -> PreExp Acc Exp t
            -> PreExp Acc Exp t

diffpreexp' tag@(Tag level) _ = tag

diffpreexp' (Const cf) _ = Const 0

diffpreexp' (Tuple t) _ = Const 0 -- TODO

diffpreexp' (Prj _ _) _ = Const 0 -- TODO

diffpreexp' (IndexNil) _ = Const 0

diffpreexp' (IndexCons _ _) _ = Const 0

diffpreexp' (IndexHead _) _ = Const 0

diffpreexp' (IndexTail _) _ = Const 0

diffpreexp' (IndexAny) _ = Const 0

diffpreexp' (ToIndex _ _) _ = Const 0

diffpreexp' (FromIndex _ _) _ = Const 0

diffpreexp' (Cond c (Exp l) (Exp r)) dx = Cond c dl dr where
  dl = Exp $ diffpreexp l dx
  dr = Exp $ diffpreexp r dx

diffpreexp' (While _ _ _) _ = Const 0 -- TODO

diffpreexp' (PrimConst _) _ = Const 0

diffpreexp' (PrimApp pf x) _ = Const 0 -- TODO - AST

diffpreexp' (Index _ _) _ = Const 0

diffpreexp' (LinearIndex _ _) _ = Const 0 -- TODO

diffpreexp' (Shape _) _ = Const 0

diffpreexp' (ShapeSize _) _ = Const 0

diffpreexp' (Intersect _ _) _ = Const 0 -- TODO

diffpreexp' (Foreign _ _ _) _ = Const 0 -- TODO

