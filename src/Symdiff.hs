{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symdiff where

import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign(..))
import Data.Array.Accelerate.Smart (Acc, Exp(..))
import Data.Array.Accelerate (foreignExp, constant)
import qualified Smart as SMT
import Types

-- Differentiating a function is intuitive
-- Differentiating an AST that looks like a result less so.



diff :: ( Elt a, IsFloating a, Eq a
        , Elt b, IsFloating b, Eq b
        )
     => (Exp a -> Exp b)
     -> Exp a
     -> Exp b
-- diff f (x::Exp a) = diffast where -- SMT.replace diffast fakex x where
diff f (x::Exp a) = SMT.replace diffast fakex x where
  diffast = SMT.diff (f fakex) fakex
  fakex = foreignExp (WithRespectTo :: WithRespectTo a a) id (68::Exp a) :: Exp a

