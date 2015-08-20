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
        )
     => (Exp a -> Exp a)
     -> Exp a
     -> Exp a
diff f (x::Exp a) = diffast where
  diffast = SMT.diff (f fakex) fakex
  fakex = foreignExp (WithRespectTo undefined :: WithRespectTo a a) id x :: Exp a

