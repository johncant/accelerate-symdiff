{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symdiff (diff) where

import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign(..))
import Data.Array.Accelerate.Smart (Acc, Exp(..))
import Data.Array.Accelerate (foreignExp, constant)
import qualified Smart as SMT
import Types

-- Differentiating a function is intuitive
-- Differentiating an AST that looks like a result less so.


diff :: SMT.Differentiate b a
     => (Exp a -> Exp b)
     -> Exp a
     -> Exp b
diff = SMT.diffScalar


