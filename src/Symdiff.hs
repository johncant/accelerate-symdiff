{-# LANGUAGE GADTs #-}
module Symdiff where

import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart (Acc, Exp(..))
import qualified Smart as SMT

-- Differentiating a function is intuitive
-- Differentiating an AST that looks like a result less so.

diff :: (Elt a, IsFloating a) => (Exp a -> Exp a) -> Exp a -> Exp a
diff f x = SMT.diff (f x) x

