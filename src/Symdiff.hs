{-# LANGUAGE GADTs #-}
module Symdiff where

import qualified Data.Array.Accelerate.AST as AST (PreOpenExp(..))
import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Smart

-- In Smart
-- new Exp t = Data.Array.Accelerate.Smart.Exp Data.Array.Accelerate.Smart.PreExp Acc Exp t
--
-- Contains AST stuff




diff :: (Elt a, IsFloating a) => (Exp a -> Exp a) -> Exp a -> Exp a
diff f x = Exp $ diffpreexp fxexp xexp where
  fxexp = let (Exp fx) = f x in fx
  xexp = let (Exp xx) = x in xx

