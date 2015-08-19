{-# LANGUAGE GADTs #-}
module Types where

import Data.Typeable
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Array.Sugar (Foreign(..))
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))

data HFalse
data HTrue


data WithRespectTo t1 t2 where
  WithRespectTo :: (Elt t1) => Exp t1 -> WithRespectTo t1 t1 deriving (Typeable)

instance Foreign WithRespectTo where
  strForeign f = "x"
