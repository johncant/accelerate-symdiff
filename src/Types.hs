{-# LANGUAGE GADTs #-}
module Types where

import Data.Typeable
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Array.Sugar (Foreign(..))
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))

data HFalse
data HTrue


data WithRespectTo t1 t2 where
-- Variable id
  WithRespectTo :: (Elt t1) => Int -> WithRespectTo t1 t1 deriving (Typeable)


instance Foreign WithRespectTo where
  strForeign (WithRespectTo n) = "WithRespectTo " ++ show n

matchMarkers :: (Foreign f, Foreign g) => f a b -> g c d -> Bool
matchMarkers f1 f2 = wrt && eq where
  wrt = take 14 (strForeign f1) == "WithRespectTo "
  eq = strForeign f2 == strForeign f1
