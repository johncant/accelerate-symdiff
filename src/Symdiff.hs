{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symdiff (diff) where

import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign(..), Vector)
import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp)
import Data.Array.Accelerate.AST (PrimFun)
import Data.Array.Accelerate (foreignExp, constant)
import Types
import qualified AST.Diff
import qualified AST.Grad
import qualified AST.PrimFun
import qualified Differentiate
import Differentiate -- (Differentiate, DifferentiateValue, ToolsT)

import System.Random
import System.IO.Unsafe
-- Differentiating a function is intuitive
-- Differentiating an AST that looks like a result less so.


data Tools = Tools


-- TODO - prove properly that this is pure, since it uses unsafePerformIO
-- It's used to tag differentiation variables rather than traversing the AST
-- and removing them after use

instance ToolsT Tools where
  diffF tk f (x::Exp a) = diffast where
    diffast = diffT tk (f fakex) fakex
    fakex = foreignExp (WithRespectTo marker) id x :: Exp a
    marker = (unsafePerformIO randomIO)
  diffT = diffVT
  diffprimfun = AST.PrimFun.diffprimfun'
  diff' = AST.Diff.diff'




-- gradF :: (Elt e, IsFloating e)
--      => Tools
--      -> (Acc (Vector e) -> Exp e)
--      -> Acc (Vector e)
--      -> Exp e
-- gradF tk f (vx :: Acc (Vector a)) = diffast where
--   diffast = gradT tk (f fakevx)
--   fakevx = foreignAcc (WithRespectToA marker) id vx
--   marker = (unsafePerformIO randomIO)

--grad = gradF tools

diff :: (DifferentiateValue a dx) => (Exp dx -> Exp a) -> Exp dx -> Exp a
diff = diffF Tools

