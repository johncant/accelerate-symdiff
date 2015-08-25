{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symdiff (diff) where

import Data.Array.Accelerate.Type (IsFloating)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign(..), Vector)
import Data.Array.Accelerate.Smart (Acc, Exp(..))
import Data.Array.Accelerate (foreignExp, constant)
import qualified Smart as SMT
import Types
import AST.Diff
import AST.Grad
import AST.PrimFun
import qualified Differentiate
import Differentiate(Differentiate)

import System.Random
import System.IO.Unsafe
-- Differentiating a function is intuitive
-- Differentiating an AST that looks like a result less so.


diffF :: Differentiate b a
     => Tools
     -> (Exp a -> Exp b)
     -> Exp a
     -> Exp b
diffF tk f (x::Exp a) = diffast where
  diffast = diffT tk (f fakex) fakex
  fakex = foreignExp (WithRespectTo marker) id x :: Exp a
  marker = (unsafePerformIO randomIO)


-- gradF :: (Elt e, IsFloating e)
--      => Tools
--      -> (Acc (Vector e) -> Exp e)
--      -> Acc (Vector e)
--      -> Exp e
-- gradF tk f (vx :: Acc (Vector a)) = diffast where
--   diffast = gradT tk (f fakevx)
--   fakevx = foreignAcc (WithRespectToA marker) id vx
--   marker = (unsafePerformIO randomIO)

diff = Symdiff.diffF tools
--grad = gradF tools

tools :: Tools
tools = Tools
  (Symdiff.diffF tools)
  (Differentiate.diffT tools)
  (diffprimfun tools)
  (AST.Diff.diff' tools)
