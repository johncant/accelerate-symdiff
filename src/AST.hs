{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
module AST where

import Data.Array.Accelerate.AST (PreOpenExp(..), PrimFun(..))
import qualified Data.Array.Accelerate.AST as AST (PrimConst)
import Data.Array.Accelerate.Type (IsFloating, IsNum, numType, NumType(..), FloatingType)
import Data.Array.Accelerate.Array.Sugar (Elt)
import Data.Array.Accelerate.Smart (Acc, Exp(..))
import Data.Array.Accelerate.Array.Sugar (fromElt, toElt, EltRepr)
import Data.Array.Accelerate.Tuple (Tuple(..))
import Foreign.C.Types (CFloat, CDouble)

-- Annoyingly this seems to require an argument so that we can get its type
-- There must be a way of creating Const zero that works with a type constraint
diffconst :: (Elt t, IsFloating t, Num t)
          => PreOpenExp acc env aenv t
          -> PreOpenExp acc env aenv t
diffconst (_:: PreOpenExp acc env aenv a) = Const $ fromElt (0 :: a)


diffpreopenexp :: ( Elt t, IsFloating t, Num t
                  , Elt s, IsFloating s, Num s
                  )
               => PreOpenExp acc env aenv t
               -> PreOpenExp acc env aenv s
               -> PreOpenExp acc env aenv t
-- Not sure of type of dx so...
diffpreopenexp = diffpreopenexp'


diffpreopenexp' :: ( Elt t, IsFloating t, Num t
                   , Elt s, IsFloating s, Num s
                   )
                => PreOpenExp acc env aenv t
                -> PreOpenExp acc env aenv s
                -> PreOpenExp acc env aenv t

diffpreopenexp' v@(Let _ _) _ = diffconst v -- TODO

diffpreopenexp' v@(Var _) _ = diffconst v -- TODO

diffpreopenexp' v@(Foreign _ _ _) _ = diffconst v -- TODO

diffpreopenexp' v@(Const _) _ = diffconst v

diffpreopenexp' v@(Tuple _) _ = diffconst v -- TODO

diffpreopenexp' v@(Prj _ _) _ = diffconst v -- TODO

diffpreopenexp' v@IndexNil _ = diffconst v

diffpreopenexp' v@(IndexCons _ _) _ = diffconst v

diffpreopenexp' v@(IndexHead _) _ = diffconst v

diffpreopenexp' v@(IndexTail _) _ = diffconst v

diffpreopenexp' v@IndexAny _ = diffconst v

diffpreopenexp' v@(IndexSlice _ _ _) _ = diffconst v

diffpreopenexp' v@(IndexFull _ _ _) _ = diffconst v

diffpreopenexp' v@(ToIndex _ _) _ = diffconst v

diffpreopenexp' v@(FromIndex _ _) _ = diffconst v

diffpreopenexp' (Cond c l r) dx = Cond c dl dr where
  dl = diffpreopenexp l dx
  dr = diffpreopenexp r dx

diffpreopenexp' v@(While _ _ _) _ = diffconst v -- TODO

diffpreopenexp' v@(PrimConst _) _ = diffconst v

diffpreopenexp' v@(PrimApp fun a) dx = diffprimfun fun a dx

diffpreopenexp' v@(Index _ _) _ = diffconst v -- TODO

diffpreopenexp' v@(LinearIndex _ _) _ = diffconst v -- TODO

diffpreopenexp' v@(Shape _) _ = diffconst v

diffpreopenexp' v@(ShapeSize _) _ = diffconst v

diffpreopenexp' v@(Intersect _ _) _ = diffconst v -- TODO

chainPrimUnary :: ( IsNum tf, Elt tf, IsFloating tf
                  , IsNum tx, Elt tx, IsFloating tx)
               => PrimFun (tf -> tf)
               -> PreOpenExp acc env aenv tf
               -> PreOpenExp acc env aenv tx
               -> PreOpenExp acc env aenv tf
chainPrimUnary dfun a dx = chainUnary (PrimApp dfun a) a dx

chainUnary :: ( IsNum tf, Elt tf, IsFloating tf
              , IsNum tx, Elt tx, IsFloating tx)
           => PreOpenExp acc env aenv tf
           -> PreOpenExp acc env aenv tf
           -> PreOpenExp acc env aenv tx
           -> PreOpenExp acc env aenv tf
chainUnary dexp a dx = PrimApp (PrimMul ty) multup where
  multup = Tuple (NilTup `SnocTup` dexp `SnocTup` da)
  ty = numType :: (IsNum b) => NumType b
  da = diffpreopenexp a dx

-- Problem: PrimFun (a -> b) appears in the AST and can be any type in theory
-- Although in practice, a and b will only be types that result from the data
-- constructors. We need to handle these differently, but you can't pattern
-- match based on type. If we naively use a class and instances, the compiler
-- will bork, since not all PrimFuns will be in the class, only the constructable
-- ones. We need this:
--
-- https://wiki.haskell.org/GHC/AdvancedOverlap
--


type family ArgT pf
type family ResultT pf
type instance ArgT (PrimFun (a -> b)) = a
type instance ResultT (PrimFun (a -> b)) = b

-- Outer class of all PrimFun s
-- TODO - if this does not work, use type families to specify types
class (Elt tx) => DifferentiatePrimFun pf tx where
  diffprimfun :: ( Elt (ArgT pf)
                 , Elt (ResultT pf)
                 , Elt tx
--                 , IsFloating (ArgT pf)
--                 , IsFloating (ResultT pf)
--                 , IsFloating tx
                 )
              => pf
              -> PreOpenExp acc env aenv (ArgT pf)
              -> PreOpenExp acc env aenv tx
              -> PreOpenExp acc env aenv (ResultT pf)

-- Inner class with flags
class (Elt tx) => DifferentiatePrimFun' flag pf tx where
  diffprimfun' :: ( Elt (ArgT pf)
                  , Elt (ResultT pf)
                  , Elt tx
--                  , IsFloating (ArgT pf)
--                  , IsFloating (ResultT pf)
--                  , IsFloating tx
                  )
               => flag
               -> pf
               -> PreOpenExp acc env aenv (ArgT pf)
               -> PreOpenExp acc env aenv tx
               -> PreOpenExp acc env aenv (ResultT pf)

instance (DifferentiatePrimFun' flag pf tx, TypesMatchPred pf tx flag) => DifferentiatePrimFun pf tx where
  diffprimfun = diffprimfun' (undefined::flag)

-- ...
class TypesMatchPred pf tx flag | pf tx -> flag where {}

-- Used if no matches

instance TypeCast flag PFUseless => TypesMatchPred pf tx flag

-- Useful instances
-- Unfortunately this won't work
--instance (IsNum t, Elt t, IsFloating t) => TypesMatchPred (PrimFun (t -> t)) PFUnaryFloatFloat
-- We have to use this:
instance TypesMatchPred (PrimFun (Float -> Float)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CFloat -> CFloat)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (Double -> Double)) tx PFUnaryFloatFloat
instance TypesMatchPred (PrimFun (CDouble -> CDouble)) tx PFUnaryFloatFloat

-- Type level flags for classifying PrimFun s
data PFUnaryFloatFloat  -- Floating -> Floating
data PFBinaryFloatFloat -- (Floating, Floating ) -> Floating
data PFOtherFloat       -- Other -> Floating
data PFUseless          -- Other -> Other

-- impl
instance (IsFloating a, Elt a, IsFloating tx, Elt tx) => DifferentiatePrimFun' PFUnaryFloatFloat (PrimFun (a -> a)) tx where

-- It might be a bit smarter to compile the derivs rather than manipulating the AST

  diffprimfun' _ (PrimNeg ty) a dx = PrimApp (PrimNeg ty) $ diffpreopenexp a dx

  diffprimfun' _ (PrimAbs ty) a dx = chainPrimUnary (PrimSig ty) a dx

  diffprimfun' _ (PrimSig ty) a _ = diffconst a

  diffprimfun' _ (PrimRecip ty) (a::PreOpenExp acc env aenv ta) dx = chainUnary dexp  a dx where
    dexp = (PrimApp (PrimFPow ty) powtup )
    powtup = Tuple (NilTup `SnocTup` a `SnocTup` (Const $ fromElt (-2 :: ta) ))

  diffprimfun' _ (PrimSin ty) a dx = chainPrimUnary (PrimCos ty) a dx

  diffprimfun' _ (PrimCos ty) a dx = chainUnary minussin a dx where
    minussin = PrimApp (PrimNeg (FloatingNumType ty)) sin
    sin = PrimApp (PrimSin ty) a

  diffprimfun' _ (PrimTan ty) a dx = chainUnary secsquared a dx where
    secsquared = PrimApp (PrimRecip ty) sec
    sec = PrimApp (PrimCos ty) a

  diffprimfun' _ (PrimAsin ty) (a::PreOpenExp acc env aenv ta) dx = chainUnary oneoversqrtoneminusasquared a dx where
    oneoversqrtoneminusasquared = PrimApp (PrimFPow ty) powtup
    powtup = Tuple (NilTup `SnocTup` oneminusasquared `SnocTup` (Const $ fromElt (-0.5 :: ta)))
    oneminusasquared = PrimApp (PrimAdd (FloatingNumType ty)) onemimustup
    onemimustup = Tuple (NilTup `SnocTup` (Const $ fromElt (1::ta)) `SnocTup` asquared)
    asquared = PrimApp (PrimFPow ty) asquaredtup
    asquaredtup = Tuple (NilTup `SnocTup` a `SnocTup` (Const $ fromElt (2 :: ta)))


instance (Elt a) => DifferentiatePrimFun' PFUseless pf a where
  diffprimfun' _ fun a dx = undefined

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

