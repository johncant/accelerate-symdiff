{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smart where

import Prelude hiding ((>*), (<*))
import Data.Array.Accelerate.Type (IsFloating, NumType(..), numType)
import Data.Array.Accelerate.Array.Sugar (Elt, Foreign)
import Data.Array.Accelerate.Smart
--import Data.Array.Accelerate.Smart (Acc, Exp(..), PreExp(..))
import Data.Array.Accelerate.AST (PrimFun(..))
import Foreign.C.Types (CFloat, CDouble)
import Data.Array.Accelerate
import Unsafe.Coerce
import Types
import AST.Diff
import AST.Grad


-- TODO - prove properly that this is pure, since it uses unsafePerformIO
-- It's used to tag differentiation variables rather than traversing the AST
-- and removing them.




-- jacobian' :: Grad a dx
--       => PreAcc Acc Exp a
--       -> Acc (Vector x)
--       -> Acc (Vector a)
-- jacobian' (Atag _ _ _) _ = grad' a x
-- jacobian' (Pipe _ _ _) _ =
--
-- jacobian' f@(Foreign a b c) (Acc x) = case x of
--   Foreign xa xb xc -> case matchMarkers a xa of
--     True -> constant 1
--     False -> undefined -- diff (b c) $ Exp x
--
-- jacobian' (Acond _ _ _) _ =
-- jacobian' (Awhile _ _ _) _ =
-- jacobian' (Atuple _) _ =
-- jacobian' (Aprj _) _ =
-- jacobian' (Use _) _ =
-- jacobian' (Unit _) _ =
-- jacobian' (Generate _ _) _ =
-- jacobian' (Reshape _ _) _ =
-- jacobian' (Replicate_ _) _ =
-- jacobian' (Slice _ _) _ =
-- jacobian' (Map _ _) _ =
-- jacobian' (ZipWith _ _ _) _ =
-- jacobian' (Fold _ _ _) _ =
-- jacobian' (Fold1 _ _) _ =
-- jacobian' (FoldSeg _ _ _ _) _ =
-- jacobian' (Fold1Seg _ _ _) _ =
-- jacobian' (Scanl _ _ _) _ =
-- jacobian' (Scanl' _ _) _ =
-- jacobian' (Scanl1 _ _) _ =
-- jacobian' (Scanr _ _ _ _) _ =
-- jacobian' (Scanr' _ _ _) _ =
-- jacobian' (Scanr1 _ _) _ =
-- jacobian' (Permute _ _ _ _) _ =
-- jacobian' (Backpermute _ _ _) _ =
-- jacobian' (Stencil _ _ _) _ =
-- jacobian' (Stencil2 _ _ _ _ _) =




