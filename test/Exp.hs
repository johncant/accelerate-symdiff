{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Exp (tests) where

import Distribution.TestSuite
import qualified Data.Array.Accelerate.Interpreter as Backend
import Data.Array.Accelerate hiding (map, (++))
import qualified Data.Array.Accelerate.Smart as SMT (showPreExpOp, Exp(..), PreExp(..))
import qualified Data.Array.Accelerate.AST as AST (PrimFun(..))
import Data.Typeable
import Symdiff (diff)
import Smart hiding (diff)
import AST

tol :: (Fractional a) => a
tol = 1e-2

dx :: (IsFloating a, Fractional a, Elt a) => Exp a
dx = constant 1e-5

testDifferentiateFun :: (Eq a, Elt a, IsFloating a, Ord a, Fractional a, Num a) => String -> (Exp a -> Exp a) -> [a] -> IO [TestInstance]
--testDifferentiateFun :: String -> (Exp Float -> Exp Float) -> [Float] -> IO [TestInstance]
testDifferentiateFun name fun (xs::[a]) = return $ map testAt xs where

  testAt xcpu = TestInstance
    { run = do
        let
          x = constant xcpu
          symbolic = Backend.run $ unit $ diff fun x :: Scalar a
          numerical = Backend.run $ unit $ (fun (x+dx) - fun x)/dx :: Scalar a
          symbolic' = (head.toList) symbolic :: a
          numerical' = (head.toList) numerical :: a
        case (symbolic' - numerical') < tol of
          False -> do
            return $ Finished $ Fail (name ++ " x at x =  " ++ show xcpu ++
                                                    ", expected " ++ show numerical' ++ ", got " ++ show symbolic'
                                                    )
          True -> return $ Finished $ Pass -- (name ++ " x at x =  " ++ show xcpu ++ ":\nExpected " ++ show numerical' ++ ", got " ++ show symbolic')

    , name = name
    , tags = []
    , options = []
    , setOption = \_ _ -> Right $ testAt xcpu
    }



tests :: IO [Test]
tests = do
  let instances = [ testDifferentiateFun "neg" (\a -> negate a) ([1.0, -1.0] :: [Double])
                  , testDifferentiateFun "abs" (\a -> abs a) ([1.0, -1.0] :: [Double])
                  , testDifferentiateFun "sig" (\a -> signum a) ([1.0, -1.0] :: [Double])
                  , testDifferentiateFun "sin" (\a -> sin a) (map (*pi) [0.0, 0.25, 0.5, 0.75, 1, -0.5] :: [Double])
                  , testDifferentiateFun "cos" (\a -> cos a) (map (*pi) [0.0, 0.25, 0.5, 0.75, 1, -0.5] :: [Double])
                  , testDifferentiateFun "tan" (\a -> tan a) (map (*pi) [0.0, 0.25, 0.45, 0.75, 1, -0.45] :: [Double])
                  , testDifferentiateFun "recip1" (\a -> recip a) ([-2, -1, 1, 2] :: [Double])
                  , testDifferentiateFun "asin" (\a -> asin a) ([-0.95, -0.5, 0, 0.5, 0.95] :: [Double])
                  , testDifferentiateFun "acos" (\a -> acos a) ([-0.95, -0.5, 0, 0.5, 0.95] :: [Double])
                  , testDifferentiateFun "atan" (\a -> atan a) ([-100, -0.5, 0, 0.5, 100] :: [Double])
                  , testDifferentiateFun "asinh" (\a -> asinh a) ([-0.95, -0.5, 0, 0.5, 0.95] :: [Double])
                  , testDifferentiateFun "acosh" (\a -> acosh a) ([1.05, 10, 100] :: [Double])
                  , testDifferentiateFun "atanh" (\a -> atanh a) ([-0.95, -0.5, 0, 0.5, 0.95] :: [Double])
                  , testDifferentiateFun "exp" (\a -> exp a) ([-10, -0.5, 0, 0.5, 10] :: [Double])
                  , testDifferentiateFun "sqrt" (\a -> sqrt a) ([0.05, 0.25, 1, 2, 9] :: [Double])
                  , testDifferentiateFun "log" (\a -> log a) ([0.1, 0.5, 1, 10, 1000000] :: [Double])
                  ] :: [IO [TestInstance]]
  instances' <- sequence instances
  return $ map Test $ concat instances'

