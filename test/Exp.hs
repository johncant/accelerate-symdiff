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
    { run = let
              x = constant xcpu
              (SMT.Exp dslFrag) = fun x
              (SMT.Exp dslDerivFrag) = diff fun x
              result = case (symbolic' - numerical') < tol of
                        _ -> do
                          return $ Finished $ Fail "foo"
                          putStrLn name
                          putStrLn "Differentiated:"
                          putStrLn $ SMT.showPreExpOp dslDerivFrag
                          putStrLn $ show (typeOf dslDerivFrag)
                          () <- case dslDerivFrag of
                            (SMT.PrimApp pf@(AST.PrimMul ty) (SMT.Exp a)) -> do
--                              Smart.print pf
--                              putStrLn $ showdiff pf
--                              putStrLn $ SMT.showPreExpOp $
--                                diffprimfun pf a x
                              putStrLn $ show ty
                              putStrLn $ show (typeOf dslFrag)
--                              putStrLn $ show dslFrag
                              putStrLn $ SMT.showPreExpOp a
                              return ()
                          putStrLn "Result:"
                          putStrLn $ SMT.showPreExpOp dslDerivFrag
                          putStrLn $ show (typeOf dslDerivFrag)
                          return $ Finished $ Fail (name ++ " x at x =  " ++ show xcpu ++
                                                    ", expected " ++ show numerical' ++ ", got " ++ show symbolic'
                                                    )
--                        True -> return $ Finished $ Fail (name ++ " x at x =  " ++ show xcpu ++ ":\nExpected " ++ show numerical' ++ ", got " ++ show symbolic')
              symbolic = Backend.run $ unit $ diff fun x :: Scalar a
              numerical = Backend.run $ unit $ (fun (x+dx) - fun x)/dx :: Scalar a
              symbolic' = (head.toList) symbolic :: a
              numerical' = (head.toList) numerical :: a
              in result
    , name = name
    , tags = []
    , options = []
    , setOption = \_ _ -> Right $ testAt xcpu
    }



tests :: IO [Test]
tests = do
  instances <- testDifferentiateFun "sin" (\a -> sin a) (map (*3.1415926535898) [0.0, 0.25, 0.5, 0.75, 1, 1.1] :: [Double])
  return $ map Test instances

