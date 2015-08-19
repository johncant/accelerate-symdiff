import qualified Exp
import Distribution.TestSuite
import System.Exit (exitFailure)

runTest :: Test -> IO ()
runTest (Test inst) = runTestInst inst

runTestInst :: TestInstance -> IO ()
runTestInst test = do
  (Finished r) <- run test

  case r of
    (Fail msg) -> do
      putStrLn msg
      exitFailure
    Pass -> putStrLn "Success! Yay!"


main :: IO ()
main = do
  tests <- Exp.tests

  mapM_ runTest tests
