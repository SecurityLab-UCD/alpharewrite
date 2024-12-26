module TestFuncRewrite (testAll) where

import Test.HUnit
import FunctionRewrite (extractNamesFromTypeSig)
import Task (Task (..))

testExtract :: Test
testExtract = TestCase $ do
  let adhoc_input = "significand :: RealFloat a => a -> a"
  let adhoc_expected = Right ["significand"]
  assertEqual "testExtract_Adhoc" adhoc_expected (extractNamesFromTypeSig adhoc_input)

  let mono_input = "words :: String -> [String]"
  let mono_expected = Right ["words"]
  assertEqual "testExtract_Mono" mono_expected (extractNamesFromTypeSig mono_input)

  let poly_input = "map :: (a -> b) -> [a] -> [b]"
  let poly_expected = Right ["map"]
  assertEqual "testExtract_Poly" poly_expected (extractNamesFromTypeSig poly_input)

  let non_input = "data Bool = False | True"
  let non_expected = Right []
  assertEqual "testExtract_Non" non_expected (extractNamesFromTypeSig non_input)


testAll :: IO ()
testAll = do
  putStrLn "Running tests for FunctionRewrite"
  runTestTT $ TestList [testExtract]
  return ()