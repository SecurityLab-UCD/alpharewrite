{-# LANGUAGE OverloadedStrings #-}
module TestAtomicTypeRewrite (testAll) where

import AtomicTypeRewrite (rewriteAtomicTypes)
import Task (Task(..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.HUnit

defineTask :: Text -> [Text] -> Text -> Task
defineTask sig deps code = Task
  { task_id = "test-id"
  , poly_type = "Monomorphic"
  , signature = sig
  , dependencies = deps
  , code = code
  }

-- Test 1: Simple atomic type replacement
testSimpleRewrite :: Test
testSimpleRewrite = TestCase $ do
  let task = defineTask
              "putChar :: Char -> IO ()"
              ["stdout :: Handle"]
              "putChar c = hPutChar stdout c"
      expected = Task
        { task_id = "test-id"
        , poly_type = "Monomorphic"
        , signature = "putChar :: T1 -> T2 ()"
        , dependencies = ["stdout :: Handle"]
        , code = "putChar c = hPutChar stdout c"
        }
  case rewriteAtomicTypes task of
    Right result -> assertEqual "Simple rewrite failed" expected result
    Left err     -> assertFailure err

-- Test 2: Algebraic data type rewrite
testAlgebraicRewrite :: Test
testAlgebraicRewrite = TestCase $ do
  let task = defineTask
              "pred :: Bool -> Bool"
              ["data Bool = False\n        | True"]
              "pred True = False\npred False = error \"bad argument\""
      expected = Task
        { task_id = "test-id"
        , poly_type = "Monomorphic"
        , signature = "pred :: T1 -> T1"
        , dependencies = ["data T1 = False\n        | True"]
        , code = "pred True = False\npred False = error \"bad argument\""
        }
  case rewriteAtomicTypes task of
    Right result -> assertEqual "Algebraic rewrite failed" expected result
    Left err     -> assertFailure err

-- Main function to run tests
testAll :: IO ()
testAll = do
  let tests = TestList [
                TestLabel "Simple Rewrite" testSimpleRewrite
              , TestLabel "Algebraic Rewrite" testAlgebraicRewrite
              ]
  runTestTT tests >>= print
