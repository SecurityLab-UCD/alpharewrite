{-# LANGUAGE OverloadedStrings #-}

module TestTypeVarRewrite (testAll) where

import Test.Hspec
import qualified Data.Text as T

import TypeVarRewrite
  ( rewriteOneTypeSignature
  , rewriteDependencies
  , rewriteTypeVars
  )

import Task (Task (..))

--------------------------------------------------------------------------------
-- Helper Expectation: for Either
--------------------------------------------------------------------------------
expectRight :: (HasCallStack, Show e) => Either e a -> (a -> Expectation) -> Expectation
expectRight (Left err)  _ = expectationFailure ("Expected Right but got Left: " ++ show err)
expectRight (Right val) f = f val

--------------------------------------------------------------------------------
-- 1) Tests for rewriteOneTypeSignature
--------------------------------------------------------------------------------

testRewriteOneTypeSignature :: Spec
testRewriteOneTypeSignature = describe "rewriteOneTypeSignature" $ do

  it "renames a single type variable, e.g. 'f :: a -> a' => 'f :: f1 -> f1'" $ do
    let input  = "f :: a -> a"
    let output = rewriteOneTypeSignature input
    expectRight output $ \rewritten ->
      rewritten `shouldBe` "f :: f1 -> f1"

  it "renames multiple type variables, ignoring the list constructor" $ do
    let input  = "g :: a -> [b] -> b"
    let output = rewriteOneTypeSignature input
    expectRight output $ \rewritten ->
      rewritten `shouldBe` "g :: f1 -> [f2] -> f2"

  it "leaves monomorphic signature unchanged, e.g. 'h :: Int -> Bool'" $ do
    let input  = "h :: Int -> Bool"
    let output = rewriteOneTypeSignature input
    expectRight output $ \rewritten ->
      rewritten `shouldBe` input

  it "returns the original string if not a type signature (e.g. function body)" $ do
    let input  = "map f [] = []"
    let output = rewriteOneTypeSignature input
    expectRight output $ \rewritten ->
      rewritten `shouldBe` input

--------------------------------------------------------------------------------
-- 2) Tests for rewriteDependencies
--------------------------------------------------------------------------------

testRewriteDependencies :: Spec
testRewriteDependencies = describe "rewriteDependencies" $ do
  it "rewrites multiple lines independently" $ do
    let deps =
          [ "foldr :: (a -> b -> b) -> b -> [a] -> b"
          , "(++)  :: [x] -> [x] -> [x]"
          , "data X = Y Int"
          ]
    let expected =
          [ "foldr :: (f1 -> f2 -> f2) -> f2 -> [f1] -> f2"
          , "(++) :: [f1] -> [f1] -> [f1]"
          , "data X = Y Int"  -- data decl is unchanged
          ]

    let resultE = rewriteDependencies (map T.pack deps)
    expectRight resultE $ \actual ->
      map T.unpack actual `shouldBe` expected

--------------------------------------------------------------------------------
-- 3) Integration tests for rewriteTypeVars
--------------------------------------------------------------------------------

testRewriteTypeVarsInTask :: Spec
testRewriteTypeVarsInTask = describe "rewriteTypeVars" $ do

  it "rewrites signature and dependencies but not code" $ do
    let originalTask =
          Task
            { task_id      = "myTask"
            , poly_type    = "Parametric"
            , signature    = "concatMap :: (a -> [b]) -> [a] -> [b]"
            , code         = "concatMap f = foldr ((++) . f) []"
            , dependencies =
                [ "foldr :: (a -> b -> b) -> b -> [a] -> b"
                , "(++) :: [x] -> [x] -> [x]"
                , "(.)  :: (r -> s) -> (t -> r) -> t -> s"
                ]
            }

    let expectedTask =
          Task
            { task_id      = "myTask"
            , poly_type    = "Parametric"
            , signature    = "concatMap :: (f1 -> [f2]) -> [f1] -> [f2]"
            , code         = "concatMap f = foldr ((++) . f) []"
            , dependencies =
                [ "foldr :: (f1 -> f2 -> f2) -> f2 -> [f1] -> f2"
                , "(++) :: [f1] -> [f1] -> [f1]"
                , "(.) :: (f1 -> f2) -> (f3 -> f1) -> f3 -> f2"
                ]
            }

    let resultE = rewriteTypeVars originalTask
    expectRight resultE $ \newTask -> do
      newTask `shouldBe` expectedTask

  it "leaves monomorphic tasks alone" $ do
    let originalTask =
          Task
            { task_id      = "myMonoTask"
            , poly_type    = "Monomorphic"
            , signature    = "fromInteger :: Integer -> Int"
            , code         = "fromInteger i = I_ (integerToInt_ i)"
            , dependencies =
                [ "integerToInt_ :: Integer -> Int_"
                , "data Int = I_ Int_"
                ]
            }

    -- We expect the same exact Task (no changes),
    -- because there are no type variables to rename.
    let resultE = rewriteTypeVars originalTask
    expectRight resultE $ \newTask -> do
      newTask `shouldBe` originalTask

--------------------------------------------------------------------------------
-- Combine all specs
--------------------------------------------------------------------------------

testAll :: IO ()
testAll = hspec $ do
  testRewriteOneTypeSignature
  testRewriteDependencies
  testRewriteTypeVarsInTask

