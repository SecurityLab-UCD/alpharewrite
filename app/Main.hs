{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import FunctionRewrite (renameFunctions)
import TypeVarRewrite (rewriteTypeVars)
import AtomicTypeRewrite (rewriteAtomicTypes)
import System.Environment (getArgs)
import Task (Task (..))
import Data.Either (rights)
import Control.Monad ((>=>))

alphaRewrite :: Task -> Either String Task
alphaRewrite = renameFunctions >=> rewriteTypeVars >=> rewriteAtomicTypes

rewriteTypes :: Task -> Either String Task
rewriteTypes = rewriteTypeVars >=> rewriteAtomicTypes

main :: IO ()
main = do
  args <- getArgs
  (func, input) <- case args of
    [opt, file] -> do
      input <- BL.readFile file
      return (selectFunction opt, input)
    _ -> error "Usage: alpharewrite-exe [1..5] [Benchmark-F.json]"

  -- We expect an array of tasks in the input JSON
  case decode input :: Maybe [Task] of
    Nothing -> error "Could not parse JSON."
    Just tasks -> do
      let renamed = rights (map func tasks)
      BL.putStr (encode renamed)

selectFunction :: String -> (Task -> Either String Task)
selectFunction "1" = alphaRewrite
selectFunction "2" = rewriteTypes
selectFunction "3" = rewriteAtomicTypes
selectFunction "4" = rewriteTypeVars
selectFunction "5" = renameFunctions
selectFunction _   = error "Invalid option. Use [1..5]."
