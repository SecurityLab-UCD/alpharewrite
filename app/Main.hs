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

rewriteTypes :: Task -> Either String Task
rewriteTypes = rewriteTypeVars >> rewriteAtomicTypes

alphaRewrite :: Task -> Either String Task
alphaRewrite = renameFunctions >> rewriteTypes

main :: IO ()
main = do
  args <- getArgs
  (func, input) <- case args of
    [] -> error "Usage: alpharewrite [0|1|2] [tasks.json]"
    [opt] -> do
      input <- BL.getContents
      return (selectFunction opt, input)
    [opt, file] -> do
      input <- BL.readFile file
      return (selectFunction opt, input)
    _ -> error "Usage: alpharewrite [0|1|2] [tasks.json]"

  -- We expect an array of tasks in the input JSON
  case decode input :: Maybe [Task] of
    Nothing -> error "Could not parse JSON."
    Just tasks -> do
      let renamed = rights (map func tasks)
      BL.putStr (encode renamed)

selectFunction :: String -> (Task -> Either String Task)
selectFunction "0" = alphaRewrite
selectFunction "1" = rewriteTypes
selectFunction "2" = renameFunctions
selectFunction _   = error "Invalid option. Use 0, 1, or 2."
