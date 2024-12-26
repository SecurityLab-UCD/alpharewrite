{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import FunctionRewrite (renameFunctions)
import TypeVarRewrite (rewriteTypeVars)
import System.Environment (getArgs)
import Task (Task (..))
import Data.Either (rights)

alphaRewrite :: Task -> Either String Task
alphaRewrite task = renameFunctions task >>= rewriteTypeVars

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> BL.getContents
    [file] -> BL.readFile file
    _ -> error "Usage: alpharewrite [tasks.json]"

  -- We expect an array of tasks in the input JSON
  case decode input :: Maybe [Task] of
    Nothing -> error "Could not parse JSON."
    Just tasks -> do
      let renamed = rights (map alphaRewrite tasks)
      BL.putStr (encode renamed)
